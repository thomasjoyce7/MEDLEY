# Script to serialize tabular POD medication data to a text format using a list template

import pandas as pd
import yaml
import os
import json
from datetime import datetime
from dateutil.relativedelta import relativedelta

# Set working directory
os.chdir(".")

# Load config
with open("config/paths.yml", "r") as f:
    config = yaml.safe_load(f)

# Access nested paths
paths = config["paths"]

# Build path to pod_medications_clean.csv
pod_med_path = os.path.join(paths["derived_data"], "pod_medications_cleaned.csv")

# Build path to pod_positive_identifiers.csv
pod_pos_path = os.path.join(paths["derived_data"], "pod_positive_identifiers.csv")

# Build path to demographics_cleaned.csv
demographics_path = os.path.join(paths["derived_data"], "demographics_cleaned.csv")
 
# Read in medications data with date parsing
pod_medications = pd.read_csv(pod_med_path, parse_dates=["starttime", "stoptime"])

# Read in POD positive identifiers with date parsing
pod_positive = pd.read_csv(pod_pos_path, parse_dates=["first_surgery_date", "icu_intime", "first_cam_positive_time"])

# Read in demographics data for POD positive patients
demographics = pd.read_csv(demographics_path)

print("Converting tabular medication data to text format using list template.")

# Join pod_medications, pod_positive, and demographics tables --------------------------------------

# Merge using a left join to retain all pod_positive subjects
tabular_input = pd.merge(
    pod_positive,
    pod_medications,
    on=["subject_id", "hadm_id", "stay_id"],
    how="left"  # keep all rows from pod_positive
)

# Then merge the result with demographics
tabular_input = pd.merge(
    tabular_input,
    demographics,
    on=["subject_id", "hadm_id", "stay_id"],
    how="left" 
)

# Shift dates back in time based on anchor_year and anchor_year_group -----------------------------

# Ensure all datetime columns are parsed correctly
date_columns = ["icu_intime","icu_outtime","hosp_admittime","hosp_dischtime","starttime", "stoptime", "first_surgery_date", "first_cam_positive_time"]
for col in date_columns:
    tabular_input[col] = pd.to_datetime(tabular_input[col], errors='coerce')

# Function to extract year difference and shift dates
def shift_dates(row):
    try:
        # Parse first year in anchor_year_group
        anchor_group_start_year = int(row['anchor_year_group'].split(" - ")[0])
        shift_years = int(row['anchor_year']) - anchor_group_start_year
        
        # Shift each date column by that many years
        for col in date_columns:
            if pd.notnull(row[col]):
                row[col] = row[col] - relativedelta(years=shift_years)
    except Exception as e:
        print(f"Error processing row {row.name}: {e}")
    return row

# Apply row-wise
tabular_input = tabular_input.apply(shift_dates, axis=1)

# Create list template function for tabular to text serialization -----------------------------------

def list_template(tabular_input):
    text_outputs = []
    
    for subject_id, group in tabular_input.groupby("subject_id"):
        group = group.sort_values("starttime")
        
        # --- Extract key information ---
        first_row = group.iloc[0]
        age = first_row.get("age")
        gender = first_row.get("gender").lower()
        
        if gender.lower() == "female":
            possessive_pronoun = "her"
            subject_pronoun = "she"
        elif gender.lower() == "male":
            possessive_pronoun = "his"
            subject_pronoun = "he"
        else:
            possessive_pronoun = "their"
            subject_pronoun = "they"
            
        # Parse and format first_surgery_date
        first_surgery_date = first_row.get("first_surgery_date", pd.NaT)
        first_surgery_date = pd.to_datetime(first_surgery_date, errors="coerce")

        # Format to natural language
        if pd.notnull(first_surgery_date):
             surgery_date_str = first_surgery_date.strftime("%B %-d, %Y")
        else:
             surgery_date_str = "an unknown date"

        icu_admit = pd.to_datetime(first_row.get("icu_intime", pd.NaT))
        icu_admit_str = icu_admit.strftime("%-I:%M %p on %B %-d, %Y") if pd.notnull(icu_admit) else "an unknown time"
        
        first_positive = pd.to_datetime(first_row.get("first_cam_positive_time", pd.NaT))
        first_positive_str = first_positive.strftime("%-I:%M %p on %B %-d, %Y") if pd.notnull(first_positive) else "an unknown time"
     
        # --- Build summary header ---        
        summary = (
    f"Summary: The patient is a {age}-year-old {gender}. "
    f"{possessive_pronoun.capitalize()} first surgery was on {surgery_date_str}. "
    f"{subject_pronoun.capitalize()} was admitted to the ICU at {icu_admit_str}. "
    f"{possessive_pronoun.capitalize()} first positive CAM-ICU result occurred at {first_positive_str}.\n")

        # --- Build medication timeline ---
        if group["medication"].isna().all() and group["starttime"].isna().all():
            # No medication data — only return summary
            full_text = summary
        else:
            # Medication data exists — build entries
            med_entries = []
            for _, row in group.iterrows():
                start = pd.to_datetime(row["starttime"])
                stop = pd.to_datetime(row["stoptime"])
                start_str = start.strftime("%B %-d, %Y, %-I:%M %p") if pd.notnull(start) else "an unknown time"
                
                med = row["medication"]
                dose = row["amount"]
                units = row["units"]
                route = row["route"]

                if pd.notnull(stop) and pd.notnull(start) and start != stop:
                    if start.date() == stop.date():
                        stop_str = stop.strftime("%-I:%M %p")
                    else:
                        stop_str = stop.strftime("%B %-d, %Y, %-I:%M %p")
                    entry = (
                        f"{start_str} – {stop_str}:\n"
                        f"  - Medication: {med}\n"
                        f"  - Dosage: {dose}\n"
                        f"  - Units: {units}\n"
                        f"  - Route: {route}"
                    )
                else:
                    entry = (
                        f"{start_str}:\n"
                        f"  - Medication: {med}\n"
                        f"  - Dosage: {dose}\n"
                        f"  - Units: {units}\n"
                        f"  - Route: {route}"
                    )
                med_entries.append(entry)
            
            full_text = summary + "\n" + "\n".join(med_entries)
        
        text_outputs.append((subject_id, full_text))
    
    return text_outputs

# Save text output for each patient ---------------------------------------------------------------------------------
medication_texts = list_template(tabular_input)

# Save as .jsonl
with open("data/llm_inputs/medications_list_template.jsonl", "w") as f:
    for subject_id, text in medication_texts:
        json.dump({"subject_id": subject_id, "text": text}, f)
        f.write("\n")
        
print("Medication list templates for POD positive patients successfully saved.")