# Script to serialize tabular PONV medication data to a text format using a list template

import pandas as pd
import yaml
import os
import json
from datetime import datetime
from dateutil.relativedelta import relativedelta

# Set working directory
os.chdir(".")
 
# Read in medications data with date parsing
ponv_medications = pd.read_csv("data/derived/02_PONV/ponv_medications_cleaned.csv", parse_dates=["admin_time", "procedure_date"])

# Read study cohort identifiers with date parsing
ponv_study_cohort_identifiers = pd.read_csv("data/cohorts/02_PONV/ponv_study_cohort_identifiers.csv", parse_dates=["procedure_date", "hosp_admittime", "hosp_dischtime"])

# Read in demographics data for the PONV study cohort 
demographics = pd.read_csv("data/derived/02_PONV/ponv_demographics_cleaned.csv")

print("Converting tabular medication data to text format using list template.")

# Join ponv_medications, ponv_study_cohort_identifiers, and demographics tables --------------------------------------

# Merge using a left join to retain all subjects in ponv_study_cohort_identifiers
tabular_input = pd.merge(
    ponv_study_cohort_identifiers,
    ponv_medications,
    on=["subject_id", "hadm_id", "procedure_date"],
    how="left"  # keep all rows from ponv_study_cohort_identifiers 
)

# Then merge the result with demographics
tabular_input = pd.merge(
    tabular_input,
    demographics,
    on=["subject_id", "hadm_id"],
    how="left" 
)

# Shift dates back in time based on anchor_year and anchor_year_group -----------------------------

# Ensure all datetime columns are parsed correctly
date_columns = ["hosp_admittime","hosp_dischtime","procedure_date","admin_time"]
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
        group = group.sort_values("admin_time")
        
        # --- Extract key information ---
        first_row = group.iloc[0]
        age = first_row.get("age")
        gender = first_row.get("gender").lower()
        
        if gender.lower() == "female":
            possessive_pronoun = "her"
            subject_pronoun = "she"
        else:
            possessive_pronoun = "his"
            subject_pronoun = "he"
            
        # Parse and format first_surgery_date
        surgery_date = first_row.get("procedure_date", pd.NaT)
        surgery_date = pd.to_datetime(surgery_date, errors="coerce")

        # Format to natural language
        if pd.notnull(surgery_date):
             surgery_date_str = surgery_date.strftime("%B %-d, %Y")
        else:
             surgery_date_str = "an unknown date"

        hosp_admit = pd.to_datetime(first_row.get("hosp_admittime", pd.NaT))
        hosp_admit_str = hosp_admit.strftime("%-I:%M %p on %B %-d, %Y") if pd.notnull(hosp_admit) else "an unknown time"
             
        # --- Build summary header ---        
        summary = (
    f"Summary: The patient is a {age}-year-old {gender}. "
    f"{subject_pronoun.capitalize()} was admitted to the hospital at {hosp_admit_str}. "
    f"{possessive_pronoun.capitalize()} surgery was on {surgery_date_str}.")
    
        # --- Build medication timeline ---
        if group["medication"].isna().all() and group["admin_time"].isna().all():
            # No medication data — only return summary
            full_text = summary
        else:
            # Medication data exists — build entries
            med_entries = []
            for _, row in group.iterrows():
                admin_time = pd.to_datetime(row["admin_time"])
                admin_time_str = admin_time.strftime("%B %-d, %Y, %-I:%M %p") if pd.notnull(admin_time) else "an unknown time"
                
                med = row["medication"]
                dose = row["amount"]
                units = row["units"]
                route = row["route"]

                entry = (
                        f"{admin_time_str}:\n"
                        f"Medication: {med}\n"
                        f"Dose: {dose}\n"
                        f"Units: {units}\n"
                        f"Route: {route}"
                    )

                med_entries.append(entry) 
            
            full_text = summary + "\n" + "\n".join(med_entries)
        
        text_outputs.append((subject_id, full_text))
    
    return text_outputs

# Save text output for each patient ---------------------------------------------------------------------------------
medication_texts = list_template(tabular_input)

# Save as .jsonl
with open("data/llm_inputs/02_PONV/ponv_medications_list_template.jsonl", "w") as f:
    for subject_id, text in medication_texts:
        json.dump({"subject_id": subject_id, "text": text}, f)
        f.write("\n")
        
print("Medication list templates for the PONV study cohort successfully saved.")


