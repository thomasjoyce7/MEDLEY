# Script to serialize tabular PONV medication data to a text format using a list template
# Create separate templates for each of the 4 perioperative windows:
# Day before surgery, day of surgery, 1-2 days after surgery, and 2-4 days after surgery

import pandas as pd
import numpy as np 
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

print("Converting tabular medication data to text format using list template for each perioperative window.")

# Join ponv_medications and ponv_study_cohort_identifiers --------------------------------------

# Merge using a left join to retain all subjects in ponv_study_cohort_identifiers
tabular_input = pd.merge(
    ponv_study_cohort_identifiers,
    ponv_medications,
    on=["subject_id", "hadm_id", "procedure_date"],
    how="left"  # keep all rows from ponv_study_cohort_identifiers 
)

# Create a new column in tabular input for perioperative window
# admin_time occurs 0 to 1 days before procedure_date => window = 1
# admin_time occurs 0 to 1 days after procedure_date => window = 2
# admin_time occurs 1 to 2 days after procedure_date => window = 3
# admin_time occurs 2 to 4 days after procedure_date => window = 4

tabular_input["delta_days"] = (
    (tabular_input["admin_time"] - tabular_input["procedure_date"])
    .dt.total_seconds() / 86400.0
)

conditions = [
    tabular_input["delta_days"].between(-1, 0, inclusive="both"), # [-1, 0]
    tabular_input["delta_days"].between(0, 1, inclusive="right"), # (0, 1]
    tabular_input["delta_days"].between(1, 2, inclusive="right"), # (1, 2]
    tabular_input["delta_days"].between(2, 4, inclusive="right"), # (2, 4]
]

choices = [1, 2, 3, 4]

tabular_input["periop_window"] = np.select(conditions, choices, default=np.nan)

tabular_input = tabular_input.drop(columns=["delta_days"])

# Helper function to format times relative to surgery date 
def format_relative_to_surgery_date(delta: pd.Timedelta) -> str:
    if pd.isna(delta):
        return "NA"

    total_seconds = int(delta.total_seconds())
    sign = "before" if total_seconds < 0 else "after"
    total_seconds = abs(total_seconds)

    days, rem = divmod(total_seconds, 86400)
    hours, rem = divmod(rem, 3600)
    minutes, _seconds = divmod(rem, 60)

    if days > 0:
        return f"{days}d {hours}h {minutes}m {sign} surgery date"
    else:
        return f"{hours}h {minutes}m {sign} surgery date"

# Create list template function for tabular to text serialization (this function will be applied to each of the 4 perioperative windows) -----------------------------------

def list_template(tabular_input: pd.DataFrame, window: int):
    # Define window header
    if window == 1:
        summary = "Day before surgery:"
    elif window == 2:
        summary = "Day of surgery:"
    elif window == 3:
        summary = "1 to 2 days after surgery:"
    elif window == 4:
        summary = "2 to 4 days after surgery:"
    else:
        summary = f"Window {window}:"

    text_outputs = []

    # Iterate over all subjects (not just those with rows in this window)
    for subject_id, subj_df in tabular_input.groupby("subject_id", dropna=False):

        # rows for this subject in this window
        group = subj_df.loc[subj_df["periop_window"] == window].copy()
        group = group.sort_values("admin_time")

        # If no meds in this window (or missing times/med names), return explicit no-med text
        if group.empty or group["medication"].isna().all() or group["admin_time"].isna().all():
            full_text = f"{summary} no medications"
            text_outputs.append((subject_id, full_text))
            continue

        # Otherwise, build medication entries
        med_entries = []
        for _, row in group.iterrows():
            admin_time = row["admin_time"]
            proc_date = row["procedure_date"]

            if pd.isna(admin_time) or pd.isna(proc_date) or pd.isna(row.get("medication")):
                continue

            surgery_midnight = proc_date.floor("D")
            rel_time_str = format_relative_to_surgery_date(admin_time - surgery_midnight)

            med = row.get("medication", "")
            dose = row.get("amount", "")
            units = row.get("units", "")
            route = row.get("route", "")

            entry = (
                f"{rel_time_str}:\n"
                f"Medication: {med}\n"
                f"Dose: {dose}\n"
                f"Units: {units}\n"
                f"Route: {route}"
            )
            med_entries.append(entry)

        if len(med_entries) == 0:
            full_text = f"{summary} no medications"
        else:
            full_text = summary + "\n" + "\n".join(med_entries)

        text_outputs.append((subject_id, full_text))

    return text_outputs

# Save text output for each subject and perioperative window as a single jsonl file ---------------------------------------------------------------------------------
# Generate window texts
medication_texts_window1 = dict(list_template(tabular_input, window=1))
medication_texts_window2 = dict(list_template(tabular_input, window=2))
medication_texts_window3 = dict(list_template(tabular_input, window=3))
medication_texts_window4 = dict(list_template(tabular_input, window=4))

# Get full set of subjects
all_subjects = set(
    medication_texts_window1.keys()
).union(
    medication_texts_window2.keys(),
    medication_texts_window3.keys(),
    medication_texts_window4.keys()
)

output_path = "data/llm_inputs/02_PONV/ponv_medications_list_template_4windows.jsonl"

with open(output_path, "w") as f:
    for subject_id in sorted(all_subjects):
        record = {
            "subject_id": subject_id,
            "window_1": medication_texts_window1.get(subject_id, ""),
            "window_2": medication_texts_window2.get(subject_id, ""),
            "window_3": medication_texts_window3.get(subject_id, ""),
            "window_4": medication_texts_window4.get(subject_id, "")
        }
        json.dump(record, f)
        f.write("\n")

print("Medication list templates (4 windows) for the PONV study cohort successfully saved.")