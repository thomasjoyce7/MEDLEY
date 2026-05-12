# Create summary tables for tokenized sequence lengths for the POD application

import os
import json
import numpy as np
import pandas as pd
from tqdm import tqdm
from transformers import AutoTokenizer

# Set working directory
os.chdir(".")

# Load pod_study_cohort_identifiers 
final_cohort = pd.read_csv("data/cohorts/final_cohort_features.csv")
final_ids = set(final_cohort["subject_id"].unique())

# Load tokenizers for each model 
tokenizers = {
    "Clinical-Longformer": AutoTokenizer.from_pretrained("yikuan8/Clinical-Longformer"),
    "GatorTron-Base-2k": AutoTokenizer.from_pretrained("UFNLP/gatortron-base-2k"),
    "Longformer-Base-4096": AutoTokenizer.from_pretrained(
        "allenai/longformer-base-4096", torch_dtype="auto"
    ),
}

# Define templates
all_templates = {
    "List": "data/llm_inputs/medications_list_template.jsonl",
    "Text": "data/llm_inputs/medications_text_template.jsonl",
    "LLM": "data/llm_inputs/medications_llama3.3_template.jsonl",
}

# Helper function to load templates and filter to subject_ids in the final cohort
def load_texts_from_jsonl(path, final_ids):
    texts = []
    with open(path, "r") as f:
        for line in f:
            row = json.loads(line)
            if row["subject_id"] in final_ids:
                texts.append(row["text"])
    return texts

# Function to summarize tokenized sequence lengths
def summarize_token_lengths(
    texts,
    tokenizer,
    max_length,
    thresholds=(512, 2048, 4096)
):
    lengths = []

    for txt in tqdm(texts, leave=False):
        tokens = tokenizer(
            txt,
            truncation=False,
            add_special_tokens=True,
            return_attention_mask=False,
            return_token_type_ids=False
        )
        lengths.append(len(tokens["input_ids"]))

    lengths = np.array(lengths)

    return {
        "Mean": lengths.mean(),
        "Median": np.median(lengths),
        "Min": lengths.min(),
        "Max": lengths.max(),
        ">512 (%)": 100 * np.mean(lengths > thresholds[0]),
        ">2048 (%)": 100 * np.mean(lengths > thresholds[1]),
        ">4096 (%)": 100 * np.mean(lengths > thresholds[2]),
        "% Truncated": 100 * np.mean(lengths > max_length),
    }

tokenizer_max_lengths = {
    "Clinical-Longformer": 4096,
    "Longformer-Base-4096": 4096,
    "GatorTron-Base-2k": 2048,
}

rows = []

for tokenizer_name, tokenizer in tokenizers.items():
    max_length = tokenizer_max_lengths[tokenizer_name]

    for template_name, template_path in all_templates.items():

        texts = load_texts_from_jsonl(
            template_path,
            final_ids
        )

        summary = summarize_token_lengths(
            texts=texts,
            tokenizer=tokenizer,
            max_length=max_length
        )

        summary.update({
            "Tokenizer": tokenizer_name,
            "Template": template_name,
        })

        rows.append(summary)

# Create summary dataframe and apply rounding
df = pd.DataFrame(rows)

# Adjust cdolumn order 
df = df[
    ["Tokenizer", "Template", "Mean", "Median", "Min", "Max", ">512 (%)", ">4096 (%)"]
]

# Round to one decimal place 
df = pd.DataFrame(rows)

df = df[
    [
        "Tokenizer",
        "Template",
        "Mean",
        "Median",
        "Min",
        "Max",
        ">512 (%)",
        ">2048 (%)",
        ">4096 (%)",
        "% Truncated",
    ]
]

# Round all numeric columns to 1 decimal place
numeric_cols = df.columns.difference(["Tokenizer", "Template"])
df[numeric_cols] = df[numeric_cols].round(1)

# Create and save table 
latex_table = df.to_latex(
    index=False,
    escape=False,
    column_format="llrrrrrrrr",
    float_format="%.2f",
    caption=("Tokenized sequence length summary for the POD application"),
    label="tab:token_lengths",
)

with open("tables/01_POD/pod_tokenized_seq_lengths_summary.tex", "w") as f:
    f.write(latex_table)












            
