# Script to calculate embeddings for all templates using Qwen3-Embedding-8B
# Huggingface model card: https://huggingface.co/Qwen/Qwen3-Embedding-8B
# Pooling strategy: Left padding with last token pool 
# Model size = 8B parameters
# Embedding dimension = 4096
# Max context length can be 32768 tokens, but practical max_length depends on GPU memory (start with 8192) 

import os
import yaml
import json
import torch
import torch.nn.functional as F
import pandas as pd
from transformers import AutoTokenizer, AutoModel
import random
import numpy as np
from torch import Tensor

# Set location to store model in project directory 
os.environ["HF_HUB_CACHE"] = "hf_cache"

# -----------------------------------------------------------------------------
# Set working directory as project root directory
# -----------------------------------------------------------------------------
os.chdir(".")

# -----------------------------------------------------------------------------
# Set random seeds and ensure deterministic GPU operations for reproducibility
# -----------------------------------------------------------------------------
SEED = 42
random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)
if torch.cuda.is_available():
    torch.cuda.manual_seed(SEED)
    torch.cuda.manual_seed_all(SEED)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False

# -----------------------------------------------------------------------------
# Load config
# -----------------------------------------------------------------------------
with open("config/paths.yml", "r") as f:
    config = yaml.safe_load(f)

paths = config["paths"]

# -----------------------------------------------------------------------------
# Input paths
# -----------------------------------------------------------------------------
list_template_path = os.path.join(paths["llm_inputs"], "medications_list_template.jsonl")
text_template_path = os.path.join(paths["llm_inputs"], "medications_text_template.jsonl")
llm_template_path = os.path.join(paths["llm_inputs"], "medications_llama3.3_template.jsonl")

# -----------------------------------------------------------------------------
# Pooling helper for Qwen3-Embedding-8B
# -----------------------------------------------------------------------------
def last_token_pool(last_hidden_states: Tensor, attention_mask: Tensor) -> Tensor:
    """
    Pool the final non-padding token representation.
    Works correctly for left-padded or right-padded batches.
    """
    left_padding = (attention_mask[:, -1].sum() == attention_mask.shape[0])

    if left_padding:
        return last_hidden_states[:, -1]
    else:
        sequence_lengths = attention_mask.sum(dim=1) - 1
        batch_size = last_hidden_states.shape[0]
        return last_hidden_states[
            torch.arange(batch_size, device=last_hidden_states.device),
            sequence_lengths
        ]

# -----------------------------------------------------------------------------
# Device
# -----------------------------------------------------------------------------
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print(f"Using device: {device}")

# -----------------------------------------------------------------------------
# Load tokenizer + model
# -----------------------------------------------------------------------------
model_name = "Qwen/Qwen3-Embedding-8B"

cache_dir = "hf_cache"

tokenizer = AutoTokenizer.from_pretrained(
    model_name,
    padding_side="left",
    cache_dir=cache_dir
)

# Ensure pad token exists
if tokenizer.pad_token_id is None:
    tokenizer.pad_token = tokenizer.eos_token

# Recommended: flash_attention_2 if available on your system
# If this causes issues, remove attn_implementation and/or torch_dtype
model = AutoModel.from_pretrained(
    model_name,
    torch_dtype=torch.float16,
    cache_dir=cache_dir
).to(device)

model.eval()

# Confirm model loaded correctly
print("Model loaded successfully")
print(f"Device: {next(model.parameters()).device}")
print(f"Dtype: {next(model.parameters()).dtype}")

# -----------------------------------------------------------------------------
# Parameters
# -----------------------------------------------------------------------------
MAX_LENGTH = 8192
BATCH_SIZE = 2   # Reduce to 1 if you hit OOM

# -----------------------------------------------------------------------------
# Read JSONL helper
# -----------------------------------------------------------------------------
def load_jsonl_records(input_path):
    records = []
    with open(input_path, "r") as f:
        for line in f:
            record = json.loads(line)
            records.append({
                "subject_id": record["subject_id"],
                "text": record["text"]
            })
    return records

# -----------------------------------------------------------------------------
# Embedding function
# -----------------------------------------------------------------------------
def compute_embeddings(input_path, batch_size=BATCH_SIZE, max_length=MAX_LENGTH):
    """
    Compute Qwen3 embeddings for all records in a JSONL file.

    Returns
    -------
    all_embeddings : np.ndarray of shape (n_subjects, embedding_dim)
    subject_ids : list
    """
    records = load_jsonl_records(input_path)

    all_embeddings = []
    subject_ids = []

    for start_idx in range(0, len(records), batch_size):
        batch_records = records[start_idx:start_idx + batch_size]
        batch_texts = [r["text"] for r in batch_records]
        batch_subject_ids = [r["subject_id"] for r in batch_records]

        # Tokenize
        batch_dict = tokenizer(
            batch_texts,
            padding=True,
            truncation=True,
            max_length=max_length,
            return_tensors="pt"
        )

        batch_dict = {k: v.to(device) for k, v in batch_dict.items()}

        # Forward pass
        with torch.inference_mode():
            outputs = model(**batch_dict)

            # Last-token pooling
            embeddings = last_token_pool(
                outputs.last_hidden_state,
                batch_dict["attention_mask"]
            )

            # L2 normalize
            embeddings = F.normalize(embeddings, p=2, dim=1)

        all_embeddings.append(embeddings.cpu())
        subject_ids.extend(batch_subject_ids)

        if start_idx % (100 * batch_size) == 0:
            print(f"Processed {min(start_idx + batch_size, len(records))} / {len(records)} records")

    all_embeddings = torch.cat(all_embeddings, dim=0).numpy()

    return all_embeddings, subject_ids

# -----------------------------------------------------------------------------
# Process each template file and save embeddings
# -----------------------------------------------------------------------------
print("Processing Qwen3-Embedding-8B embeddings.")

input_paths = [list_template_path, text_template_path, llm_template_path]

for input_path in input_paths:
    if input_path == list_template_path:
        template_name = "list_template"
    elif input_path == text_template_path:
        template_name = "text_template"
    else:
        template_name = "llm_template"

    print(f"\nProcessing {template_name}")

    all_embeddings, subject_ids = compute_embeddings(
        input_path=input_path,
        batch_size=BATCH_SIZE,
        max_length=MAX_LENGTH
    )

    # Convert to DataFrame
    df = pd.DataFrame(all_embeddings)
    df.insert(0, "subject_id", subject_ids)

    # Save as CSV
    output_path = os.path.join(
        paths["embeddings"],
        f"Qwen3_embeddings_8B_{template_name}.csv"
    )
    df.to_csv(output_path, index=False)

    print(f"Embeddings for {template_name} saved to {output_path}")
    print(f"Output shape: {df.shape}")
