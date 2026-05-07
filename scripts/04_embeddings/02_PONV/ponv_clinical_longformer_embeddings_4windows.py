# Clinical-Longformer PONV application 
# Script to calculate mean pooled embeddings for each perioperative window, 
# L2 normalize per window, concatenate windows, then L2 normalize again 
# Save intermediate window embeddings and final concatenated embeddings 

# Model size = 149M parameters 
# Maximum input = 4096 tokens
# Embedding dimension = 768
# Concatenated embedding dimension = 768*4 = 3072

import os
import yaml
import json
import torch
import pyarrow 
import pandas as pd
from transformers import AutoTokenizer, AutoModel
import random
import numpy as np

# Set working directory as project root directory 
os.chdir(".")

# Reproducibility
SEED = 42
random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)
torch.cuda.manual_seed(SEED)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False

# Paths
list_template_path = "data/llm_inputs/02_PONV/ponv_medications_list_template_4windows.jsonl"
text_template_path = "data/llm_inputs/02_PONV/ponv_medications_text_template_4windows.jsonl"

out_dir = "data/embeddings/02_PONV/"

# Load the model + tokenizer
tokenizer = AutoTokenizer.from_pretrained("yikuan8/Clinical-Longformer")
model = AutoModel.from_pretrained("yikuan8/Clinical-Longformer")
model.eval()

# Ensure pad token exists (important for Longformer padding)
if tokenizer.pad_token_id is None:
    # Clinical-Longformer often has no pad token; use eos as pad
    tokenizer.pad_token = tokenizer.eos_token

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.to(device)

ATTN_WIN = model.config.attention_window[0]  # Longformer attention window (usually 512)
MAX_LEN = 4096


# Helper function to normalize embeddings 
def l2_normalize(vec: np.ndarray, eps: float = 1e-12) -> np.ndarray:
    norm = np.linalg.norm(vec)
    if norm < eps:
        return vec
    return vec / norm


def embed_text_meanpool(text: str) -> np.ndarray:
    """Tokenize -> pad to attention window multiple -> forward -> attention-masked mean pool."""
    encoding = tokenizer(
        text,
        add_special_tokens=True,
        truncation=True,
        max_length=MAX_LEN,
        return_tensors="pt",
    )
    encoding = {k: v.to(device) for k, v in encoding.items()}

    # Pad seq_len to a multiple of attention window
    seq_len = encoding["input_ids"].shape[1]
    pad_len = (ATTN_WIN - (seq_len % ATTN_WIN)) % ATTN_WIN

    if pad_len > 0:
        pad_token_id = tokenizer.pad_token_id
        padding = torch.full((1, pad_len), pad_token_id, dtype=torch.long, device=device)
        attention_padding = torch.zeros((1, pad_len), dtype=torch.long, device=device)
        encoding["input_ids"] = torch.cat([encoding["input_ids"], padding], dim=1)
        encoding["attention_mask"] = torch.cat([encoding["attention_mask"], attention_padding], dim=1)

    with torch.no_grad():
        outputs = model(input_ids=encoding["input_ids"], attention_mask=encoding["attention_mask"])
        hidden_states = outputs.last_hidden_state  # (1, seq, hidden)

        # attention-masked mean pooling
        mask = encoding["attention_mask"].unsqueeze(-1)  # (1, seq, 1)
        masked_hidden = hidden_states * mask
        sum_hidden = masked_hidden.sum(dim=1)  # (1, hidden)
        count_tokens = mask.sum(dim=1)         # (1, 1)

        # avoid division by zero (shouldn't happen, but safe)
        count_tokens = torch.clamp(count_tokens, min=1.0)
        mean_embedding = (sum_hidden / count_tokens).squeeze(0)  # (hidden,)

    return mean_embedding.cpu().numpy()


def process_embeddings_4windows(input_path: str, normalize_concat: bool = True):
    """
    Expects JSONL where each line has:
      subject_id, window_1, window_2, window_3, window_4
    Returns:
      subject_ids
      window_embs: dict window -> np.ndarray (n_subjects, hidden)
      concat_embs: np.ndarray (n_subjects, 4*hidden)
    """
    subject_ids = []
    w1_list, w2_list, w3_list, w4_list = [], [], [], []

    with open(input_path, "r", encoding="utf-8") as f:
        for line in f:
            record = json.loads(line)
            subject_id = record["subject_id"]

            # If any are missing, fall back to a safe default so you still get an embedding
            t1 = record.get("window_1", "Day before surgery: no medications")
            t2 = record.get("window_2", "Day of surgery: no medications")
            t3 = record.get("window_3", "1 to 2 days after surgery: no medications")
            t4 = record.get("window_4", "2 to 4 days after surgery: no medications")

            e1 = l2_normalize(embed_text_meanpool(t1))
            e2 = l2_normalize(embed_text_meanpool(t2))
            e3 = l2_normalize(embed_text_meanpool(t3))
            e4 = l2_normalize(embed_text_meanpool(t4))

            subject_ids.append(subject_id)
            w1_list.append(e1)
            w2_list.append(e2)
            w3_list.append(e3)
            w4_list.append(e4)

    W1 = np.vstack(w1_list)
    W2 = np.vstack(w2_list)
    W3 = np.vstack(w3_list)
    W4 = np.vstack(w4_list)

    concat = np.hstack([W1, W2, W3, W4])

    # Optional (recommended): normalize the concatenated vector too
    if normalize_concat:
        concat = np.vstack([l2_normalize(v) for v in concat])

    window_embs = {"window_1": W1, "window_2": W2, "window_3": W3, "window_4": W4}
    return subject_ids, window_embs, concat


def save_embeddings_parquet(subject_ids, emb_matrix: np.ndarray, output_path: str):
    emb_matrix = emb_matrix.astype(np.float32)  # reduce file size
    
    df = pd.DataFrame(emb_matrix)
    df.insert(0, "subject_id", subject_ids)

    df.to_parquet(
        output_path,
        index=False,
        engine="pyarrow",
        compression="snappy"
    )


print("Calculating Clinical-Longformer embeddings (4 windows, normalized, concatenated).")

input_paths = {
    list_template_path: "list_template",
    text_template_path: "text_template",
}

for input_path, template_name in input_paths.items():
    print(f"Processing {template_name}: {input_path}")

    subject_ids, window_embs, concat_embs = process_embeddings_4windows(
        input_path,
        normalize_concat=True, 
    )

    # Save final concatenated embeddings
    out_concat = os.path.join(
        out_dir, f"ponv_clinical_longformer_{template_name}_concat4w_meanpool_l2norm.parquet"
    )
    save_embeddings_parquet(subject_ids, concat_embs, out_concat)
    print(f"Saved concatenated embeddings to {out_concat}")

    # Save each window separately (useful for debugging/ablations)
    for wname, mat in window_embs.items():
        out_w = os.path.join(
            out_dir, f"ponv_clinical_longformer_{template_name}_{wname}_meanpool_l2norm.parquet"
        )
        save_embeddings_parquet(subject_ids, mat, out_w)

    print(f"Saved per-window embeddings to {out_dir}")