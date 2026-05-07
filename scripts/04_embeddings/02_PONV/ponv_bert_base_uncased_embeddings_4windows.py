# BERT-base-uncased PONV application 
# Script to calculate mean pooled embeddings for each perioperative window, 
# L2 normalize per window, concatenate windows, then L2 normalize again 
# Save intermediate window embeddings and final concatenated embeddings 

# Model size = 110M parameters 
# Maximum input = 512 tokens
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
tokenizer = AutoTokenizer.from_pretrained("bert-base-uncased")
model = AutoModel.from_pretrained("bert-base-uncased")
model.eval()

# Ensure pad token exists
if tokenizer.pad_token_id is None:
    if tokenizer.sep_token is not None:
        tokenizer.pad_token = tokenizer.sep_token
    elif tokenizer.cls_token is not None:
        tokenizer.pad_token = tokenizer.cls_token
    else:
        tokenizer.add_special_tokens({"pad_token": "[PAD]"})
        model.resize_token_embeddings(len(tokenizer))

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.to(device)

# Set maximum token input length 
MAX_LEN = 512

# Helper function to normalize embeddings 
def l2_normalize(vec: np.ndarray, eps: float = 1e-12) -> np.ndarray:
    norm = np.linalg.norm(vec)
    if norm < eps:
        return vec
    return vec / norm


def l2_normalize_rows(mat: np.ndarray, eps: float = 1e-12) -> np.ndarray:
    norms = np.linalg.norm(mat, axis=1, keepdims=True)
    return mat / np.clip(norms, eps, None)


def embed_text_meanpool(text: str) -> np.ndarray:
    """
    Tokenize -> forward -> attention-masked mean pool.
    """
    encoding = tokenizer(
        text,
        add_special_tokens=True,
        truncation=True,
        max_length=MAX_LEN,
        return_tensors="pt",
        padding=False,  # single-example inference
    )
    encoding = {k: v.to(device) for k, v in encoding.items()}

    with torch.inference_mode():
        outputs = model(**encoding)
        hidden_states = outputs.last_hidden_state  # (1, seq, hidden)

        mask = encoding["attention_mask"].unsqueeze(-1).to(hidden_states.dtype)  # (1, seq, 1)
        sum_hidden = (hidden_states * mask).sum(dim=1)  # (1, hidden)
        count_tokens = mask.sum(dim=1)
        count_tokens = torch.clamp(count_tokens, min=1.0)

        mean_embedding = (sum_hidden / count_tokens).squeeze(0)  # (hidden,)

    return mean_embedding.float().cpu().numpy()


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

            # Fallback defaults if any window key is missing
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

    if normalize_concat:
        concat = l2_normalize_rows(concat)

    window_embs = {"window_1": W1, "window_2": W2, "window_3": W3, "window_4": W4}
    return subject_ids, window_embs, concat


def save_embeddings_parquet(subject_ids, emb_matrix: np.ndarray, output_path: str):
    emb_matrix = emb_matrix.astype(np.float32)
    df = pd.DataFrame(emb_matrix)
    df.insert(0, "subject_id", subject_ids)
    df.to_parquet(output_path, index=False, engine="pyarrow", compression="snappy")


# 
print("Calculating BERT-base-uncased embeddings (4 windows, normalized, concatenated).")

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

    out_concat = os.path.join(
        out_dir, f"ponv_bert_base_uncased_{template_name}_concat4w_meanpool_l2norm.parquet"
    )
    save_embeddings_parquet(subject_ids, concat_embs, out_concat)
    print(f"Saved concatenated embeddings to {out_concat}")

    for wname, mat in window_embs.items():
        out_w = os.path.join(
            out_dir, f"ponv_bert_base_uncased_{template_name}_{wname}_meanpool_l2norm.parquet"
        )
        save_embeddings_parquet(subject_ids, mat, out_w)

    print(f"Saved per-window embeddings to {out_dir}")