# Script to calculate the embeddings for all templates using Longformer-base 
# Apply global mean pooling to obtain one final embedding per patient

# Model size ~149M parameters 
# Maximum input = 4096 tokens
# Embedding dimension = 768

import os
import yaml
import json
import torch
import pandas as pd
from transformers import AutoTokenizer, AutoModel
import random
import numpy as np

# Set working directory as project root directory 
os.chdir(".")

# Set random seeds and ensure deterministic GPU operations for reproducibility
SEED = 42
random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)
torch.cuda.manual_seed(SEED)
torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False

# Load config
with open("config/paths.yml", "r") as f:
    config = yaml.safe_load(f)

# Access nested paths
paths = config["paths"]

# Build path to medications_list_template.jsonl
list_template_path = os.path.join(paths["llm_inputs"], "medications_list_template.jsonl")

# Build path to medications_text_template.jsonl
text_template_path = os.path.join(paths["llm_inputs"], "medications_text_template.jsonl")

# Build path to LLM template
llm_template_path = os.path.join(paths["llm_inputs"], "medications_llama3.3_template.jsonl")

# Load model + tokenizer
tokenizer = AutoTokenizer.from_pretrained("allenai/longformer-base-4096", torch_dtype="auto")
model = AutoModel.from_pretrained("allenai/longformer-base-4096", torch_dtype="auto")
model.eval()

# Move model to GPU if available
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
model.to(device)

def embeddings(input_path):
    
    # Prepare storage
    all_embeddings = []
    subject_ids = []

    # Read all lines from JSONL and process
    with open(input_path, "r") as f:
        for line in f:
            record = json.loads(line)
            subject_id = record["subject_id"]
            text = record["text"]

            # Tokenize and encode
            encoding = tokenizer(
                text,
                add_special_tokens=True,
                truncation=True,
                #padding="max_length",
                max_length=4096, # All sequences will be padded or truncated to the maximum input
                return_tensors="pt"
            )
            encoding = {k: v.to(device) for k, v in encoding.items()}

            # Pad to multiple of attention window
            attention_window = model.config.attention_window[0]
            seq_len = encoding["input_ids"].shape[1]
            pad_len = (attention_window - seq_len % attention_window) % attention_window

            if pad_len > 0:
                pad_token_id = tokenizer.pad_token_id or tokenizer.eos_token_id
                padding = torch.full((1, pad_len), pad_token_id, dtype=torch.long).to(device)
                attention_padding = torch.zeros((1, pad_len), dtype=torch.long).to(device)
                encoding["input_ids"] = torch.cat([encoding["input_ids"], padding], dim=1)
                encoding["attention_mask"] = torch.cat([encoding["attention_mask"], attention_padding], dim=1)

            # Forward pass
            with torch.no_grad():
                outputs = model(input_ids=encoding["input_ids"], attention_mask=encoding["attention_mask"])
                hidden_states = outputs.last_hidden_state

                # Mean pooling (padding tokens will be masked out during mean pooling)
                mask = encoding["attention_mask"].unsqueeze(-1)
                masked_hidden = hidden_states * mask
                sum_hidden = masked_hidden.sum(dim=1)
                count_tokens = mask.sum(dim=1)
                mean_embedding = sum_hidden / count_tokens

            # Store
            all_embeddings.append(mean_embedding.squeeze(0).cpu().numpy())
            subject_ids.append(subject_id)
        
    return all_embeddings, subject_ids

# Calculate embeddings for each serialization template format and save as csv files
print("Calculating Longformer-base embeddings.")

#input_paths = [list_template_path, text_template_path, llm_template_path]
input_paths = [llm_template_path]

for input_path in input_paths:
    
    if input_path == list_template_path:
        template_name = "list_template_embeddings"
    elif input_path == text_template_path:
        template_name = "text_template_embeddings"
    else:
        template_name = "llm_template_embeddings3"
        
    print(f"Processing {template_name}")
    
    all_embeddings, subject_ids = embeddings(input_path)

    # Convert to DataFrame
    df = pd.DataFrame(all_embeddings)
    df.insert(0, "subject_id", subject_ids)
    
    # Save as a csv file
    output_path = os.path.join(paths["embeddings"], f"longformer_base_{template_name}.csv")
    df.to_csv(output_path, index=False)
    
    print(f"Embeddings for {template_name} saved to {output_path}")
