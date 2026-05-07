# Script to calculate the embeddings for all templates using gatortron-base-2k
# Apply global mean pooling to obtain one final embedding per patient 

# Model size = 345M parameters
# Maximum input = 2048 tokens
# Embedding dimension = 1024

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
tokenizer = AutoTokenizer.from_pretrained("UFNLP/gatortron-base-2k")
model = AutoModel.from_pretrained("UFNLP/gatortron-base-2k")
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
                max_length=2048, # All sequences will be padded or truncated to the maximum input
                return_tensors="pt"
            )
            encoding = {k: v.to(device) for k, v in encoding.items()}

            # Forward pass
            with torch.no_grad():
                outputs = model(**encoding)
    
                # Get token embeddings and attention mask
                token_embeddings = outputs.last_hidden_state  
                attention_mask = encoding["attention_mask"]   

                # Mean pooling (padding tokens will be masked out during mean pooling)
                mask = attention_mask.unsqueeze(-1).expand(token_embeddings.size()).float()
                sum_hidden = torch.sum(token_embeddings * mask, dim=1)
                count_tokens = torch.clamp(mask.sum(dim=1), min=1e-9)
                mean_embedding = sum_hidden / count_tokens 

            # Store
            all_embeddings.append(mean_embedding.squeeze(0).cpu().numpy())
            subject_ids.append(subject_id)
        
    return all_embeddings, subject_ids

# Calculate embeddings for each serialization template format and save as csv files
print("Calculating gatortron-base-2k embeddings.")

#input_paths = [list_template_path, text_template_path, llm_template_path]
input_paths = [llm_template_path]

for input_path in input_paths:
    
    if input_path == list_template_path:
        template_name = "list_template"
    elif input_path == text_template_path:
        template_name = "text_template"
    else:
        template_name = "llm_template"
        
    print(f"Processing {template_name}")
    
    all_embeddings, subject_ids = embeddings(input_path)

    # Convert to DataFrame
    df = pd.DataFrame(all_embeddings)
    df.insert(0, "subject_id", subject_ids)
    
    # Save as a csv file
    output_path = os.path.join(paths["embeddings"], f"gatortron_base_2k_{template_name}_embeddings3.csv")
    df.to_csv(output_path, index=False)
    
    print(f"Embeddings for {template_name} saved to {output_path}")
