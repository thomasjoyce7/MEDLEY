# Script to use the Llama3.3-70B-Instruct decoder-only LLM to summarize the list templates
# Run this script using H100 GPU; it may take several hours to run
# Settings: Llama-3.3-70B-Instruct with beam search, length_penalty = 0.9, max_new_tokens = 512; use 4 beams by default and 2 beams if the input text exceeds 3000 tokens

import os
import argparse
import json
from transformers import AutoTokenizer, LlamaForCausalLM
import torch
import gc
from pathlib import Path

# Step to avoid fragmentation and save memory 
os.environ["PYTORCH_CUDA_ALLOC_CONF"] = "expandable_segments:True,max_split_size_mb:128"

REPO_ROOT = Path(__file__).resolve().parents[3]
DEFAULT_INPUT_PATH = REPO_ROOT / "data" / "medications_list_template.jsonl"
DEFAULT_OUTPUT_PATH = REPO_ROOT / "data" / "llm_inputs" / "medications_llama3.3_template.jsonl"


def parse_args():
    parser = argparse.ArgumentParser(
        description="Generate POD medication templates with a user-specified decoder LLM."
    )
    parser.add_argument(
        "--model-path",
        default=os.getenv("LLAMA_MODEL_PATH"),
        help=(
            "Path or Hugging Face model identifier for the decoder LLM. "
            "Can also be supplied with the LLAMA_MODEL_PATH environment variable."
        ),
    )
    parser.add_argument(
        "--input-path",
        type=Path,
        default=DEFAULT_INPUT_PATH,
        help="Relative or absolute path to the POD list-template JSONL input.",
    )
    parser.add_argument(
        "--output-path",
        type=Path,
        default=DEFAULT_OUTPUT_PATH,
        help="Relative or absolute output path. Defaults to data/llm_inputs/.",
    )
    return parser.parse_args()


def resolve_path(path):
    return path if path.is_absolute() else REPO_ROOT / path

# Function to generate medication summaries for each patient using decoder-only LLM
def llm_template(input_path, tokenizer, model, device):
    text_outputs = []
    two_beam_count = 0

    with open(input_path, "r", encoding = "utf-8") as f:
        for line in f:
            # Reset peak stats for this sample
            #torch.cuda.reset_peak_memory_stats()
            
            record = json.loads(line)
            subject_id = record["subject_id"]
            text = record["text"]
            
            # Construct full prompt with placeholder for input text
            prompt = f"""[INST] <<SYS>>
You are a helpful assistant that summarizes clinical medication histories for postoperative delirium (POD) in ICU surgery patients.
<</SYS>>

Your job is to summarize the patient's POD medication use history into a concise, well-structured
narrative that can later be embedded. The input text to be summarized will contain patient background information,
followed by POD medication use history in chronological order. 

The following medications are commonly used to treat POD: 
Dexmedetomidine, Quetiapine, Haloperidol, Olanzapine, Risperidone, Chlorpromazine, Rivastigmine. 

In your summary, follow the style guide below *exactly*. 

Style Guide
1. Write in full sentences. 
2. Always begin the summary with patient background information. 
3. If POD medications are not mentioned, clearly state that the patient did not take any POD medications. 
4. For each medication, include duration, dosage, units, and route.
5. If the same medication appears repeatedly, condense this information into a single administration.  
6. Never hallucinate: if a medication is not mentioned, do not invent it. 
7. Do not repeat the summary or mention instructions in the style guide. 

Here is the POD medication use history to be summarized:

{text}
[/INST]
"""
            # 1 sample per batch, so no padding needed 
            inputs = tokenizer(prompt, return_tensors="pt", truncation=True)
            input_ids = inputs["input_ids"].to(device)
            attention_mask = inputs["attention_mask"].to(device)

            # Adjust num_beams based on input length
            if input_ids.shape[1] > 3000:
                beams_for_this_sample = 2 # long inputs will use 2 beams to save GPU memory 
                two_beam_count += 1
            else:
                beams_for_this_sample = 4 # most samples will use 4 beams 

            # Tokenize and move input to model device
            with torch.no_grad():
                eos = tokenizer.eos_token_id
                generated_tokens = model.generate(input_ids,
                                                  attention_mask=attention_mask,
                                                  do_sample=False,
                                                  max_new_tokens=512,
                                                  num_beams=beams_for_this_sample, 
                                                  early_stopping=True,
                                                  length_penalty=0.9, # Encourage slightly shorter outputs 
                                                  return_dict_in_generate=False,
                                                  output_scores=False,
                                                  pad_token_id=eos,
                                                  eos_token_id=eos)
                output_ids = generated_tokens[0][input_ids.shape[1]:]
                generated_text = tokenizer.decode(output_ids, skip_special_tokens=True).strip()
                
                # Note: Counting the number of tokens seems to add some computational time 
                # num_tokens = len(tokenizer.encode(generated_text, add_special_tokens=False))

            text_outputs.append((subject_id, generated_text))
            
            # Free up GPU memory before processing the next sample 
            del input_ids, attention_mask, generated_tokens, output_ids
            torch.cuda.empty_cache()
            gc.collect()

    print(f"Total samples using two beams: {two_beam_count}")
    return text_outputs

def main():
    args = parse_args()
    if not args.model_path:
        raise ValueError(
            "Please specify the decoder LLM model path with --model-path or LLAMA_MODEL_PATH."
        )

    input_path = resolve_path(args.input_path)
    output_path = resolve_path(args.output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Load tokenizer and model from the user-specified model path or model identifier.
    tokenizer = AutoTokenizer.from_pretrained(args.model_path)
    model = LlamaForCausalLM.from_pretrained(
        args.model_path,
        torch_dtype=torch.float16,
        device_map="auto"
    )

    # Set model to evaluation mode
    model.eval()

    # Get device from model
    device = model.device if hasattr(model, "device") else next(model.parameters()).device

    # Save text output for each patient
    medication_texts = llm_template(input_path, tokenizer, model, device)

    # Save as .jsonl
    with open(output_path, "w", encoding="utf-8") as f:
        for subject_id, text in medication_texts:
            json.dump({"subject_id": subject_id, "text": text}, f)
            f.write("\n")

    print(f"Medication LLM-generated templates successfully saved to {output_path}.")


if __name__ == "__main__":
    main()
