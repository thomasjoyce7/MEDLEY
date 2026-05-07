#!/bin/bash

#SBATCH --job-name=tokenize_pod_seq
#SBATCH --cpus-per-task=1   
#SBATCH --mem=32g           
#SBATCH -t 00:20:00     
#SBATCH --output=scripts/05_slurm/embeddings/01_POD/tokenize_seq.out

module purge
# Adapt these to your cluster environment
module load python/3.12.4 cuda/12.4

# Activate your Python environment
source /path/to/venv/bin/activate

python3 scripts/03_descriptives/01_POD/pod_tokenized_seq_lengths_table.py
