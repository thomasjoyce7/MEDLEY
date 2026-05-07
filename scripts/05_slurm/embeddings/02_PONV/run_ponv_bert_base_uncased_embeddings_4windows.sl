#!/bin/bash

#SBATCH -n 1
#SBATCH --job-name=bert_base
#SBATCH --cpus-per-task=1
#SBATCH --mem=32g
#SBATCH -t 2:00:00
#SBATCH -p l40-gpu
#SBATCH --qos=gpu_access
#SBATCH --gres=gpu:1
#SBATCH --output=scripts/05_slurm/embeddings/02_PONV/bert_base_uncased_4windows.out

module purge
# Adapt these to your cluster environment
module load python/3.12.4 cuda/12.4

# Activate your Python environment
source /path/to/venv/bin/activate

python3 scripts/04_embeddings/02_PONV/ponv_bert_base_uncased_embeddings_4windows.py
