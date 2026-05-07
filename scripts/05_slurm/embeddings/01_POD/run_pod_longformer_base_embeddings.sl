#!/bin/bash

#SBATCH -n 1
#SBATCH --cpus-per-task=1
#SBATCH --mem=32g
#SBATCH -t 2:00:00
#SBATCH -p l40-gpu
#SBATCH --qos=gpu_access
#SBATCH --gres=gpu:1

module purge
# Adapt these to your cluster environment
module load python/3.12.4 cuda/12.4

# Activate your Python environment
source /path/to/venv/bin/activate

python3 scripts/04_embeddings/01_POD/pod_longformer_base_embeddings.py
