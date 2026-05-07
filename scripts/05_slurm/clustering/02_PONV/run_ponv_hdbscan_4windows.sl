#!/bin/bash
#SBATCH -n 1
#SBATCH --job-name=ponv_hdbscan_4windows 
#SBATCH --cpus-per-task=1   
#SBATCH --mem=64g           
#SBATCH -t 10:00:00
#SBATCH --output=scripts/05_slurm/clustering/02_PONV/hdbscan_4windows.out

module purge
module load r/4.4.0

# Print start time
echo "Job started at: $(date)"

# Run and time the R script
/usr/bin/time -v Rscript scripts/07_clustering/02_PONV/ponv_HDBSCAN_4windows.R

# Print end time
echo "Job ended at: $(date)"
