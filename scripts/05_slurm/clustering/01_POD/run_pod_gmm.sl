#!/bin/bash
#SBATCH -n 1
#SBATCH --job-name=pod_gmm
#SBATCH --cpus-per-task=1   
#SBATCH --mem=64g           
#SBATCH -t 10:00:00
#SBATCH --output=scripts/05_slurm/clustering/01_POD/gmm.out

module purge
module load r/4.4.0

# Print start time
echo "Job started at: $(date)"

# Run and time the R script
/usr/bin/time -v Rscript scripts/07_clustering/01_POD/pod_GMM.R

# Print end time
echo "Job ended at: $(date)"