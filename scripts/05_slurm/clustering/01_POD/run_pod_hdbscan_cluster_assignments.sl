#!/bin/bash
#SBATCH -n 1
#SBATCH --job-name=pod_hdbscan_clust
#SBATCH --cpus-per-task=1   
#SBATCH --mem=32g           
#SBATCH -t 10:00:00
#SBATCH --output=scripts/05_slurm/clustering/01_POD/hdbscan_clust.out

module purge
module load r/4.4.0

# Print start time
echo "Job started at: $(date)"

# Run and time the R script
/usr/bin/time -v Rscript scripts/07_clustering/01_POD/pod_extract_HDBSCAN_clusters.R

# Print end time
echo "Job ended at: $(date)"