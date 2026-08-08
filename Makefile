# MEDLEY Makefile

.DEFAULT_GOAL := help

R ?= Rscript
PYTHON ?= python3
SBATCH ?= sbatch
LLAMA_MODEL_PATH ?=

.PHONY: help all reproduce dirs \
	extract hcup-source-files surgery \
	pod pod-cohort pod-templates pod-llm-template pod-embeddings-local pod-large-embeddings-local pod-filter-embeddings pod-tokenized-seq-lengths \
	pod-benchmark pod-benchmark-clustering pod-benchmark-tables pod-benchmark-figures \
	ponv ponv-cohort ponv-templates ponv-embeddings-local \
	manuscript manuscript-pod manuscript-ponv manuscript-tables manuscript-figures manuscript-pod-figures manuscript-ponv-figures \
	submit-pod-embeddings submit-pod-tokenized-seq-lengths submit-ponv-embeddings submit-pod-clustering submit-ponv-clustering submit-all-slurm \
	logs

help:
	@echo "MEDLEY reproducibility targets"
	@echo ""
	@echo "Main workflows:"
	@echo "  make reproduce              Run extract, HCUP download, cohorts, templates, and manuscript tables"
	@echo "  make extract                Extract raw MIMIC-IV tables to data/raw"
	@echo "  make hcup-source-files      Download HCUP source files to data/external"
	@echo "  make pod                    Build POD cohort inputs and manuscript outputs"
	@echo "  make ponv                   Build PONV cohort inputs and manuscript outputs"
	@echo "  make manuscript             Rebuild manuscript tables and scripted figure outputs"
	@echo ""
	@echo "Embedding and clustering:"
	@echo "  make pod-llm-template       Run POD decoder-LLM template generation; set LLAMA_MODEL_PATH"
	@echo "  make pod-embeddings-local   Run POD encoder embeddings locally"
	@echo "  make pod-large-embeddings-local"
	@echo "                                Run POD Qwen3 and SFR encoder embeddings locally"
	@echo "  make pod-filter-embeddings  Filter POD embeddings to the final POD cohort"
	@echo "  make pod-tokenized-seq-lengths"
	@echo "                                Build POD tokenized sequence length summary table"
	@echo "  make pod-benchmark          Run POD conventional-representation benchmarking"
	@echo "  make ponv-embeddings-local  Run PONV encoder embeddings locally"
	@echo "  make submit-all-slurm       Submit embedding and clustering jobs to SLURM"
	@echo ""
	@echo "Utilities:"
	@echo "  make dirs                   Create expected output directories"
	@echo "  make logs                   Show recent cohort-building log lines"
	@echo ""
	@echo "Database path:"
	@echo "  Set MIMICIV_DB_PATH=/path/to/mimic4.db to override config/paths.yml"
	@echo "Decoder LLM model path:"
	@echo "  Set LLAMA_MODEL_PATH=/path/or/huggingface-id for make pod-llm-template"

all: reproduce

reproduce: dirs extract pod-cohort pod-templates pod-llm-template ponv-cohort ponv-templates manuscript

dirs:
	mkdir -p data/raw data/external data/derived data/derived/01_POD data/derived/02_PONV
	mkdir -p data/llm_inputs data/llm_inputs/01_POD data/llm_inputs/02_PONV data/embeddings data/embeddings/01_POD data/embeddings/02_PONV
	mkdir -p data/cohorts data/cohorts/01_POD data/cohorts/01_POD/AHC_clusters data/cohorts/01_POD/GMM_clusters data/cohorts/01_POD/HDBSCAN_clusters data/cohorts/01_POD/kmeans_clusters data/cohorts/02_PONV data/logs
	mkdir -p data/results/clustering/01_POD/AHC data/results/clustering/01_POD/GMM data/results/clustering/01_POD/HDBSCAN data/results/clustering/01_POD/k_means data/results/clustering/01_POD/benchmarking
	mkdir -p data/results/clustering/02_PONV/AHC data/results/clustering/02_PONV/GMM data/results/clustering/02_PONV/HDBSCAN data/results/clustering/02_PONV/k_means
	mkdir -p figures/main/01_POD figures/main/02_PONV
	mkdir -p figures/clustering/01_POD/AHC figures/clustering/01_POD/GMM figures/clustering/01_POD/HDBSCAN figures/clustering/01_POD/k_means figures/clustering/01_POD/PCA
	mkdir -p figures/clustering/02_PONV/AHC figures/clustering/02_PONV/GMM figures/clustering/02_PONV/HDBSCAN figures/clustering/02_PONV/k_means figures/clustering/02_PONV/PCA
	mkdir -p tables tables/01_POD tables/01_POD/benchmarking tables/02_PONV

################################################################################
# Shared data extraction and surgical cohort
################################################################################

extract: dirs
	$(R) scripts/01_extract/extract_small_raw_tables.R
	$(R) scripts/01_extract/extract_icu_chartevents_cam.R
	$(R) scripts/01_extract/extract_icu_chartevents_bmi.R
	$(R) scripts/01_extract/extract_icu_ventilation.R
	$(R) scripts/01_extract/extract_pod_medications.R
	$(R) scripts/01_extract/extract_ponv_medications.R
	$(R) scripts/01_extract/extract_opioid_medications.R
	$(R) scripts/01_extract/extract_icu_chartevents_rass.R

hcup-source-files: dirs
	$(R) scripts/01_extract/download_hcup_source_files.R

surgery: dirs hcup-source-files
	$(R) scripts/02_clean/identify_surgery_patients.R

################################################################################
# POD application
################################################################################

pod: pod-cohort pod-templates pod-llm-template manuscript-pod

pod-cohort: dirs surgery
	$(R) scripts/02_clean/01_POD/filter_pod_icu_surgery_with_cam.R
	$(R) scripts/02_clean/01_POD/clean_pod_demographics.R
	$(R) scripts/02_clean/01_POD/pod_clinical_features.R
	$(R) scripts/02_clean/01_POD/extract_pod_pos_rass_scores.R
	$(R) scripts/02_clean/01_POD/clean_pod_medications.R
	$(R) scripts/02_clean/01_POD/combine_pod_features.R
	$(R) scripts/02_clean/01_POD/extract_pod_final_cohort.R

pod-templates: dirs
	$(PYTHON) scripts/04_embeddings/01_POD/pod_list_template_serialization.py
	$(PYTHON) scripts/04_embeddings/01_POD/pod_text_template_serialization.py

pod-llm-template: dirs pod-templates
	@if [ -z "$(LLAMA_MODEL_PATH)" ]; then echo "Set LLAMA_MODEL_PATH=/path/or/huggingface-id to run the POD decoder-LLM template."; exit 1; fi
	$(PYTHON) scripts/06_decoder_llm/01_POD/run_llama3.3_beam_search.py --model-path "$(LLAMA_MODEL_PATH)"

pod-embeddings-local: dirs pod-templates pod-llm-template
	$(PYTHON) scripts/04_embeddings/01_POD/pod_Clinical_Longformer_embeddings.py
	$(PYTHON) scripts/04_embeddings/01_POD/pod_gatortron_base_2k_embeddings.py
	$(PYTHON) scripts/04_embeddings/01_POD/pod_longformer_base_embeddings.py

pod-large-embeddings-local: dirs pod-templates pod-llm-template
	$(PYTHON) scripts/04_embeddings/01_POD/pod_Qwen3_embedding_8B.py
	$(PYTHON) scripts/04_embeddings/01_POD/pod_SFR_embedding_mistral.py

pod-filter-embeddings: dirs
	$(R) scripts/02_clean/01_POD/filter_pod_embeddings.R

pod-tokenized-seq-lengths: dirs pod-cohort pod-templates pod-llm-template
	$(PYTHON) scripts/03_descriptives/01_POD/pod_tokenized_seq_lengths_table.py

pod-benchmark: pod-benchmark-clustering pod-benchmark-tables pod-benchmark-figures

pod-benchmark-clustering: dirs pod-cohort
	$(R) scripts/02_clean/01_POD/create_pod_med_reps.R
	$(R) scripts/07_clustering/01_POD/benchmarking/pod_benchmark_HDBSCAN.R

pod-benchmark-tables: dirs pod-benchmark-clustering
	$(R) scripts/03_descriptives/01_POD/benchmarking/pod_benchmark_cluster_characteristics_tables.R

pod-benchmark-figures: dirs pod-benchmark-clustering
	$(R) -e "rmarkdown::render('scripts/03_descriptives/01_POD/benchmarking/pod_benchmark_HDBSCAN_figures.Rmd', quiet = FALSE)"

################################################################################
# PONV application
################################################################################

ponv: ponv-cohort ponv-templates manuscript-ponv

ponv-cohort: dirs surgery
	$(R) scripts/02_clean/02_PONV/extract_ponv_study_cohort.R
	$(R) scripts/02_clean/02_PONV/clean_ponv_demographics.R
	$(R) scripts/02_clean/02_PONV/ponv_clinical_features.R
	$(R) scripts/02_clean/02_PONV/clean_ponv_medications.R
	$(R) scripts/02_clean/02_PONV/combine_ponv_features.R

ponv-templates: dirs
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_list_template_serialization_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_text_template_serialization_4windows.py

ponv-embeddings-local: dirs ponv-templates
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_clinical_longformer_embeddings_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_gatortron_base_2k_embeddings_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_longformer_base_embeddings_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_bio_clinical_bert_embeddings_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_gatortron_base_embeddings_4windows.py
	$(PYTHON) scripts/04_embeddings/02_PONV/ponv_bert_base_uncased_embeddings_4windows.py

################################################################################
# Manuscript tables and scripted figure outputs
################################################################################

manuscript: manuscript-tables manuscript-figures

manuscript-tables: manuscript-pod manuscript-ponv

manuscript-pod: pod-benchmark-tables
	$(R) scripts/03_descriptives/01_POD/pod_baseline_characteristics.R
	$(R) reports/clustering/01_POD/make_pod_clustering_results_tables.R
	$(R) reports/clustering/01_POD/make_pod_clustering_results_tables_large_encoders.R
	$(R) scripts/03_descriptives/01_POD/pod_hdbscan_cluster_characteristics.R
	$(R) scripts/03_descriptives/01_POD/pod_hdbscan_cluster_medications.R
	$(R) scripts/03_descriptives/01_POD/pod_hdbscan_noise_observations.R

manuscript-ponv:
	$(R) scripts/03_descriptives/02_PONV/make_ponv_patients_characteristics_table.R
	$(R) reports/clustering/02_PONV/make_ponv_clustering_results_tables_4windows.R
	$(R) scripts/03_descriptives/02_PONV/Tables_4windows/make_ponv_gmm_gt_base_text_4windows_cluster_characteristics_table.R
	$(R) scripts/03_descriptives/02_PONV/Tables_4windows/make_ponv_gmm_gt_base_text_4windows_cluster_med_tables.R

manuscript-figures: manuscript-pod-figures manuscript-ponv-figures

manuscript-pod-figures: pod-benchmark-figures
	$(R) -e "rmarkdown::render('scripts/03_descriptives/01_POD/HDBSCAN_clusters/pod_hdbscan_cl_list_2pcs_centered_unscaled_cluster_analysis.Rmd', quiet = FALSE)"

manuscript-ponv-figures:
	$(R) -e "rmarkdown::render('scripts/03_descriptives/02_PONV/GMM_clusters_4windows/ponv_gmm_gt_base_text_4windows_5pcs_centered_unscaled_8clusters.Rmd', quiet = FALSE)"

################################################################################
# SLURM submission targets for heavy embedding and clustering jobs
################################################################################

submit-pod-embeddings:
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_clinical_longformer_embeddings.sl
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_gatortron_embeddings.sl
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_longformer_base_embeddings.sl
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_Qwen3_embedding_8B.sl
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_SFR_embedding_mistral.sl

submit-pod-tokenized-seq-lengths:
	$(SBATCH) scripts/05_slurm/embeddings/01_POD/run_pod_tokenized_seq_lengths_table.sl

submit-ponv-embeddings:
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_clinical_longformer_embeddings_4windows.sl
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_gatortron_base_2k_embeddings_4windows.sl
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_longformer_base_embeddings_4windows.sl
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_bio_clinical_bert_embeddings_4windows.sl
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_gatortron_base_embeddings_4windows.sl
	$(SBATCH) scripts/05_slurm/embeddings/02_PONV/run_ponv_bert_base_uncased_embeddings_4windows.sl

submit-pod-clustering:
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_pca_cumvar.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_kmeans.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_gmm.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_hdbscan.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_ahc.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_kmeans_cluster_assignments.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_gmm_cluster_assignments.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_hdbscan_cluster_assignments.sl
	$(SBATCH) scripts/05_slurm/clustering/01_POD/run_pod_ahc_cluster_assignments.sl

submit-ponv-clustering:
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_pca_cumvar_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_kmeans_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_gmm_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_hdbscan_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_ahc_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_kmeans_extract_clusters_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_gmm_extract_clusters_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_hdbscan_extract_clusters_4windows.sl
	$(SBATCH) scripts/05_slurm/clustering/02_PONV/run_ponv_ahc_extract_clusters_4windows.sl

submit-all-slurm: submit-pod-embeddings submit-ponv-embeddings submit-pod-clustering submit-ponv-clustering

logs:
	tail -n 20 data/logs/cohort_building.log
