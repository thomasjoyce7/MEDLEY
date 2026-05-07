# Script to filter embeddings for the POD applications to subjects in the final cohort
# Save as CSV files and do not attach demographic and clinical features 

# Load libraries
library(readr)
library(tidyverse)
library(yaml)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Load clinical and demographic features for the final cohort
final_cohort_features <- read_csv(file.path(config$paths$cohorts, "final_cohort_features.csv"))

# Load embeddings
clinical_longformer_list_emb <- read_csv(file.path(config$paths$embeddings, "clinical_longformer_list_template_embeddings.csv"))
clinical_longformer_text_emb <- read_csv(file.path(config$paths$embeddings, "clinical_longformer_text_template_embeddings.csv"))
clinical_longformer_llm_emb2 <- read_csv(file.path(config$paths$embeddings, "clinical_longformer_llm_template_embeddings2.csv"))

gatortron_list_emb <- read_csv(file.path(config$paths$embeddings, "gatortron_base_2k_list_template_embeddings.csv"))
gatortron_text_emb <- read_csv(file.path(config$paths$embeddings, "gatortron_base_2k_text_template_embeddings.csv"))
gatortron_llm_emb2 <- read_csv(file.path(config$paths$embeddings, "gatortron_base_2k_llm_template_embeddings2.csv"))

longformer_base_list_emb <- read_csv(file.path(config$paths$embeddings, "longformer_base_list_template_embeddings.csv"))
longformer_base_text_emb <- read_csv(file.path(config$paths$embeddings, "longformer_base_text_template_embeddings.csv"))
longformer_base_llm_emb2 <- read_csv(file.path(config$paths$embeddings, "longformer_base_llm_template_embeddings2.csv"))

Qwen3_embeddings_8B_list_template <- read_csv(file.path(config$paths$embeddings, "Qwen3_embeddings_8B_list_template.csv"))
Qwen3_embeddings_8B_text_template <- read_csv(file.path(config$paths$embeddings, "Qwen3_embeddings_8B_text_template.csv"))
Qwen3_embeddings_8B_llm_template <- read_csv(file.path(config$paths$embeddings, "Qwen3_embeddings_8B_llm_template.csv"))

SFR_embedding_mistral_list_template <- read_csv(file.path(config$paths$embeddings, "SFR_embedding_mistral_list_template.csv"))
SFR_embedding_mistral_text_template <- read_csv(file.path(config$paths$embeddings, "SFR_embedding_mistral_text_template.csv"))
SFR_embedding_mistral_llm_template <- read_csv(file.path(config$paths$embeddings, "SFR_embedding_mistral_llm_template.csv"))

# Filter embeddings to subject_ids in final_cohort_features 

pod_clinical_longformer_list_template_embeddings <- clinical_longformer_list_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_clinical_longformer_text_template_embeddings <- clinical_longformer_text_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_clinical_longformer_llm_template_embeddings <- clinical_longformer_llm_emb2 %>% filter(subject_id %in% final_cohort_features$subject_id)

pod_gatortron_base_2k_list_template_embeddings <- gatortron_list_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_gatortron_base_2k_text_template_embeddings <- gatortron_text_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_gatortron_base_2k_llm_template_embeddings <- gatortron_llm_emb2 %>% filter(subject_id %in% final_cohort_features$subject_id)

pod_longformer_base_list_template_embeddings <- longformer_base_list_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_longformer_base_text_template_embeddings <- longformer_base_text_emb %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_longformer_base_llm_template_embeddings <- longformer_base_llm_emb2 %>% filter(subject_id %in% final_cohort_features$subject_id)

pod_Qwen3_embeddings_8B_list_template <- Qwen3_embeddings_8B_list_template %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_Qwen3_embeddings_8B_text_template <- Qwen3_embeddings_8B_text_template %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_Qwen3_embeddings_8B_llm_template <- Qwen3_embeddings_8B_llm_template %>% filter(subject_id %in% final_cohort_features$subject_id)

pod_SFR_embedding_mistral_list_template <- SFR_embedding_mistral_list_template %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_SFR_embedding_mistral_text_template <- SFR_embedding_mistral_text_template %>% filter(subject_id %in% final_cohort_features$subject_id)
pod_SFR_embedding_mistral_llm_template <- SFR_embedding_mistral_llm_template %>% filter(subject_id %in% final_cohort_features$subject_id)


# Save embedding copies for the final POD study cohort without clinical and demographic features attached

write_csv(pod_clinical_longformer_list_template_embeddings, "data/embeddings/01_POD/pod_clinical_longformer_list_template_embeddings.csv")
write_csv(pod_clinical_longformer_text_template_embeddings, "data/embeddings/01_POD/pod_clinical_longformer_text_template_embeddings.csv")
write_csv(pod_clinical_longformer_llm_template_embeddings, "data/embeddings/01_POD/pod_clinical_longformer_llm_template_embeddings.csv")

write_csv(pod_gatortron_base_2k_list_template_embeddings, "data/embeddings/01_POD/pod_gatortron_base_2k_list_template_embeddings.csv")
write_csv(pod_gatortron_base_2k_text_template_embeddings, "data/embeddings/01_POD/pod_gatortron_base_2k_text_template_embeddings.csv")
write_csv(pod_gatortron_base_2k_llm_template_embeddings, "data/embeddings/01_POD/pod_gatortron_base_2k_llm_template_embeddings.csv")

write_csv(pod_longformer_base_list_template_embeddings, "data/embeddings/01_POD/pod_longformer_base_list_template_embeddings.csv")
write_csv(pod_longformer_base_text_template_embeddings, "data/embeddings/01_POD/pod_longformer_base_text_template_embeddings.csv")
write_csv(pod_longformer_base_llm_template_embeddings, "data/embeddings/01_POD/pod_longformer_base_llm_template_embeddings.csv")

write_csv(pod_Qwen3_embeddings_8B_list_template, "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_list_template.csv")
write_csv(pod_Qwen3_embeddings_8B_text_template, "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_text_template.csv")
write_csv(pod_Qwen3_embeddings_8B_llm_template, "data/embeddings/01_POD/pod_Qwen3_embeddings_8B_llm_template.csv")

write_csv(pod_SFR_embedding_mistral_list_template, "data/embeddings/01_POD/pod_SFR_embedding_mistral_list_template.csv")
write_csv(pod_SFR_embedding_mistral_text_template, "data/embeddings/01_POD/pod_SFR_embedding_mistral_text_template.csv")
write_csv(pod_SFR_embedding_mistral_llm_template, "data/embeddings/01_POD/pod_SFR_embedding_mistral_llm_template.csv")
