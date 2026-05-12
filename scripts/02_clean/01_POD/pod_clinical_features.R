# Script to obtain and clean other clinical features for POD positive patients

# Import libraries
library(tidyverse)
library(readr)
library(naniar)
library(stringr)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

message("Obtaining and cleaning clinical features.")

# Load files ---------------------------------------------------------------------------------------
pod_positive <- read_csv(file.path(config$paths$derived_data, "pod_positive_identifiers.csv"))
icu_surgery_with_cam_pos <- read_csv(file.path(config$paths$derived_data, "icu_surgery_with_cam_pos.csv"))

icu_chartevents_bmi <- read_csv(file.path(config$paths$raw_data, "icu_chartevents_bmi_filtered.csv"))
hosp_omr <- read_csv(file.path(config$paths$raw_data, "hosp_omr.csv"))

hosp_diagnoses_icd <- read_csv(file.path(config$paths$raw_data, "hosp_diagnoses_icd.csv"))
hosp_d_icd_diagnoses <- read_csv(file.path(config$paths$raw_data, "hosp_d_icd_diagnoses.csv"))
hosp_admissions <- read_csv(file.path(config$paths$raw_data, "hosp_admissions.csv"))

icu_d_items <- read_csv(file.path(config$paths$raw_data, "icu_d_items.csv"))

icu_procedureevents_ventilation <- read_csv(file.path(config$paths$raw_data, "icu_procedureevents_ventilation_filtered.csv"))

# BMI --------------------------------------------------------------------------------------

## Most Recent Admission --------------------------------------------------------------------

# Search for BMI-related itmeids in icd_d_items
bmi_itemid <- icu_d_items %>%
  filter(
    grepl("bmi", label, ignore.case = TRUE) |
      grepl("body mass index", label, ignore.case = TRUE) |
      grepl("height", label, ignore.case = TRUE) |
      grepl("weight", label, ignore.case = TRUE)
  ) %>%
  select(itemid, label, linksto, category)

# First, check for BMI values for the most recent hospital or ICU admission

# Add corresponding BMI-related labels to icu_chartevents and drop the itemid column
icu_chartevents_bmi_curr <- icu_chartevents_bmi %>% filter(itemid %in% c(224639, 226512, 226531, 226707, 226730)) %>%
  mutate(label = case_when(itemid == 224639 ~ "Daily Weight",
                           itemid == 226512 ~ "Admission Weight (Kg)",
                           itemid == 226531 ~ "Admission Weight (lbs)",
                           itemid == 226707 ~ "Height",
                           itemid == 226730 ~ "Height (cm)")) %>% select(-itemid)

# Filter to POD positive patients
icu_chartevents_bmi_curr <- icu_chartevents_bmi_curr %>% filter(stay_id %in% pod_positive$stay_id) 

# Extract Admission Weight (Kg) and Height (cm)
weight <- icu_chartevents_bmi_curr %>% filter(label == "Admission Weight (Kg)") %>% select(subject_id, hadm_id, stay_id, value) %>% rename(weight_kg = value)

height <- icu_chartevents_bmi_curr %>% filter(label == "Height (cm)") %>% select(subject_id, hadm_id, stay_id, value) %>% rename(height_cm = value)

# Remove weights and heights that are implausible
weight <- weight %>% filter(weight_kg >= 30 & weight_kg <= 400)

height <- height %>% filter(height_cm >= 80 & height_cm <= 250)

# Calculate BMI for subjects with weight and height values
chartevents_bmi_curr <- merge(weight, height, by = c("subject_id", "hadm_id", "stay_id"))

chartevents_bmi_curr$BMI <- (chartevents_bmi_curr$weight_kg) / ((chartevents_bmi_curr$height_cm/100)**2)

chartevents_bmi_curr <- chartevents_bmi_curr %>% select(-weight_kg, -height_cm)


# Not all patients have height and weight measurements in icu/chartevents
# Search for additional BMI measurements in hosp/omr
hosp_omr_curr <- hosp_omr %>% filter(subject_id %in% pod_positive$subject_id)

omr_bmi_curr <- hosp_omr_curr %>% filter(result_name %in% c("BMI", "BMI (kg/m2)")) %>% rename(BMI = result_value)

omr_bmi_curr$BMI <- as.numeric(omr_bmi_curr$BMI)

# Filter omr_bmi_curr to the first BMI measurements recorded during the most recent hospital admission
omr_bmi_curr <- merge(omr_bmi_curr %>% select(subject_id, BMI, chartdate, seq_num), pod_positive, by = "subject_id") %>%
  filter(chartdate >= hosp_admittime & chartdate <= hosp_dischtime) %>%
  group_by(subject_id, hadm_id) %>% filter(chartdate == min(chartdate)) %>% ungroup() %>%
  group_by(subject_id, hadm_id, chartdate) %>% filter(seq_num == min(seq_num)) %>% ungroup() %>%
  select(subject_id, hadm_id, stay_id, BMI)

# Filter omr_bmi to subjects NOT in chartevents_bmi_curr and remove implausible BMI values
omr_bmi_curr <- omr_bmi_curr %>% filter(!(subject_id %in% chartevents_bmi_curr$subject_id)) %>% filter(BMI >= 5 & BMI <= 200)

# Combine chartevents_bmi_curr and omr_bmi_curr
bmi <- rbind(chartevents_bmi_curr, omr_bmi_curr)

pct_bmi <- round(100*(nrow(bmi)/nrow(pod_positive)),2)
message(sprintf("%s%% of POD positive patients have BMI measurements for the most recent hadm_id.", pct_bmi))


## Previous Admissions -----------------------------------------------------------------

# Carry forward any BMI values from previous admissions to reduce the missingness proportion

# Add corresponding BMI-related labels to icu_chartevents and drop the itemid column
icu_chartevents_bmi_prev <- icu_chartevents_bmi %>% filter(itemid %in% c(224639, 226512, 226531, 226707, 226730)) %>%
  mutate(label = case_when(itemid == 224639 ~ "Daily Weight",
                           itemid == 226512 ~ "Admission Weight (Kg)",
                           itemid == 226531 ~ "Admission Weight (lbs)",
                           itemid == 226707 ~ "Height",
                           itemid == 226730 ~ "Height (cm)")) %>% select(-itemid)

# Filter to POD positive patients missing BMI values for the most recent admission
icu_chartevents_bmi_prev <- icu_chartevents_bmi_prev %>% filter(subject_id %in% pod_positive$subject_id) %>% filter(!(subject_id %in% bmi$subject_id))

# Extract Admission Weight (Kg) and Height (cm)
weight <- icu_chartevents_bmi_prev %>% filter(label == "Admission Weight (Kg)") %>% select(subject_id, hadm_id, stay_id, charttime, value) %>% rename(weight_kg = value)

height <- icu_chartevents_bmi_prev %>% filter(label == "Height (cm)") %>% select(subject_id, hadm_id, stay_id, charttime, value) %>% rename(height_cm = value)

# Remove weights and heights that are implausible
weight <- weight %>% filter(weight_kg >= 30 & weight_kg <= 400)

height <- height %>% filter(height_cm >= 80 & height_cm <= 250)

# Calculate BMI for subjects with weight and height values
chartevents_bmi_prev <- merge(weight, height, by = c("subject_id", "hadm_id", "stay_id", "charttime"))

chartevents_bmi_prev$BMI <- (chartevents_bmi_prev$weight_kg) / ((chartevents_bmi_prev$height_cm/100)**2)

chartevents_bmi_prev <- chartevents_bmi_prev %>% select(-weight_kg, -height_cm)

# Filter chartevents_prev_bmi to the most recent BMI measurements occurring before icu_intime for the ICU stay of interest
chartevents_bmi_prev <- chartevents_bmi_prev %>% select(subject_id, charttime, BMI) %>% left_join(pod_positive %>% select(subject_id, hadm_id, stay_id, icu_intime), by = "subject_id") %>% 
  group_by(subject_id) %>% filter(charttime <= icu_intime) %>% filter(charttime == max(charttime)) %>% ungroup() %>%
  select(subject_id, hadm_id, stay_id, BMI)

# Not all patients have height and weight measurements in icu/chartevents
# Search for additional BMI measurements in hosp/omr
hosp_omr_prev <- hosp_omr %>% filter(subject_id %in% pod_positive$subject_id) %>% filter(!(subject_id %in% bmi$subject_id))

omr_bmi_prev <- hosp_omr_prev %>% filter(result_name %in% c("BMI", "BMI (kg/m2)")) %>% rename(BMI = result_value)

omr_bmi_prev$BMI <- as.numeric(omr_bmi_prev$BMI)

# Filter omr_bmi_prev to subjects NOT in chartevents_bmi_prev and remove implausible BMI values
omr_bmi_prev <- omr_bmi_prev %>% filter(!(subject_id %in% chartevents_bmi_prev$subject_id)) %>% filter(BMI >= 5 & BMI <= 200)

# Filter omr_bmi_prev to the most recent BMI measurements occurring before icu_intime for the ICU stay of interest
omr_bmi_prev <- omr_bmi_prev %>% left_join(pod_positive %>% select(subject_id, hadm_id, stay_id, icu_intime), by = "subject_id") %>%
  group_by(subject_id) %>% filter(chartdate <= icu_intime) %>% filter(chartdate == max(chartdate)) %>% filter(seq_num == max(seq_num)) %>% ungroup() %>%
  select(subject_id, hadm_id, stay_id, BMI)

# Combine bmi, chartevents_bmi_prev, and omr_bmi_prev
bmi <- rbind(bmi, chartevents_bmi_prev, omr_bmi_prev)

# Summarize BMI
bmi %>% summarize(min(BMI), median(BMI), mean(BMI), max(BMI))

pct_bmi <- round(100*(nrow(bmi)/nrow(pod_positive)),2)
message(sprintf("%s%% of POD positive patients have BMI measurements after carrying the last observation forward.", pct_bmi))

# Add NA values for patients missing BMI
bmi <- left_join(pod_positive %>% select(subject_id, hadm_id, stay_id), bmi, by = c("subject_id", "hadm_id", "stay_id"))


# Diabetes mellitus ----------------------------------------------------------------------

diabetes_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & str_starts(icd_code, "250")) |
      (icd_version == 10 & (
        str_starts(icd_code, "E08") |
          str_starts(icd_code, "E09") |
          str_starts(icd_code, "E10") |
          str_starts(icd_code, "E11") |
          str_starts(icd_code, "E13")
      ))
  )

# Extract diabetes diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
diabetes_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% diabetes_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)
  
diabetes_diagnoses <- merge(diabetes_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

diabetes_diagnoses <- left_join(diabetes_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

diabetes <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(diabetes = case_when(subject_id %in% diabetes_diagnoses$subject_id ~ "Yes", TRUE ~ "No")) 


# History of stroke -------------------------------------------------------------------

stroke_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "430") |
        str_starts(icd_code, "431") |
        str_starts(icd_code, "432") |
        str_starts(icd_code, "433") |
        str_starts(icd_code, "434") |
        str_starts(icd_code, "436") |
        str_starts(icd_code, "438")
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "I60") |
          str_starts(icd_code, "I61") |
          str_starts(icd_code, "I62") |
          str_starts(icd_code, "I63") |
          str_starts(icd_code, "I64") |
          str_starts(icd_code, "I69")
      ))
  )

# Extract stroke diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
stroke_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% stroke_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

stroke_diagnoses <- merge(stroke_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

stroke_diagnoses <- left_join(stroke_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

stroke <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(stroke = case_when(subject_id %in% stroke_diagnoses$subject_id ~ "Yes", TRUE ~ "No")) 


# Alcohol use disorder ---------------------------------------------------------------------------------------------

alcohol_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "291") |
        str_starts(icd_code, "303") |
        str_starts(icd_code, "3050")  
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "F10") |
          str_starts(icd_code, "K70")
      ))
  )

# Extract alcohol use disorder diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
alcohol_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% alcohol_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

alcohol_diagnoses <- merge(alcohol_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

alcohol_diagnoses <- left_join(alcohol_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

alcohol <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(alcohol_use_disorder = case_when(subject_id %in% alcohol_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Hypertension -------------------------------------------------------------------------------

hypertension_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "401") |
        str_starts(icd_code, "402") |
        str_starts(icd_code, "403") |
        str_starts(icd_code, "404") |
        str_starts(icd_code, "405")
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "I10") |
          str_starts(icd_code, "I11") |
          str_starts(icd_code, "I12") |
          str_starts(icd_code, "I13") |
          str_starts(icd_code, "I15") |
          str_starts(icd_code, "I16") |
          str_starts(icd_code, "I1A")
      ))
  )

# Extract hypertension diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
hypertension_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% hypertension_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

hypertension_diagnoses <- merge(hypertension_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

hypertension_diagnoses <- left_join(hypertension_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

hypertension <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(hypertension = case_when(subject_id %in% hypertension_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# History of smoking ------------------------------------------------------------------------

smoking_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "3051") |
        str_starts(icd_code, "V1582")
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "F17") |
          str_starts(icd_code, "Z720") |
          str_starts(icd_code, "Z87891")
      ))
  )

# Extract smoking history for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
smoking_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% smoking_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

smoking_diagnoses <- merge(smoking_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

smoking_diagnoses <- left_join(smoking_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

smoking <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(smoking = case_when(subject_id %in% smoking_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# COPD -------------------------------------------------------------------------

copd_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "490") |
        str_starts(icd_code, "491") |
        str_starts(icd_code, "492") |
        str_starts(icd_code, "494") |
        str_starts(icd_code, "496")
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "J40") |
          str_starts(icd_code, "J41") |
          str_starts(icd_code, "J42") |
          str_starts(icd_code, "J43") |
          str_starts(icd_code, "J44") |
          str_starts(icd_code, "J47")
      ))
  )

# Extract COPD diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
copd_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% copd_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

copd_diagnoses <- merge(copd_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

copd_diagnoses <- left_join(copd_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

copd <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(COPD = case_when(subject_id %in% copd_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Liver disease ----------------------------------------------------------------------------

liver_disease_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "070") |  
        str_starts(icd_code, "571") | 
        str_starts(icd_code, "571") |  
        str_starts(icd_code, "572") | 
        str_starts(icd_code, "573")   
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "K70") |  
          str_starts(icd_code, "K71") |  
          str_starts(icd_code, "K72") |  
          str_starts(icd_code, "K73") |  
          str_starts(icd_code, "K74") |  
          str_starts(icd_code, "K75") |  
          str_starts(icd_code, "K76") | 
          str_starts(icd_code, "B18")    
      ))
  )

# Extract liver disease diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
liver_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% liver_disease_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

liver_diagnoses <- merge(liver_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

liver_diagnoses <- left_join(liver_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

liver_disease <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(liver_disease = case_when(subject_id %in% liver_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Chronic kidney disease (CKD) -----------------------------------------------------------------------

ckd_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "585") |  
        str_starts(icd_code, "586")    
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "N18") |  
          str_starts(icd_code, "N19") |  
          icd_code %in% c(
            "E0822", "E0922", "E1022", "E1122", "E1322",  
            "I120", "I129", "I130", "I131", "I132"
          )
      ))
  )

# Extract CKD diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
ckd_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% ckd_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

ckd_diagnoses <- merge(ckd_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

ckd_diagnoses <- left_join(ckd_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

chronic_kidney_disease <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(CKD = case_when(subject_id %in% ckd_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Dementia --------------------------------------------------------------------

dementia_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "290") |   
        str_starts(icd_code, "2941") |  
        str_starts(icd_code, "2942") |   
        icd_code == "3310"               
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "F00") |
          str_starts(icd_code, "F01") |
          str_starts(icd_code, "F02") |    
          str_starts(icd_code, "F03") |    
          str_starts(icd_code, "G30") |   
          icd_code == "G311"             
      ))
  )

# Extract dementia diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
dementia_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% dementia_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

dementia_diagnoses <- merge(dementia_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

dementia_diagnoses <- left_join(dementia_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

dementia <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(dementia = case_when(subject_id %in% dementia_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Depression -------------------------------------------------------------------

depression_icd_codes <- hosp_d_icd_diagnoses %>%
  filter(
    (icd_version == 9 & (
      str_starts(icd_code, "2962") |  
        str_starts(icd_code, "2963") | 
        icd_code == "3004" |
        icd_code == "3091" |
        icd_code == "311"               
    )) |
      (icd_version == 10 & (
        str_starts(icd_code, "F32") |   
          str_starts(icd_code, "F33") |   
          icd_code == "F341" |            
          icd_code == "F329" |
          icd_code == "F530"
      ))
  )

# Extract depression diagnoses for POD positive patients
# Qualifying diagnoses may occur at any hadm_id leading up to the most recent hadm_id
depression_diagnoses <- hosp_diagnoses_icd %>% filter(subject_id %in% pod_positive$subject_id & icd_code %in% depression_icd_codes$icd_code) %>%
  select(subject_id, hadm_id)

depression_diagnoses <- merge(depression_diagnoses, hosp_admissions %>% select(subject_id, hadm_id, admittime, dischtime), by = c("subject_id", "hadm_id")) %>% distinct()

depression_diagnoses <- left_join(depression_diagnoses, pod_positive %>% select(subject_id, hosp_dischtime), by = "subject_id") %>% filter(dischtime <= hosp_dischtime)

depression <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(depression = case_when(subject_id %in% depression_diagnoses$subject_id ~ "Yes", TRUE ~ "No"))


# Mechanical ventilator use ----------------------------------------------------------

# Create a binary feature for mechanical ventilator use during the ICU stay
icu_ventilation <- left_join(icu_procedureevents_ventilation, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)
icu_ventilation <- icu_ventilation %>% filter(stay_id %in% pod_positive$stay_id)

ventilation <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(
  ventilator_use = case_when(subject_id %in% icu_ventilation$subject_id ~ "Yes", TRUE ~ "No")
)

# Create additional binary feature for invasive ventilation
icu_ventilation <- left_join(icu_procedureevents_ventilation, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)
invasive_ventilation <- icu_ventilation %>% filter(stay_id %in% pod_positive$stay_id & label == "Invasive Ventilation")

invasive_ventilation <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(
  invasive_ventilation = case_when(subject_id %in% invasive_ventilation$subject_id ~ "Yes", TRUE ~ "No")
)

# Create additional binary feature for non-invasive ventilation
icu_ventilation <- left_join(icu_procedureevents_ventilation, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)
non_invasive_ventilation <- icu_ventilation %>% filter(stay_id %in% pod_positive$stay_id & label == "Non-invasive Ventilation")

non_invasive_ventilation <- pod_positive %>% select(subject_id, hadm_id, stay_id) %>% mutate(
  non_invasive_ventilation = case_when(subject_id %in% non_invasive_ventilation$subject_id ~ "Yes", TRUE ~ "No")
)

# Mechanical ventilator duration ---------------------------------------------------------------

# Create a continuous feature for total mechanical ventilator duration (in minutes) during the ICU stay
icu_ventilation <- left_join(icu_procedureevents_ventilation, icu_d_items %>% select(itemid, label), by = "itemid") %>% select(-itemid)
icu_ventilation <- icu_ventilation %>% filter(stay_id %in% pod_positive$stay_id)

ventilation_duration <- left_join(pod_positive %>% select(subject_id, hadm_id, stay_id), icu_ventilation, by = c("subject_id", "hadm_id", "stay_id"))

ventilation_duration_mins <- ventilation_duration %>%
  filter(!is.na(value), !is.na(valueuom)) %>%
  mutate(
    duration_mins = case_when(
      valueuom == "min"  ~ value,
      valueuom == "hour" ~ value * 60,
      valueuom == "day"  ~ value * 24 * 60,
      TRUE ~ NA_real_
    )
  )

ventilation_duration_mins <- ventilation_duration_mins %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  summarise(
    ventilation_duration_mins = sum(duration_mins, na.rm = TRUE),
    .groups = "drop"
  )

ventilation_duration_mins <- left_join(pod_positive %>% select(subject_id, hadm_id, stay_id), ventilation_duration_mins, by = c("subject_id", "hadm_id", "stay_id")) %>%
  mutate(ventilation_duration_mins = case_when(is.na(ventilation_duration_mins) == TRUE ~ 0,
                                               TRUE ~ ventilation_duration_mins))


# First procedure type ------------------------------------------------------------------------------

# Note: HCUP has a clinical classifications software for classifying ICD-9 and ICD-10
# procedures into a smaller number of clinical domains.

# ICD-9: https://hcup-us.ahrq.gov/toolssoftware/ccs/ccs.jsp#download
# For ICD-9, use the multi-level CSS system. The top level has 16 categories
# broadly defining different operation types. 

# ICD-10: https://hcup-us.ahrq.gov/toolssoftware/ccsr/prccsr.jsp
# For ICD-10, use the 31 clinical domains. 

# For simplicity, consider the first procedure type to be the most frequent clinical domain on the first surgery date for each patient 
# Break ties by taking the procedure that comes first in alphabetical order 

clinical_domains <- icu_surgery_with_cam_pos %>% select(subject_id, hadm_id, stay_id, procedure_date, procedure_description, clinical_domain) %>% distinct()

procedure_types <- clinical_domains %>%
  group_by(subject_id, hadm_id, stay_id) %>%
  filter(procedure_date == min(procedure_date)) %>%
  count(subject_id, clinical_domain) %>%
  group_by(subject_id) %>%
  arrange(desc(n), clinical_domain) %>%   
  slice(1) %>% # take top row (break ties alphabetically)
  ungroup() %>%
  select(subject_id, hadm_id, stay_id, clinical_domain) %>%
  rename(procedure_type = clinical_domain)

# Narrow down procedure types into 6 common categories 
procedure_types <- procedure_types %>% mutate(first_procedure_type = case_when(
  procedure_type %in% c("Cardiovascular Procedures", "Operations on the cardiovascular system") ~ "Cardiovascular System",
  procedure_type %in% c("Musculoskeletal, Subcutaneous Tissue, and Fascia Procedures", "Operations on the musculoskeletal system") ~ "Musculoskeletal",
  procedure_type %in% c("Central Nervous System Procedures", "Operations on the nervous system", "Peripheral Nervous System Procedures") ~ "Nervous System",
  procedure_type %in% c("Gastrointestinal System Procedures", "Operations on the digestive system", "Hepatobiliary and Pancreas Procedures") ~ "Digestive System",
  procedure_type %in% c("Respiratory System Procedures","Operations on the respiratory system") ~ "Respiratory System",
  TRUE ~ "Other"
)) %>% select(-procedure_type)

# Number of surgery days ---------------------------------------------------------

# To approximate the number of surgeries, count the number of distinct procedure dates for each patient 

surgery_count <- icu_surgery_with_cam_pos %>% select(subject_id, hadm_id, stay_id, procedure_date) %>% 
  distinct() %>% group_by(subject_id, hadm_id, stay_id) %>% summarize(surgery_count = n(), .groups = "drop")


# ICU care unit -----------------------------------------------------------------------

# There are 15 total ICU careunits
# We can group these into common categories

icu_careunit <- pod_positive %>% select(subject_id, hadm_id, stay_id, icu_careunit) %>%  
  mutate(icu_careunit = case_when(
    icu_careunit %in% c("Medical Intensive Care Unit (MICU)", 
                        "Medical/Surgical Intensive Care Unit (MICU/SICU)", 
                        "Medicine") ~ "Medical ICU",
    icu_careunit %in% c("Surgical Intensive Care Unit (SICU)", 
                        "Trauma SICU (TSICU)", 
                        "Surgery/Trauma") ~ "Surgical ICU",
    icu_careunit %in% c("Coronary Care Unit (CCU)", 
                        "Cardiac Vascular Intensive Care Unit (CVICU)") ~ "Cardiac ICU",
    icu_careunit %in% c("Neuro Surgical Intensive Care Unit (Neuro SICU)", 
                        "Neurology", 
                        "Neuro Intermediate", 
                        "Neuro Stepdown") ~ "Neuro ICU",
    TRUE ~ "General/Other ICU"
  ))


# Merge clinical features and save output file ----------------------------------------------

clinical_features <- bmi %>%
  left_join(diabetes, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(stroke, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(alcohol, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(hypertension, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(smoking, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(copd, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(liver_disease, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(chronic_kidney_disease, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(dementia, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(depression, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(ventilation, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(invasive_ventilation, by = c("subject_id", "hadm_id", "stay_id")) %>%
  left_join(non_invasive_ventilation, by= c("subject_id", "hadm_id", "stay_id")) %>%
  left_join(ventilation_duration_mins, by = c("subject_id", "hadm_id", "stay_id")) %>%
  left_join(procedure_types, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(surgery_count, by = c("subject_id","hadm_id","stay_id")) %>%
  left_join(icu_careunit, by = c("subject_id","hadm_id","stay_id"))
  
write_csv(clinical_features, "data/derived/clinical_features_cleaned.csv")

message("Clinical features successfully cleaned and saved.")












