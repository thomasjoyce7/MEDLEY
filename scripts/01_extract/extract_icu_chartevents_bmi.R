# Script to extract raw data for BMI from the icu/chartevents table

# Load libraries
library(DBI)
library(tidyverse)
library(readr)

# Load config 
config <- yaml::yaml.load_file("config/paths.yml")

# Connect to database
con <- dbConnect(RSQLite::SQLite(), Sys.getenv("MIMICIV_DB_PATH", unset = config$paths$db))

# Create raw output folder
dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Search for BMI-related terms in icu/d_items
pod_itemid <- dbGetQuery(con, "SELECT itemid, label, linksto FROM 'icu/d_items'
                         WHERE lower(label) LIKE '%bmi%'
                         OR lower(label) LIKE '%body mass index%'
                         OR lower(label) LIKE '%height%'
                         OR lower(label) LIKE '%weight%'")

# Search for BMI-related recordings in icu/chartevents
itemid_list <- paste(pod_itemid$itemid, collapse = ",")

query <- paste0("
  SELECT subject_id, hadm_id, stay_id, charttime, itemid, value
  FROM 'icu/chartevents'
  WHERE itemid IN (", itemid_list, ")
")

message("Extracting BMI data from icu/chartevents")
chartevents <- dbGetQuery(con, query)

# Save output files
write_csv(chartevents, "data/raw/icu_bmi_chartevents_filtered.csv")
message("BMI data from chartevents raw table successfully extracted and saved.")


# Disconnect
dbDisconnect(con)