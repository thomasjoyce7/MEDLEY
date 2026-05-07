# Script to extract raw data for CAM-ICU assessments from the icu/chartevents table

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

# Search for POD terms in icu/d_items
pod_itemid <- dbGetQuery(con, "SELECT itemid, label, linksto FROM 'icu/d_items'
                         WHERE lower(label) LIKE '%delirium%'
                         OR lower(label) LIKE '%cam-icu%'
                         OR lower(label) LIKE '%rass%'")

# Search for POD recordings in icu/chartevents
itemid_list <- paste(pod_itemid$itemid, collapse = ",")

query <- paste0("
  SELECT subject_id, hadm_id, stay_id, charttime, itemid, value
  FROM 'icu/chartevents'
  WHERE itemid IN (", itemid_list, ")
")

message("Extracting CAM-ICU data from icu/chartevents")
chartevents <- dbGetQuery(con, query)

# Save output files
write_csv(chartevents, "data/raw/icu_chartevents_cam_filtered.csv")
message("CAM-ICU data from chartevents raw table successfully extracted and saved.")


# Disconnect
dbDisconnect(con)
