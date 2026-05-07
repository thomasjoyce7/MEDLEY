# Script to extract raw data for mechanical ventilation from icu/procedureevents 

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

# Search for terms related to mechanical ventilation in icu/d_items
mv_itemid <- dbGetQuery(con, "SELECT itemid, label, linksto FROM 'icu/d_items'
                        WHERE lower(label) LIKE '%ventilator%'
                         OR lower(label) LIKE '%ventilation%'
                         OR lower(label) LIKE '%mechanical ventilation%'")

# Search for mechanical ventilation recordings in icu/procedureevents
itemid_list <- paste(mv_itemid$itemid, collapse = ",")

query <- paste0("
  SELECT subject_id, hadm_id, stay_id, starttime, endtime, itemid, value, valueuom
  FROM 'icu/procedureevents'
  WHERE itemid IN (", itemid_list, ")
")

message("Extracting mechanical ventilation data from icu/procedureevents.")
procedureevents <- dbGetQuery(con, query)

# Save output files
write_csv(procedureevents, "data/raw/icu_procedureevents_ventilation_filtered.csv")
message("Mechanical ventilation data from procedureevents raw table successfully extracted and saved.")


# Disconnect
dbDisconnect(con)