# Script to extract raw data for benzodiazepine use from hosp/emar and icu/inputevents 

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

# List of drug names for benzodiazepine (Include generic names and trade names)
# https://www.dea.gov/sites/default/files/2020-06/Benzodiazepenes-2020_1.pdf

drug_names <- tolower(c(
  "benzodiazepine", "alprazolam", "xanax", "clonazepam", "klonopin",
  "diazepam", "valium", "lorazepam", "ativan", "temazepam", "restoril",
  "midazolam", "versed", "triazolam", "halcion", "estazolam", "prosom",
  "flurazepam", "dalmane", "chlordiazepoxide", "librium", "clorazepate",
  "tranxene", "halazepam", "paxipam", "oxazepam", "serax", "prazepam",
  "centrax", "quazepam", "doral"
))


# hosp/emar -------------------------------------------------------------------------

where_clause <- paste0("LOWER(medication) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT subject_id, hadm_id, emar_id, emar_seq, pharmacy_id, charttime, medication, event_txt
  FROM 'hosp/emar'
  WHERE ", where_clause
)

message("Extracting benzodiazepine medications from hosp/emar.")
emar <- dbGetQuery(con, query)


# icu/inputevents --------------------------------------------------------------------

where_clause <- paste0("LOWER(label) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT itemid, label
  FROM 'icu/d_items'
  WHERE ", where_clause
)

inputevent_itemids <- dbGetQuery(con, query)

# Extract inputevent records for the corresponding itemids 
itemid_list <- paste0("(", paste(inputevent_itemids$itemid, collapse = ","), ")")

inputevents_query <- paste0("
  SELECT subject_id, hadm_id, stay_id, starttime, endtime, itemid, amount, amountuom, ordercategorydescription
  FROM 'icu/inputevents'
  WHERE itemid IN ", itemid_list
)

message("Extracting benzodiazepine medications from icu/inputevents.")
inputevents <- dbGetQuery(con, inputevents_query)


# Save output files -------------------------------------------------------------------

write_csv(emar, "data/raw/hosp_emar_benzos_filtered.csv")
write_csv(inputevents, "data/raw/icu_inputevents_benzos_filtered.csv")

message("Benzodiazepine medications raw tables successfully extracted and saved.")

# Disconnect
dbDisconnect(con)



