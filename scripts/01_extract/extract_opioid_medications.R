# Script to extract raw data for opioid use from hosp/emar and icu/inputevents 

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

# List of drug names for opioid (Include generic names and trade names)
opioid_names <- tolower(c(
  # Core opioids
  "morphine", "hydrocodone", "oxycodone", "hydromorphone",
  "fentanyl", "tramadol", "methadone", "codeine",
  "oxymorphone", "tapentadol", "meperidine",
  "remifentanil", "alfentanil", "sufentanil",
  "buprenorphine",
  
  # Common trade / alias names
  "dilaudid",
  "norco", "vicodin",
  "percocet", "roxicodone",
  "duragesic"
))


# hosp/emar -------------------------------------------------------------------------

where_clause <- paste0("LOWER(medication) LIKE '%", opioid_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT subject_id, hadm_id, emar_id, emar_seq, pharmacy_id, charttime, medication, event_txt
  FROM 'hosp/emar'
  WHERE ", where_clause
)

message("Extracting opioid medications from hosp/emar.")
emar <- dbGetQuery(con, query)


# icu/inputevents --------------------------------------------------------------------

where_clause <- paste0("LOWER(label) LIKE '%", opioid_names, "%'", collapse = " OR ")

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

message("Extracting opioid medications from icu/inputevents.")
inputevents <- dbGetQuery(con, inputevents_query)


# Save output files -------------------------------------------------------------------

write_csv(emar, "data/raw/hosp_emar_opioids_filtered.csv")
write_csv(inputevents, "data/raw/icu_inputevents_opioids_filtered.csv")

message("Opioid medications raw tables successfully extracted and saved.")

# Disconnect
dbDisconnect(con)