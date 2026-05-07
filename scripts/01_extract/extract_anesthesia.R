# Script to extract raw data for anesthesia from hosp/emar, icu/inputevents, and icu/procedureevents 

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

# List of common drug names (both generic and trade names) for the four anesthesia methods
# https://www.uclahealth.org/medical-services/anesthesiology/types-anesthesia
drug_names <- tolower(c(
  # General Anesthesia
  "propofol", "diprivan", "disoprivan",
  "etomidate", "amidate",
  "ketamine", "ketalar", "ketaset", "ketanest",
  "thiopental", "pentothal", "trapanal",
  "methohexital", "brevital",
  "sevoflurane", "ultane", "sevorane",
  "desflurane", "suprane",
  "isoflurane", "forane",
  "nitrous oxide", "laughing gas",
  
  # Regional Anesthesia (incl. spinal, epidural)
  "bupivacaine", "marcaine", "sensorcaine", "vivacaine", "exparel",
  "ropivacaine", "naropin",
  "lidocaine", "xylocaine", "lignocaine",
  "mepivacaine", "carbocaine", "polocaine",
  "tetracaine", "pontocaine", "amethocaine",
  "chloroprocaine", "nesacaine",
  "dibucaine", "nupercainal",
  
  # Sedation (ICU or monitored anesthesia care)
  "midazolam", "versed",
  "lorazepam", "ativan",
  "diazepam", "valium",
  "dexmedetomidine", "precedex", "dexdor", "igalmi",
  "fentanyl", "sublimaze", "actiq", "duragesic", "abstral", "lazanda",
  "remifentanil", "ultiva",
  "sufentanil", "sufenta",
  "alfentanil", "alfenta",
  "zolpidem", "ambien", "edluar", "intermezzo", "zolpimist",
  "propofol-ketamine",
  
  # Local Anesthesia
  "lidocaine", "xylocaine",
  "prilocaine", "citanest",
  "articaine", "septocaine",
  "benzocaine", "orajel", "anbesol", "cepacol",
  "procaine", "novocain",
  "dyclonine", "sucrets",
  "tetracaine", "pontocaine",
  
  # Additional anesthetic agents
  "isoflurane",
  "forane",
  "diprivan",
  "disoprivan",
  "dexdor",
  "exparel",
  "naropin",
  "carbocaine",
  "nesacaine"
  
))


# hosp/emar -------------------------------------------------------------------------

where_clause <- paste0("LOWER(medication) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT subject_id, hadm_id, emar_id, emar_seq, pharmacy_id, charttime, medication, event_txt
  FROM 'hosp/emar'
  WHERE ", where_clause
)

message("Extracting anesthesia medications from hosp/emar.")
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

message("Extracting anesthesia medications from icu/inputevents.")
inputevents <- dbGetQuery(con, inputevents_query)


# icu/procedureevents ----------------------------------------------------------------------

where_clause <- paste0("LOWER(label) LIKE '%", drug_names, "%'", collapse = " OR ")

query <- paste0("
  SELECT itemid, label
  FROM 'icu/d_items'
  WHERE ", where_clause
)

procedureevent_itemids <- dbGetQuery(con, query)

# Extract procedureevent records for the corresponding itemids 
itemid_list <- paste0("(", paste(procedureevent_itemids$itemid, collapse = ","), ")")

procedureevents_query <- paste0("
  SELECT subject_id, hadm_id, stay_id, starttime, endtime, itemid
  FROM 'icu/procedureevents'
  WHERE itemid IN ", itemid_list
)

message("Extracting anesthesia medications from icu/procedureevents.")
procedureevents <- dbGetQuery(con, procedureevents_query)


# Save output files -------------------------------------------------------------------

write_csv(emar, "data/raw/hosp_emar_anesthesia_filtered.csv")
write_csv(inputevents, "data/raw/icu_inputevents_anesthesia_filtered.csv")

message("Anesthesia medications raw tables successfully extracted and saved.")

# Disconnect
dbDisconnect(con)