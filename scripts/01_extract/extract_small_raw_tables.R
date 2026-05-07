# Script to extract raw data for all subjects from smaller tables

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

# Define helper to save table
save_table <- function(tbl_name) {
  message(paste("Extracting", tbl_name))
  df <- dbReadTable(con, tbl_name)
  write_csv(df, file = paste0("data/raw/", gsub("/", "_", tbl_name), ".csv"))
}

# Tables to extract
tables <- c(
  "hosp/admissions",
  "hosp/patients",
  "hosp/procedures_icd",
  "hosp/d_icd_procedures",
  "hosp/diagnoses_icd",
  "hosp/d_icd_diagnoses",
  "hosp/omr",
  "icu/icustays",
  "icu/d_items"
)

# Run extraction loop
invisible(lapply(tables, save_table))

# Disconnect
dbDisconnect(con)

message("Small raw tables successfully extracted and saved.")
