# Download HCUP source files used by scripts/02_clean/identify_surgery_patients.R
#
# Run from the repository root:
#   Rscript scripts/01_extract/download_hcup_source_files.R
#
# Existing files are kept by default. Add --overwrite to replace them:
#   Rscript scripts/01_extract/download_hcup_source_files.R --overwrite

args <- commandArgs(trailingOnly = TRUE)
overwrite <- "--overwrite" %in% args

config_path <- "config/paths.yml"
external_data <- "data/external"

if (file.exists(config_path) && requireNamespace("yaml", quietly = TRUE)) {
  config <- yaml::yaml.load_file(config_path)
  external_data <- config$paths$external_data
}

dir.create(external_data, recursive = TRUE, showWarnings = FALSE)

download_file <- function(url, dest_path, overwrite = FALSE) {
  if (file.exists(dest_path) && !overwrite) {
    message("Keeping existing file: ", dest_path)
    return(invisible(dest_path))
  }

  message("Downloading: ", url)
  utils::download.file(url, dest_path, mode = "wb", quiet = FALSE)
  message("Saved: ", dest_path)
  invisible(dest_path)
}

download_zip_member <- function(url, member_pattern, dest_path, overwrite = FALSE) {
  if (file.exists(dest_path) && !overwrite) {
    message("Keeping existing file: ", dest_path)
    return(invisible(dest_path))
  }

  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile()
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  on.exit({
    unlink(temp_zip, force = TRUE)
    unlink(temp_dir, recursive = TRUE, force = TRUE)
  }, add = TRUE)

  message("Downloading: ", url)
  utils::download.file(url, temp_zip, mode = "wb", quiet = FALSE)

  zip_contents <- utils::unzip(temp_zip, list = TRUE)
  member <- zip_contents$Name[grepl(member_pattern, zip_contents$Name, ignore.case = TRUE)]

  if (length(member) == 0) {
    stop("No file matching '", member_pattern, "' found in ", url, call. = FALSE)
  }

  if (length(member) > 1) {
    message("Multiple matching files found; using: ", member[[1]])
  }

  utils::unzip(temp_zip, files = member[[1]], exdir = temp_dir)
  extracted_path <- file.path(temp_dir, member[[1]])
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  file.copy(extracted_path, dest_path, overwrite = TRUE)

  message("Saved: ", dest_path)
  invisible(dest_path)
}

hcup_sources <- list(
  icd9_procedure_classes = list(
    url = "https://hcup-us.ahrq.gov/toolssoftware/procedure/pc2015.csv",
    output = file.path(external_data, "hcup_procedure_classes_icd9.csv")
  ),
  icd10_procedure_classes = list(
    version = "v2025.1",
    url = "https://hcup-us.ahrq.gov/toolssoftware/procedureicd10/PClassR_v2025-1.zip",
    member_pattern = "PClassR_v.*\\.csv$",
    output = file.path(external_data, "hcup_procedure_classes_icd10.csv")
  ),
  icd9_clinical_domains = list(
    url = "https://www.hcup-us.ahrq.gov/toolssoftware/ccs/Multi_Level_CCS_2015.zip",
    member_pattern = "ccs_multi_pr_tool_2015\\.csv$",
    output = file.path(external_data, "hcup_clinical_domains_icd9.csv")
  ),
  icd10_clinical_domains = list(
    version = "v2025.1",
    url = "https://hcup-us.ahrq.gov/toolssoftware/ccsr/PRCCSR_v2025-1.zip",
    member_pattern = "PRCCSR.*\\.csv$",
    output = file.path(external_data, "hcup_clinical_domains_icd10.csv")
  )
)

download_file(
  hcup_sources$icd9_procedure_classes$url,
  hcup_sources$icd9_procedure_classes$output,
  overwrite = overwrite
)

download_zip_member(
  hcup_sources$icd10_procedure_classes$url,
  hcup_sources$icd10_procedure_classes$member_pattern,
  hcup_sources$icd10_procedure_classes$output,
  overwrite = overwrite
)

download_zip_member(
  hcup_sources$icd9_clinical_domains$url,
  hcup_sources$icd9_clinical_domains$member_pattern,
  hcup_sources$icd9_clinical_domains$output,
  overwrite = overwrite
)

download_zip_member(
  hcup_sources$icd10_clinical_domains$url,
  hcup_sources$icd10_clinical_domains$member_pattern,
  hcup_sources$icd10_clinical_domains$output,
  overwrite = overwrite
)

message("HCUP source file download complete.")
