# ============================================================
# 01_load_packages.R
# Load required R packages for replication project
# ============================================================

# This script installs (if necessary) and loads all packages
# used in the replication and extension analyses.
# It should be run once at the start of a session.

# ---- Required packages ----
packages <- c(
  "haven",      # read Stata (.dta) files
  "dplyr",      # data manipulation
  "tidyr",      # data reshaping
  "stringr",    # string handling
  "ggplot2",    # plots
  "readxl",     # read Excel files (ParlGov, author materials)
  "lme4",       # multilevel models (if used)
  "broom",      # tidy model outputs
  "modelsummary" # regression tables
)

# ---- Install missing packages ----
installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

# ---- Load packages ----
lapply(packages, library, character.only = TRUE)

# ---- Clean workspace message ----
cat("All required packages loaded successfully.\n")
