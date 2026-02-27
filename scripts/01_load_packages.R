## ============================================================
# 01_load_packages.R
# Load required R packages for the project
# ============================================================

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

installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  install.packages(packages[!installed])
}

lapply(packages, library, character.only = TRUE)
