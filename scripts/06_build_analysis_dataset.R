# ============================================================
# 06_build_analysis_dataset.R
#
# PURPOSE:
#   Take the dataset built by script 05 (ess_with_dv.rds), which already
#   contains all individual-level variables, cabinet context, and the
#   far-right vote DV, and turn it into a clean, analysis-ready file.
#
# What this script does:
#   1. Load ess_with_dv.rds
#   2. Keep only the variables needed for analysis
#   3. Remove rows that are missing on the outcome (fr_vote)
#   4. Print a summary table: how many obs, how much missing per variable
#   5. Save the final dataset
#
# Input:  data/intermediate/ess_with_dv.rds
# Output: data/final/analysis_dataset.rds
# ============================================================

library(dplyr)

# ---- File paths ----
IN_PATH  <- "data/intermediate/ess_with_dv.rds"
OUT_PATH <- "data/final/analysis_dataset.rds"

# ---- Step 1: Load the data --------------------------------------------
# ess_with_dv already contains everything built by scripts 03, 04, 05:
#   - individual-level indices (trust, immigration, authoritarianism, ...)
#   - controls (age, female, educ, ...)
#   - cabinet context (cabinetid, farrightpower)
#   - far-right vote DV (fr_vote)

ess <- readRDS(IN_PATH)

# ---- Step 2: Keep only the variables needed for analysis ----

analysis <- ess|>
  select(
    
    # Identifiers (needed to know who is who)
    idno, cntry, essround,
    
    # Country identifiers from ParlGov
    country_name_short,
    country_id,       # numeric ParlGov ID — needed for the period formula below
    
    # Cabinet context
    cabinetid,        # which cabinet was in power at interview time
    
    # Outcome variable: did the respondent vote for a far-right party?
    fr_vote,
    
    # Key contextual variable: was a far-right party in government?
    farrightpower,
    
    # --- Individual-level predictors ---
    trust_political,
    anti_immigration,
    authoritarian,
    redistribution,
    bad_economy,
    
    # --- Control variables ---
    age,
    female,
    educ,
    subincome,
    unemployed,
    religiosity,
    polinterest,
    urban
  )

#cat("\nAfter selecting variables:  N =", nrow(analysis), " | variables:", ncol(analysis), "\n")


# ---- Step 3: Apply the paper's selectperiod filter ----
#
# The paper defines a "period" as a unique combination of:
#   ESS round × country × cabinet
#
# It only keeps periods that contain at least one far-right voter.
# This removes four periods where no one voted far-right
# (so a model cannot distinguish 0s from 1s within those periods).
#
# The period formula creates a unique number per combination:
#   period = (essround * 10,000,000) + (country_id * 10,000) + cabinetid
#
# country_id (the ParlGov numeric ID, unique per country) is used to ensure
# no two countries accidentally produce the same period number.

analysis <- analysis|>
  mutate(period = essround * 10000000 + country_id * 10000 + cabinetid)

# Find which periods have at least one far-right voter
period_has_frvote <- analysis|>
  filter(!is.na(fr_vote))|>
  group_by(period)|>
  summarise(any_fr = any(fr_vote == 1, na.rm = TRUE))|>
  filter(any_fr)

#cat("\nPeriods with at least one far-right voter:", nrow(period_has_frvote), "\n")

before <- nrow(analysis)
analysis <- analysis|>
  filter(period %in% period_has_frvote$period)
after  <- nrow(analysis)

#cat("Rows dropped by selectperiod filter:", before - after, "\n")
#cat("Rows remaining:", after, "\n")
#cat("Countries remaining:", length(unique(analysis$cntry)), "\n")


# ---- Step 4: Standardize the independent variables -------------------
#
# The paper states: "The independent variables are standardized
# (mean is 0 and standard deviation is 1)."
#
# scale() does this in one step: it subtracts the mean and divides by
# the standard deviation. We wrap it in as.numeric() because scale()
# returns a matrix by default, and we want a plain numeric column.
#
# New columns are created with a _z suffix so the raw originals are kept.
# Only substantive predictors and controls are standardized —
# NOT the outcome (fr_vote), NOT binary context variables (farrightpower),
# and NOT the grouping variables (period, cntry, etc.).

analysis <- analysis|>
  mutate(
    trust_political_z  = as.numeric(scale(trust_political)),
    anti_immigration_z = as.numeric(scale(anti_immigration)),
    authoritarian_z    = as.numeric(scale(authoritarian)),
    redistribution_z   = as.numeric(scale(redistribution)),
    bad_economy_z      = as.numeric(scale(bad_economy)),
    age_z              = as.numeric(scale(age)),
    female_z           = as.numeric(scale(female)),
    educ_z             = as.numeric(scale(educ)),
    subincome_z        = as.numeric(scale(subincome)),
    unemployed_z       = as.numeric(scale(unemployed)),
    religiosity_z      = as.numeric(scale(religiosity)),
    polinterest_z      = as.numeric(scale(polinterest)),
    urban_z            = as.numeric(scale(urban))
  )

# Quick check: mean of a standardized variable should be ~0, SD ~1
#cat("\n--- Standardization check (trust_political_z) ---\n")
#cat("Mean:", round(mean(analysis$trust_political_z, na.rm = TRUE), 4),
#    " (should be ~0)\n")
#cat("SD:  ", round(sd(analysis$trust_political_z,   na.rm = TRUE), 4),
#    " (should be ~1)\n")


# ---- Step 5: Drop rows missing on the outcome variable ----------------
# A respondent cannot be used in the regression if their vote is unknown.

before <- nrow(analysis)
analysis <- analysis|> filter(!is.na(fr_vote))
after  <- nrow(analysis)

#cat("\nRows dropped because fr_vote is missing:", before - after, "\n")
#cat("Rows remaining for analysis:", after, "\n")


# ---- Step 6: Checks --------------------------------------------

# 4a: Distribution of the outcome
#cat("\n--- fr_vote (0 = other vote, 1 = far-right vote) ---\n")
#print(table(analysis$fr_vote, useNA = "ifany"))

# 4b: Distribution of the key contextual variable
#cat("\n--- farrightpower (0 = not in gov, 1 = in gov) ---\n")
#print(table(analysis$farrightpower, useNA = "ifany"))

# 4c: How many respondents per country?
#cat("\n--- Number of respondents per country ---\n")
#print(
#  analysis|>
#    count(cntry, name = "n_respondents")|>
#    arrange(cntry)
#)

# 4d: Missingness table for all key variables
# The % of missing values is calculated for each variable.
# Good practice: check this before running any model.
#cat("\n--- % missing per variable ---\n")

#miss_table <- analysis|>
#  summarise(
#    across(
#      everything(),
#      ~ round(mean(is.na(.)) * 100, 1)  # % missing, rounded to 1 decimal
#    )
#  )|>
#  tidyr::pivot_longer(
#    cols      = everything(),
#    names_to  = "variable",
#    values_to = "pct_missing"
#  )|>
#  arrange(desc(pct_missing))

#print(miss_table, n = Inf)


# ---- Step 7: Save -----------------------------------------------------
saveRDS(analysis, OUT_PATH)

#cat("Final dataset:  N =", nrow(analysis), " | variables:", ncol(analysis), "\n")