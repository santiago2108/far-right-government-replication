# ============================================================
# 03_harmonize_ess.R
#
# PURPOSE: Take the raw combined ESS data and create all the
# individual-level variables needed for the analysis.
#
# Input:  data/intermediate/ess_raw_combined.rds
# Output: data/intermediate/ess_harmonized.rds
#
# This script follows the variable construction in the authors'
# SPSS syntax "(0) ESS1-8_General-independent-individual-variables.sps"
# as closely as possible.
# ============================================================

library(dplyr)   # for data manipulation (mutate, filter, select, etc.)
library(haven)   # for zap_labels() — strips SPSS value labels from numbers


# ---- File paths -------------------------------------------------------

RAW_PATH <- "data/intermediate/ess_raw_combined.rds"
OUT_PATH <- "data/intermediate/ess_harmonized.rds"


# ---- Helper functions -------------------------------------------------

# SPSS's MEAN() calculates the mean of available items per row.
# It returns NA only when ALL items are missing.
# R's rowMeans() does the same when na.rm = TRUE, but returns NaN
# (not NA) when all values are missing — so it fixes that.
row_mean_spss <- function(mat) {
  out <- rowMeans(mat, na.rm = TRUE)  # mean ignoring NAs
  out[is.nan(out)] <- NA_real_        # replace NaN with proper NA
  out
}

# Reverse a 0-10 scale to 10-0 (used for anti-immigration and bad economy).
# SPSS: recode x (0=10)(1=9)(2=8)...(10=0)(else=sysmis)
# In R: new value = 10 - old value. Values outside 0-10 become NA.
reverse_0_10 <- function(x) {
  x <- as.numeric(zap_labels(x))  # strip SPSS labels, convert to number
  ifelse(x >= 0 & x <= 10, 10 - x, NA_real_)
}

# Recode authoritarian items from 1-6 scale to 5-0 scale.
# SPSS: recode x (1=5)(2=4)(3=3)(4=2)(5=1)(6=0)(else=sysmis)
# In R: new value = 6 - old value. Values outside 1-6 become NA.
recode_auth_item <- function(x) {
  x <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok <- x %in% 1:6
  out[ok] <- 6 - x[ok]
  out
}

# Reverse a 1-5 scale to 4-0 (used for redistribution / gincdif).
# SPSS: RECODE gincdif (1=4)(2=3)(3=2)(4=1)(5=0)(ELSE=SYSMIS) into redistribute.
recode_1_5_to_4_0 <- function(x) {
  x <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok <- x %in% 1:5
  out[ok] <- 5 - x[ok]
  out
}


# ---- Load data --------------------------------------------------------

ess <- readRDS(RAW_PATH)

cat("Loaded ESS data:", nrow(ess), "rows,", ncol(ess), "columns\n")
cat("ESS rounds present:", sort(unique(ess$essround)), "\n")
cat("Countries present:", sort(unique(as.character(ess$cntry))), "\n\n")


# ---- Step 1: Keep only the 22 countries used in the paper -------------
# The paper uses a specific set of European countries (see Table 1 in paper).

countries_used <- c(
  "AT", "BE", "BG", "CH", "CZ", "DE", "DK", "EE", "FI", "FR", "GB",
  "GR", "HR", "HU", "IT", "LT", "NL", "NO", "PL", "SE", "SI", "SK"
)

# Make sure cntry is a plain character vector before filtering
ess <- ess %>%
  mutate(cntry = as.character(cntry)) %>%
  filter(cntry %in% countries_used)

cat("After country filter:", nrow(ess), "rows\n")
cat("Countries kept:", sort(unique(ess$cntry)), "\n")
cat("N countries kept:", length(unique(ess$cntry)), "\n\n")
cat("=== Rows per country and ESS round (after filter) ===\n")
print(
  ess %>%
    count(cntry, essround, name = "n") %>%
    arrange(cntry, essround),
  n = Inf
)
cat("\n")


# ---- Step 2: Check all required raw variables exist -------------------
# This will stop the script early with a clear message if something
# is missing from the raw data.

required_vars <- c(
  # Political trust
  "trstprl", "trstplt", "trstprt",
  # Anti-immigration
  "imbgeco", "imueclt", "imwbcnt",
  # Authoritarian sentiments
  "ipfrule", "ipstrgv", "ipbhprp", "imptrad", "impsafe",
  # Economic attitudes
  "gincdif", "stfeco",
  # Demographics
  "agea", "gndr", "hincfel", "mnactic", "rlgdgr",
  "polintr", "domicil",
  # Education
  "edulvl",
  # Left-right self-placement (used as control later)
  "lrscale",
  # Interview date components (primary: start-of-interview)
  "inwyr", "inwmm", "inwdd"
)

missing_vars <- setdiff(required_vars, names(ess))

if (length(missing_vars) > 0) {
  stop(
    "The following variables are missing from the raw data:\n",
    paste(missing_vars, collapse = ", "),
    "\nCheck your 02_import_ess.R script to make sure these are imported."
  )
}

cat("All required variables found.\n\n")


# ---- Step 3: Build interview date (YYYYMMDD integer) ------------------
#
# The SPSS syntax uses a THREE-STEP fallback to handle missing dates:
#
#   Step A: Use inwyr/inwmm/inwdd (start-of-interview date, all rounds)
#           Already harmonized in script 02 from ESS1-2 (inwyr) and
#           ESS3-8 (inwyys) into a single set of columns.
#
#   Step B: If still missing, try inwyye/inwmme/inwdde
#           (end-of-interview date — present in some ESS rounds).
#
#   Step C: If STILL missing, impute the MINIMUM interview date for
#           that country × ESS round combination.
#           The SPSS comment says: "N=63 cases; concentrated in BE, GB, SI.
#           In these country-rounds the government did NOT change between
#           far-right opposition and inclusion during the ESS fieldwork."
#           This means the cabinet assignment is the same regardless of
#           the exact date, so using the round minimum is safe.
#
# After all three steps, interviewdate should have zero (or near-zero)
# missingness.

ess <- ess %>%
  mutate(
    # Convert all date parts to plain numbers
    inwyr = as.numeric(zap_labels(inwyr)),
    inwmm = as.numeric(zap_labels(inwmm)),
    inwdd = as.numeric(zap_labels(inwdd)),
    
    # Step A: Build date from start-of-interview (primary source)
    interviewdate = ifelse(
      !is.na(inwyr) & !is.na(inwmm) & !is.na(inwdd),
      inwyr * 10000 + inwmm * 100 + inwdd,
      NA_real_
    )
  )

# Step B: Fill remaining NAs using end-of-interview date (inwyye/inwmme/inwdde)
# Check if these columns may not exist in all ESS rounds.
if (all(c("inwyye", "inwmme", "inwdde") %in% names(ess))) {
  ess <- ess %>%
    mutate(
      inwyye = as.numeric(zap_labels(inwyye)),
      inwmme = as.numeric(zap_labels(inwmme)),
      inwdde = as.numeric(zap_labels(inwdde)),
      # Build end-of-interview date
      interviewdate_end = ifelse(
        !is.na(inwyye) & !is.na(inwmme) & !is.na(inwdde),
        inwyye * 10000 + inwmme * 100 + inwdde,
        NA_real_
      ),
      # Use end date only where start date is missing
      interviewdate = ifelse(
        is.na(interviewdate) & !is.na(interviewdate_end),
        interviewdate_end,
        interviewdate
      )
    ) %>%
    # Drop the helper columns — we no longer need them
    select(-inwyye, -inwmme, -inwdde, -interviewdate_end)
  
  cat("Step B (end-of-interview fallback) applied.\n")
} else {
  cat("Step B skipped: inwyye/inwmme/inwdde columns not found in data.\n")
  cat("  (This is expected if 02_import_ess.R did not import them.)\n")
}

# Step C: Impute the minimum interview date within each country × ESS round
# for respondents whose date is still missing after steps A and B.
#
# group_by() + mutate() + min() calculates the group minimum while
# keeping all rows (unlike summarise(), which collapses to one row per group).
# na.rm = TRUE means "ignore NAs when finding the minimum."
#
# The imputed value is only used when interviewdate is still NA.
n_missing_before_c <- sum(is.na(ess$interviewdate))
cat("Missing interview dates before Step C:", n_missing_before_c, "\n")

ess <- ess %>%
  group_by(cntry, essround) %>%
  mutate(
    # Calculate the earliest date seen in this country × round.
    # IMPORTANT: if every respondent in a group has NA (e.g. Estonia Round 5),
    # min(..., na.rm = TRUE) returns Inf instead of NA. We fix this below.
    interviewdate_min = min(interviewdate, na.rm = TRUE),
    # Replace NA with the group minimum
    interviewdate = ifelse(
      is.na(interviewdate),
      interviewdate_min,
      interviewdate
    )
  ) %>%
  ungroup() %>%
  select(-interviewdate_min) %>%  # clean up helper column
  # Fix: if min() returned Inf (whole group was NA), convert back to NA.
  # is.infinite() catches Inf; those rows had no date at all and should
  # stay NA rather than get a nonsense Inf value.
  mutate(interviewdate = ifelse(is.infinite(interviewdate), NA_real_, interviewdate))

n_missing_after_c <- sum(is.na(ess$interviewdate))
cat("Missing interview dates after  Step C:", n_missing_after_c, "\n")
cat("(Any remaining NAs are from country-rounds where NO respondent had a date,\n")
cat(" so no group minimum could be computed. Cabinet assignment for these\n")
cat(" respondents will be handled by script 04's fallback logic.)\n\n")


# ---- Step 4: Education variable (educ, 0-4) ---------------------------

ess <- ess %>%
  mutate(
    edulvl_num = as.numeric(zap_labels(edulvl)),
    edulvlbR = case_when(
      edulvl_num %in% c(0, 113)                            ~ 1,
      edulvl_num %in% c(129, 212, 213, 221, 222, 223)      ~ 2,
      edulvl_num %in% c(229, 311, 312, 313, 321, 322, 323) ~ 3,
      edulvl_num %in% c(412, 413, 421, 422, 423)           ~ 4,
      edulvl_num %in% c(510, 520, 610, 620, 710, 720, 800) ~ 5,
      TRUE ~ NA_real_
    ),
    edusc5 = case_when(
      !is.na(edulvlbR) ~ edulvlbR,
      TRUE             ~ edulvl_num
    ),
    edusc5 = ifelse(edusc5 == 55, NA_real_, edusc5),
    educ = case_when(
      edusc5 == 1 ~ 0,
      edusc5 == 2 ~ 1,
      edusc5 == 3 ~ 2,
      edusc5 == 4 ~ 3,
      edusc5 == 5 ~ 4,
      TRUE        ~ NA_real_
    )
  )


# ---- Step 5: Build all other variables --------------------------------

ess <- ess %>%
  mutate(
    trust_political = row_mean_spss(cbind(
      as.numeric(zap_labels(trstprl)),
      as.numeric(zap_labels(trstplt)),
      as.numeric(zap_labels(trstprt))
    )),
    anti_immigration = row_mean_spss(cbind(
      reverse_0_10(imbgeco),
      reverse_0_10(imueclt),
      reverse_0_10(imwbcnt)
    )),
    authoritarian = row_mean_spss(cbind(
      recode_auth_item(ipfrule),
      recode_auth_item(ipstrgv),
      recode_auth_item(ipbhprp),
      recode_auth_item(imptrad),
      recode_auth_item(impsafe)
    )),
    redistribution = recode_1_5_to_4_0(gincdif),
    bad_economy = reverse_0_10(stfeco),
    
    age = {
      a <- as.numeric(zap_labels(agea))
      ifelse(a >= 18 & a <= 102, a, NA_real_)
    },
    female = case_when(
      as.numeric(zap_labels(gndr)) == 1 ~ 0,
      as.numeric(zap_labels(gndr)) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    subincome = case_when(
      as.numeric(zap_labels(hincfel)) == 1 ~ 3,
      as.numeric(zap_labels(hincfel)) == 2 ~ 2,
      as.numeric(zap_labels(hincfel)) == 3 ~ 1,
      as.numeric(zap_labels(hincfel)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    mnactic_num = as.numeric(zap_labels(mnactic)),
    unemployed = case_when(
      mnactic_num %in% c(3, 4)                    ~ 1,
      mnactic_num %in% c(1, 2, 5, 6, 7, 8, 9)     ~ 0,
      TRUE ~ NA_real_
    ),
    religiosity = {
      r <- as.numeric(zap_labels(rlgdgr))
      ifelse(r >= 0 & r <= 10, r, NA_real_)
    },
    polinterest = case_when(
      as.numeric(zap_labels(polintr)) == 1 ~ 3,
      as.numeric(zap_labels(polintr)) == 2 ~ 2,
      as.numeric(zap_labels(polintr)) == 3 ~ 1,
      as.numeric(zap_labels(polintr)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    urban = case_when(
      as.numeric(zap_labels(domicil)) %in% 1:3 ~ 1,
      as.numeric(zap_labels(domicil)) %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    ),
    lrscale = as.numeric(zap_labels(lrscale))
  )


# ---- Step 6: Select final columns for the harmonized dataset ----------

ess_harmonized <- ess %>%
  select(
    idno, cntry, essround, interviewdate,
    trust_political, anti_immigration, authoritarian, redistribution, bad_economy,
    age, female, educ, subincome, unemployed, religiosity, polinterest, urban,
    lrscale
  )


# ---- Step 7: Save output ----------------------------------------------

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_harmonized, OUT_PATH)

cat("Saved harmonized dataset to:", OUT_PATH, "\n")
cat("Rows:", nrow(ess_harmonized), "| Columns:", ncol(ess_harmonized), "\n\n")


# ---- Step 8: Sanity checks --------------------------------------------

cat("=== Missing interview date by ESS round ===\n")
print(
  ess_harmonized %>%
    group_by(essround) %>%
    summarise(pct_missing = round(mean(is.na(interviewdate)) * 100, 1))
)
# After the three-step fallback, pct_missing should be 0% for all rounds.
# The original SPSS had ~230 initially missing, reduced to 63 after step B,
# and those 63 were imputed in step C. So 0% is the expected result.

cat("\n=== Education (educ) distribution: 0 = lowest, 4 = highest ===\n")
print(table(ess_harmonized$educ, useNA = "ifany"))

cat("\n=== Missing values for key variables (%) ===\n")
print(
  ess_harmonized %>%
    summarise(
      trust_political  = round(mean(is.na(trust_political))  * 100, 1),
      anti_immigration = round(mean(is.na(anti_immigration)) * 100, 1),
      authoritarian    = round(mean(is.na(authoritarian))    * 100, 1),
      redistribution   = round(mean(is.na(redistribution))   * 100, 1),
      bad_economy      = round(mean(is.na(bad_economy))      * 100, 1),
      educ             = round(mean(is.na(educ))             * 100, 1),
      subincome        = round(mean(is.na(subincome))        * 100, 1),
      unemployed       = round(mean(is.na(unemployed))       * 100, 1)
    )
)

cat("\n=== Rows per country ===\n")
print(
  ess_harmonized %>%
    count(cntry, name = "n_respondents") %>%
    arrange(cntry)
)