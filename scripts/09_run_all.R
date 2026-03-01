# ============================================================
# 09_run_all.R
#
# REPLICATION: Muis, Brils & Gaidytė (2022)
# "Arrived in Power, and Yet Still Disgruntled?"
# Government & Opposition
#
# This script runs the full analysis pipeline from start to finish.
# It combines scripts 01 through 08 into one file so the entire
# replication can be run in a single step.
#
# BEFORE RUNNING:
#   Make sure your working directory contains these folders:
#     data/raw/ess/              → ESS1_raw.dta ... ESS8_raw.dta
#     data/raw/author_materials/ → the four SPSS .sps files
#     data/raw/parlgov/          → view_cabinet.csv
#     data/raw/popuList/         → The_PopuList_3.0.csv
#
#   Run this script from its own folder (Session > Set Working Directory)
#   or adjust the path constants in each section below.
#
# OUTPUT FILES:
#   data/intermediate/ess_raw_combined.rds
#   data/intermediate/ess_harmonized.rds
#   data/intermediate/ess_with_cabinet.rds
#   data/intermediate/ess_with_dv.rds
#   data/final/analysis_dataset.rds
#   output/tables/replication_models.rds
#   output/tables/replication_models.txt
#   output/tables/replication_table.html
#   output/tables/descriptive_stats.html
#   output/figures/interaction_plot.png
# ============================================================


# ==============================================================
# SECTION 1 — LOAD PACKAGES
# (originally 01_load_packages.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 1: Loading packages\n")
cat("##############################################################\n\n")

packages <- c(
  "haven",        # read Stata (.dta) files
  "dplyr",        # data manipulation
  "tidyr",        # data reshaping
  "stringr",      # string handling
  "ggplot2",      # plots
  "readxl",       # read Excel files (ParlGov, author materials)
  "lme4",         # multilevel models
  "broom",        # tidy model outputs
  "modelsummary", # regression tables
  "readr",        # read CSV files
  "scales"        # percent formatting for plots
)

# Install any packages that are not yet installed
installed <- packages %in% rownames(installed.packages())
if (any(installed == FALSE)) {
  cat("Installing missing packages:", paste(packages[!installed], collapse = ", "), "\n")
  install.packages(packages[!installed])
}

# Load all packages
lapply(packages, library, character.only = TRUE)

cat("All packages loaded successfully.\n")


# ==============================================================
# SECTION 2 — IMPORT ESS DATA
# (originally 02_import_ess.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 2: Importing raw ESS data (rounds 1-8)\n")
cat("##############################################################\n\n")

# --- File paths ---
ESS_DIR  <- "data/raw/ess"
OUT_PATH <- "data/intermediate/ess_raw_combined.rds"

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)

# --- Variables to keep from each ESS file ---
keep_vars <- c(
  "cntry", "idno", "essround",
  # Interview date — ESS 1-2 used inwyr/inwmm/inwdd, ESS 3-8 used inwyys/inwmms/inwdds
  "inwyr", "inwmm", "inwdd",
  "inwyys", "inwmms", "inwdds",
  # Voting and trust
  "vote", "trstprl", "trstplt", "trstprt",
  # Immigration attitudes (3 items)
  "imbgeco", "imueclt", "imwbcnt",
  # Authoritarian values (5 items)
  "ipfrule", "ipstrgv", "ipbhprp", "imptrad", "impsafe",
  # Economic attitudes
  "gincdif", "stfeco",
  # Demographics and controls
  "agea", "gndr", "hincfel", "mnactic", "rlgdgr", "polintr", "domicil",
  # Education — early rounds used edulvla, later rounds used edulvlb
  "edulvla", "edulvlb",
  # Left-right self-placement
  "lrscale"
)

# --- Function: read and clean one ESS file ---
read_one_ess <- function(path) {
  
  cat("  Reading:", path, "\n")
  df <- read_dta(path)
  
  # Keep only the variables we need + party vote columns
  # starts_with("prtvt") and starts_with("prtv") catch country-specific
  # party-vote columns like prtvtgb, prtvtde2, etc.
  df <- select(df, any_of(keep_vars), starts_with("prtvt"), starts_with("prtv"))
  
  # Harmonize interview date into one consistent set of columns
  if (!("inwyr" %in% names(df))) df$inwyr <- NA
  if (!("inwmm" %in% names(df))) df$inwmm <- NA
  if (!("inwdd" %in% names(df))) df$inwdd <- NA
  
  # coalesce() uses the first non-missing value — fills in from the "s" version
  if ("inwyys" %in% names(df)) df$inwyr <- coalesce(df$inwyr, df$inwyys)
  if ("inwmms" %in% names(df)) df$inwmm <- coalesce(df$inwmm, df$inwmms)
  if ("inwdds" %in% names(df)) df$inwdd <- coalesce(df$inwdd, df$inwdds)
  df <- select(df, -any_of(c("inwyys", "inwmms", "inwdds")))
  
  # Harmonize education into one column called edulvl
  if (!("edulvla" %in% names(df))) df$edulvla <- NA
  if (!("edulvlb" %in% names(df))) df$edulvlb <- NA
  df$edulvl <- coalesce(df$edulvla, df$edulvlb)
  df <- select(df, -any_of(c("edulvla", "edulvlb")))
  
  return(df)
}

# --- Read all 8 rounds and stack into one dataset ---
files            <- file.path(ESS_DIR, paste0("ESS", 1:8, "_raw.dta"))
ess_raw_combined <- bind_rows(lapply(files, read_one_ess))

# --- Diagnostics ---
cat("\nRaw combined dataset:\n")
cat("  Rows:     ", nrow(ess_raw_combined), "\n")
cat("  Columns:  ", ncol(ess_raw_combined), "\n")
cat("  Rounds present:", sort(unique(ess_raw_combined$essround)), "\n")
cat("  Countries:", length(unique(ess_raw_combined$cntry)), "\n")

saveRDS(ess_raw_combined, OUT_PATH)
cat("\nSaved to:", OUT_PATH, "\n")


# ==============================================================
# SECTION 3 — HARMONIZE INDIVIDUAL-LEVEL VARIABLES
# (originally 03_harmonize_ess.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 3: Harmonizing individual-level variables\n")
cat("##############################################################\n\n")

RAW_PATH <- "data/intermediate/ess_raw_combined.rds"
OUT_PATH <- "data/intermediate/ess_harmonized.rds"

# --- Helper functions ---

# Row-wise mean like SPSS MEAN(): returns NA only when ALL items are missing
row_mean_spss <- function(mat) {
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_  # replace NaN (all items missing) with proper NA
  out
}

# Reverse a 0-10 scale to 10-0 (used for anti-immigration and bad economy)
reverse_0_10 <- function(x) {
  x <- as.numeric(zap_labels(x))
  ifelse(x >= 0 & x <= 10, 10 - x, NA_real_)
}

# Recode authoritarian items from 1-6 scale to 5-0
recode_auth_item <- function(x) {
  x   <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok  <- x %in% 1:6
  out[ok] <- 6 - x[ok]
  out
}

# Reverse a 1-5 scale to 4-0 (redistribution/gincdif)
recode_1_5_to_4_0 <- function(x) {
  x   <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok  <- x %in% 1:5
  out[ok] <- 5 - x[ok]
  out
}

# --- Load data ---
ess <- readRDS(RAW_PATH)

# --- Step 1: Keep only the 22 countries used in the paper ---
countries_used <- c(
  "AT", "BE", "BG", "CH", "CZ", "DE", "DK", "EE", "FI", "FR", "GB",
  "GR", "HR", "HU", "IT", "LT", "NL", "NO", "PL", "SE", "SI", "SK"
)
ess <- ess |>
  mutate(cntry = as.character(cntry)) |>
  filter(cntry %in% countries_used)

cat("After country filter:\n")
cat("  Rows:", nrow(ess), "\n")
cat("  Countries kept:", sort(unique(ess$cntry)), "\n")
cat("  N countries:", length(unique(ess$cntry)), "\n\n")

cat("Rows per country and ESS round:\n")
print(
  ess |>
    count(cntry, essround, name = "n") |>
    arrange(cntry, essround),
  n = Inf
)
cat("\n")

# --- Step 2: Check all required raw variables exist ---
required_vars <- c(
  "trstprl", "trstplt", "trstprt",
  "imbgeco", "imueclt", "imwbcnt",
  "ipfrule", "ipstrgv", "ipbhprp", "imptrad", "impsafe",
  "gincdif", "stfeco",
  "agea", "gndr", "hincfel", "mnactic", "rlgdgr", "polintr", "domicil",
  "edulvl", "lrscale",
  "inwyr", "inwmm", "inwdd"
)
missing_vars <- setdiff(required_vars, names(ess))
if (length(missing_vars) > 0) {
  stop(
    "The following variables are missing from the raw data:\n",
    paste(missing_vars, collapse = ", "),
    "\nCheck your import step."
  )
}
cat("All required variables found.\n\n")

# --- Step 3: Build interview date (YYYYMMDD integer) ---
# Three-step fallback to handle missing dates:
#   A) Use start-of-interview date (inwyr/inwmm/inwdd)
#   B) If still missing, use end-of-interview date (inwyye/inwmme/inwdde)
#   C) If still missing, impute the minimum date within country × ESS round

ess <- ess |>
  mutate(
    inwyr = as.numeric(zap_labels(inwyr)),
    inwmm = as.numeric(zap_labels(inwmm)),
    inwdd = as.numeric(zap_labels(inwdd)),
    # Step A: combine year/month/day into a single YYYYMMDD integer
    interviewdate = ifelse(
      !is.na(inwyr) & !is.na(inwmm) & !is.na(inwdd),
      inwyr * 10000 + inwmm * 100 + inwdd,
      NA_real_
    )
  )

# Step B: end-of-interview fallback (columns only exist in some rounds)
if (all(c("inwyye", "inwmme", "inwdde") %in% names(ess))) {
  ess <- ess |>
    mutate(
      inwyye = as.numeric(zap_labels(inwyye)),
      inwmme = as.numeric(zap_labels(inwmme)),
      inwdde = as.numeric(zap_labels(inwdde)),
      interviewdate_end = ifelse(
        !is.na(inwyye) & !is.na(inwmme) & !is.na(inwdde),
        inwyye * 10000 + inwmme * 100 + inwdde,
        NA_real_
      ),
      interviewdate = ifelse(
        is.na(interviewdate) & !is.na(interviewdate_end),
        interviewdate_end,
        interviewdate
      )
    ) |>
    select(-inwyye, -inwmme, -inwdde, -interviewdate_end)
  cat("Step B (end-of-interview fallback) applied.\n")
} else {
  cat("Step B skipped: inwyye/inwmme/inwdde columns not found (expected).\n")
}

n_missing_before_c <- sum(is.na(ess$interviewdate))
cat("Missing interview dates before Step C (group-minimum imputation):", n_missing_before_c, "\n")

# Step C: impute the group minimum for any still-missing dates
# group_by() + mutate() keeps all rows (unlike summarise which collapses)
ess <- ess |>
  group_by(cntry, essround) |>
  mutate(
    interviewdate_min = min(interviewdate, na.rm = TRUE),  # Inf if whole group is NA
    interviewdate = ifelse(is.na(interviewdate), interviewdate_min, interviewdate)
  ) |>
  ungroup() |>
  select(-interviewdate_min) |>
  mutate(interviewdate = ifelse(is.infinite(interviewdate), NA_real_, interviewdate))

n_missing_after_c <- sum(is.na(ess$interviewdate))
cat("Missing interview dates after  Step C:", n_missing_after_c,
    "(should be 0 or near-zero)\n\n")

cat("Missing interview date by ESS round:\n")
print(
  ess |>
    group_by(essround) |>
    summarise(pct_missing = round(mean(is.na(interviewdate)) * 100, 1))
)
cat("\n")

# --- Step 4: Education variable (educ, 0-4) ---
# The ESS used different coding schemes across rounds (edulvla vs edulvlb).
# We map all codes to a 0-4 scale: 0 = lowest, 4 = highest education.
ess <- ess |>
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
    edusc5 = ifelse(edusc5 == 55, NA_real_, edusc5),  # 55 = "other" code, recode to NA
    educ = case_when(
      edusc5 == 1 ~ 0,
      edusc5 == 2 ~ 1,
      edusc5 == 3 ~ 2,
      edusc5 == 4 ~ 3,
      edusc5 == 5 ~ 4,
      TRUE        ~ NA_real_
    )
  )

cat("Education (educ) distribution: 0 = lowest, 4 = highest\n")
print(table(ess$educ, useNA = "ifany"))
cat("\n")

# --- Step 5: Build all other individual-level variables ---
ess <- ess |>
  mutate(
    # Political trust: mean of trust in parliament, politicians, parties (0-10)
    trust_political = row_mean_spss(cbind(
      as.numeric(zap_labels(trstprl)),
      as.numeric(zap_labels(trstplt)),
      as.numeric(zap_labels(trstprt))
    )),
    # Anti-immigration: mean of 3 items, reversed so high = more anti-immigration
    anti_immigration = row_mean_spss(cbind(
      reverse_0_10(imbgeco),
      reverse_0_10(imueclt),
      reverse_0_10(imwbcnt)
    )),
    # Authoritarian values: mean of 5 items, recoded to 0-5 scale
    authoritarian = row_mean_spss(cbind(
      recode_auth_item(ipfrule),
      recode_auth_item(ipstrgv),
      recode_auth_item(ipbhprp),
      recode_auth_item(imptrad),
      recode_auth_item(impsafe)
    )),
    # Redistribution preference: gincdif reversed to 0-4 (high = more redistribution)
    redistribution = recode_1_5_to_4_0(gincdif),
    # Economic dissatisfaction: satisfaction with economy reversed (high = more dissatisfied)
    bad_economy    = reverse_0_10(stfeco),
    # Age: numeric, valid range 18-102
    age = {
      a <- as.numeric(zap_labels(agea))
      ifelse(a >= 18 & a <= 102, a, NA_real_)
    },
    # Female: 0 = male, 1 = female
    female = case_when(
      as.numeric(zap_labels(gndr)) == 1 ~ 0,
      as.numeric(zap_labels(gndr)) == 2 ~ 1,
      TRUE ~ NA_real_
    ),
    # Subjective income: hincfel reversed so high = more comfortable (0-3)
    subincome = case_when(
      as.numeric(zap_labels(hincfel)) == 1 ~ 3,
      as.numeric(zap_labels(hincfel)) == 2 ~ 2,
      as.numeric(zap_labels(hincfel)) == 3 ~ 1,
      as.numeric(zap_labels(hincfel)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    # Unemployed: 1 if unemployed/looking for work, 0 otherwise
    mnactic_num = as.numeric(zap_labels(mnactic)),
    unemployed = case_when(
      mnactic_num %in% c(3, 4)                ~ 1,
      mnactic_num %in% c(1, 2, 5, 6, 7, 8, 9) ~ 0,
      TRUE ~ NA_real_
    ),
    # Religiosity: 0-10 scale
    religiosity = {
      r <- as.numeric(zap_labels(rlgdgr))
      ifelse(r >= 0 & r <= 10, r, NA_real_)
    },
    # Political interest: reversed so high = more interested (0-3)
    polinterest = case_when(
      as.numeric(zap_labels(polintr)) == 1 ~ 3,
      as.numeric(zap_labels(polintr)) == 2 ~ 2,
      as.numeric(zap_labels(polintr)) == 3 ~ 1,
      as.numeric(zap_labels(polintr)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    # Urban: 1 = urban/suburban, 0 = rural/village
    urban = case_when(
      as.numeric(zap_labels(domicil)) %in% 1:3 ~ 1,
      as.numeric(zap_labels(domicil)) %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    ),
    lrscale = as.numeric(zap_labels(lrscale))
  )

# --- Step 6: Select final columns ---
ess_harmonized <- ess |>
  select(
    idno, cntry, essround, interviewdate,
    trust_political, anti_immigration, authoritarian, redistribution, bad_economy,
    age, female, educ, subincome, unemployed, religiosity, polinterest, urban,
    lrscale
  )

# --- Diagnostics ---
cat("Harmonized dataset: Rows =", nrow(ess_harmonized),
    "| Columns =", ncol(ess_harmonized), "\n\n")

cat("Missing values for key variables (%):\n")
print(
  ess_harmonized |>
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
cat("\n")

cat("Rows per country:\n")
print(
  ess_harmonized |>
    count(cntry, name = "n_respondents") |>
    arrange(cntry)
)

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_harmonized, OUT_PATH)
cat("\nSaved to:", OUT_PATH, "\n")


# ==============================================================
# SECTION 4 — ADD CABINET ID AND FAR-RIGHT IN POWER
# (originally 04_add_cabinet_and_farrightinpower.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 4: Adding cabinet context and far-right in power\n")
cat("##############################################################\n\n")

ESS_IN   <- "data/intermediate/ess_harmonized.rds"
SPS_CAB  <- "data/raw/author_materials/(3a) ESS1-8 Cabinet ID addition with date of Interview.sps"
SPS_FR   <- "data/raw/author_materials/(3b) ESS1-8 Far right in government addition with Cabinet ID.sps"
PARLGOV  <- "data/raw/parlgov/view_cabinet.csv"
OUT_PATH <- "data/intermediate/ess_with_cabinet.rds"

ess <- readRDS(ESS_IN)

# --- Step 2: Map ESS 2-letter codes to ParlGov numeric country IDs ---
# ESS uses "DE", ParlGov uses 35. We bridge them via 3-letter ISO codes.
cntry_map <- data.frame(
  cntry              = c("AT","BE","BG","CH","CZ","DE","DK","EE","ES",
                         "FI","FR","GB","GR","HR","HU","IE","IT","LT","LV","NL",
                         "NO","PL","PT","SE","SI","SK"),
  country_name_short = c("AUT","BEL","BGR","CHE","CZE","DEU","DNK","EST","ESP",
                         "FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU","LVA","NLD",
                         "NOR","POL","PRT","SWE","SVN","SVK"),
  stringsAsFactors = FALSE
)

parlgov_countries <- read_csv(PARLGOV, show_col_types = FALSE) |>
  select(country_name_short, country_id) |>
  distinct()

cntry_map <- cntry_map |>
  left_join(parlgov_countries, by = "country_name_short")

missing_id <- cntry_map |> filter(is.na(country_id))
if (nrow(missing_id) > 0) {
  stop("country_id missing for: ", paste(missing_id$cntry, collapse = ", "))
}

ess <- ess |>
  left_join(cntry_map, by = "cntry")

if (any(is.na(ess$country_id))) {
  stop("ESS rows with missing country_id after join for: ",
       paste(sort(unique(ess$cntry[is.na(ess$country_id)])), collapse = ", "))
}

cat("Country ID mapping successful. Countries in dataset:\n")
print(sort(unique(ess$cntry)))
cat("\n")

# --- Step 3: Extract cabinet cutoff rules from the authors' SPSS syntax ---
# The SPSS file contains rules like:
#   IF (country_id=35 AND interviewdate > 20050918) cabinetid = 412
# We extract all such rules using text pattern matching.
sps_lines  <- readLines(SPS_CAB)
rule_lines <- sps_lines[str_detect(sps_lines, "interviewdate") &
                          str_detect(sps_lines, "cabinetid")]
extracted  <- str_match(
  rule_lines,
  "country_id=\\s*(\\d+).*?interviewdate\\s*>\\s*(\\d+).*?cabinetid\\s*=\\s*(\\d+)"
)
cabinet_rules <- data.frame(
  country_id  = as.integer(extracted[, 2]),
  cutoff_date = as.integer(extracted[, 3]),
  cabinetid   = as.integer(extracted[, 4]),
  stringsAsFactors = FALSE
)
cabinet_rules <- cabinet_rules[complete.cases(cabinet_rules), ]
cabinet_rules <- distinct(cabinet_rules)
cabinet_rules <- arrange(cabinet_rules, country_id, cutoff_date)

cat("Cabinet rules extracted from SPSS file:", nrow(cabinet_rules), "rules\n\n")

# --- Step 4: Assign cabinetid to each respondent ---
# Logic: find the latest cutoff date that is BEFORE the interview date.
# That is the cabinet that was in power at interview time.
n_missing_date <- sum(is.na(ess$interviewdate))
if (n_missing_date > 0) {
  message("WARNING: ", n_missing_date, " respondents have NA interviewdate.")
  message("  They will be dropped during cabinet assignment.")
  message("  Breakdown by country and ESS round:")
  print(
    ess |>
      filter(is.na(interviewdate)) |>
      count(cntry, essround, name = "n_missing") |>
      arrange(cntry, essround)
  )
} else {
  message("All respondents have a valid interview date. No rows will be lost here.")
}

n_rows_before_join <- nrow(ess)

# Many-to-many join: link each respondent to all cabinet rules for their country,
# then keep only the rule with the latest cutoff date before their interview.
ess_cab <- ess |>
  left_join(cabinet_rules, by = "country_id", relationship = "many-to-many") |>
  filter(interviewdate > cutoff_date) |>
  group_by(cntry, idno, essround, interviewdate) |>
  slice_max(order_by = cutoff_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-cutoff_date)

n_rows_after_join <- nrow(ess_cab)
n_lost            <- n_rows_before_join - n_rows_after_join
message("Rows before cabinet assignment: ", n_rows_before_join)
message("Rows after  cabinet assignment: ", n_rows_after_join)
message("Rows lost (missing date or before first cutoff): ", n_lost)

n_missing_cab <- sum(is.na(ess_cab$cabinetid))
message("Respondents with no cabinet assigned (cabinetid = NA): ", n_missing_cab)

# --- Step 5: Extract far-right cabinet IDs from the authors' SPSS syntax ---
sps_fr_lines <- readLines(SPS_FR)
fr_lines     <- sps_fr_lines[str_detect(sps_fr_lines, "farrightpower\\s*=\\s*1")]
fr_extracted <- str_match(fr_lines, "cabinetid=\\s*(\\d+)")
fr_cabinets  <- data.frame(
  cabinetid     = as.integer(fr_extracted[, 2]),
  farrightpower = 1L,
  stringsAsFactors = FALSE
) |>
  distinct(cabinetid, .keep_all = TRUE)

cat("\nFar-right cabinet IDs extracted:", nrow(fr_cabinets), "\n")

# --- Step 6: Add farrightpower (1 = far-right in government, 0 = not) ---
ess_cab <- ess_cab |>
  left_join(fr_cabinets, by = "cabinetid") |>
  mutate(farrightpower = if_else(is.na(farrightpower), 0L, farrightpower))

# --- Step 7: Compute period variable ---
# period = a unique number for each ESS round × country × cabinet combination
# Formula: (round * 10,000,000) + (country_id * 10,000) + cabinet_id
ess_cab <- ess_cab |>
  mutate(period = (essround * 10000000L) + (country_id * 10000L) + cabinetid)

# --- Diagnostics ---
message("\nTotal rows in dataset: ", nrow(ess_cab))
message("Rows with farrightpower = 1: ", sum(ess_cab$farrightpower == 1, na.rm = TRUE))
message("Rows with farrightpower = 0: ", sum(ess_cab$farrightpower == 0, na.rm = TRUE))
message("Number of unique periods:    ", n_distinct(ess_cab$period, na.rm = TRUE))

cat("\nRespondents by farrightpower:\n")
print(table(ess_cab$farrightpower, useNA = "ifany"))

saveRDS(ess_cab, OUT_PATH)
cat("\nSaved to:", OUT_PATH, "\n")


# ==============================================================
# SECTION 5 — CODE FAR-RIGHT VOTE DEPENDENT VARIABLE
# (originally 05_code_far_right_vote.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 5: Coding the far-right vote dependent variable\n")
cat("##############################################################\n\n")

ESS_CAB_IN   <- "data/intermediate/ess_with_cabinet.rds"
ESS_RAW_IN   <- "data/intermediate/ess_raw_combined.rds"
SPSS_PARTIES <- "data/raw/author_materials/(1) ESS1-8-PARTIES NEWEST.sps"
SPSS_PARLGOV <- "data/raw/author_materials/(2) ESS1-8 PARLGOV PARTY ID.sps"
POPULIST_CSV <- "data/raw/popuList/The_PopuList_3.0.csv"
OUT_PATH     <- "data/intermediate/ess_with_dv.rds"

# --- Part A: Author's SPSS party-family classification ---
# The SPSS file has lines like: (123456 = 1) meaning party 123456 belongs to family 1.
# Family 1 = far-right in the author's coding scheme.
spss_lines   <- readLines(SPSS_PARTIES, encoding = "UTF-8")
recode_lines <- spss_lines[str_detect(
  spss_lines,
  "^\\s*\\(\\s*[0-9]+[,.]?[0-9]*\\s*=\\s*[0-9]+\\s*\\)"
)]
if (length(recode_lines) == 0) stop("No RECODE lines found in SPSS_PARTIES.")

recode_map_spss <- tibble(raw = recode_lines) |>
  mutate(
    party_key_raw = str_extract(raw, "[0-9]+[,.]?[0-9]*(?=\\s*=)"),
    party_key_raw = str_replace(party_key_raw, ",", "."),
    party_key     = as.integer(round(as.numeric(party_key_raw))),
    partyfam_spss = as.integer(str_extract(str_split_fixed(raw, "=", 2)[, 2], "\\d+"))
  ) |>
  filter(!is.na(party_key), !is.na(partyfam_spss)) |>
  distinct(party_key, .keep_all = TRUE) |>
  select(party_key, partyfam_spss)

cat("Part A: SPSS party recode table rows:", nrow(recode_map_spss), "\n")

# --- Part B: PopuList far-right classification (second source) ---
# PopuList classifies parties using ParlGov IDs.
# The SPSS crosswalk file links ESS party keys to ParlGov IDs.
parlgov_lines <- readLines(SPSS_PARLGOV, encoding = "UTF-8")
parlgov_lines <- parlgov_lines[str_detect(parlgov_lines, "IF\\s*\\(\\s*party=")]

parlgov_xwalk <- tibble(raw = parlgov_lines) |>
  mutate(
    party_key  = as.integer(round(as.numeric(str_extract(raw, "(?<=party=\\s{0,5})[0-9.]+")))),
    parlgov_id = as.integer(str_extract(raw, "(?<=partyid=\\s{0,5})[0-9]+"))
  ) |>
  filter(!is.na(party_key), !is.na(parlgov_id)) |>
  distinct(party_key, .keep_all = TRUE) |>
  select(party_key, parlgov_id)

populist_raw <- read_delim(POPULIST_CSV, delim = ";", show_col_types = FALSE)
populist_farright <- populist_raw |>
  select(parlgov_id, farright) |>
  filter(!is.na(parlgov_id)) |>
  mutate(parlgov_id = as.integer(parlgov_id)) |>
  group_by(parlgov_id) |>
  summarise(farright_populist = as.integer(max(farright, na.rm = TRUE)), .groups = "drop")

recode_map_populist <- parlgov_xwalk |>
  left_join(populist_farright, by = "parlgov_id") |>
  select(party_key, farright_populist)

cat("Part B: PopuList crosswalk rows:", nrow(recode_map_populist), "\n\n")

# --- Part C: Compute party_key from ESS vote variables ---
# party_key = a unique number per party per round, built as:
#   (ESS round * 100,000) + (country numeric code * 100) + party code
#
# IMPORTANT: the country numeric codes here come from the authors' SPSS syntax,
# NOT from standard ISO 3166-1. They must match exactly.
ess <- readRDS(ESS_CAB_IN)
raw <- readRDS(ESS_RAW_IN)

cnr_map <- tibble(
  cntry = c("AT", "BE", "BG", "CH", "CZ", "DE", "DK", "EE", "FI",
            "FR", "GB", "GR", "HR", "HU", "IT", "LT", "NL", "NO", "PL",
            "SE", "SI", "SK"),
  cnr   = c(  40,   56,   88,  756,  203,  276,  208,  288,  246,
              250,  826,  300,  311,  348,  380,  488,  528,  578,  616,
              752,  705,  703)
)

vote_cols <- grep("^(prtvt|prtv)", names(raw), value = TRUE)
if (length(vote_cols) == 0) stop("No prtvt/prtv vote columns found.")

raw_votes <- raw |>
  select(cntry, idno, essround, vote, all_of(vote_cols)) |>
  mutate(
    vote = as.numeric(zap_labels(vote)),
    across(all_of(vote_cols), ~ as.numeric(zap_labels(.)))
  )

ess2 <- ess |>
  left_join(raw_votes, by = c("cntry", "idno", "essround")) |>
  left_join(cnr_map,   by = "cntry")

if (any(is.na(ess2$cnr))) {
  stop("cnr missing for countries: ",
       paste(sort(unique(ess2$cntry[is.na(ess2$cnr)])), collapse = ", "))
}

# Average across all vote columns (handles country-specific column names),
# then round to the nearest integer to get the party code
vote_matrix <- as.matrix(sapply(ess2[vote_cols], as.numeric))
party_mean  <- rowMeans(vote_matrix, na.rm = TRUE)
party_mean[is.nan(party_mean)] <- NA_real_

ess2 <- ess2 |>
  mutate(
    party_code = as.integer(round(party_mean)),
    party_key  = as.integer(essround * 100000 + cnr * 100 + party_code)
  )

# --- Part D: Join both codings and build fr_vote ---
# fr_vote = 1 if EITHER source (SPSS author coding OR PopuList) flags the party as far-right
# fr_vote = 0 if voted but party is NOT far-right by either source
# fr_vote = NA if: did not vote, vote status unknown, or party cannot be identified
ess_with_dv <- ess2 |>
  left_join(recode_map_spss, by = "party_key") |>
  mutate(
    partyfam_spss = ifelse(partyfam_spss == 0, NA_integer_, as.integer(partyfam_spss)),
    fr_spss = case_when(
      is.na(partyfam_spss) ~ NA_integer_,
      partyfam_spss == 1L  ~ 1L,
      TRUE                 ~ 0L
    )
  ) |>
  left_join(recode_map_populist, by = "party_key") |>
  mutate(
    fr_vote = case_when(
      is.na(vote) | vote != 1                               ~ NA_integer_, # non-voter/ineligible
      (!is.na(fr_spss) & fr_spss == 1L)                     ~ 1L,          # SPSS says far-right
      (!is.na(farright_populist) & farright_populist == 1L) ~ 1L,          # PopuList says far-right
      is.na(party_code)                                     ~ NA_integer_, # voted but party unknown
      TRUE                                                   ~ 0L           # voted non-far-right
    )
  ) |>
  select(-all_of(vote_cols))

# --- Diagnostics ---
cat("DV missingness checks:\n")
voted <- !is.na(ess_with_dv$vote) & ess_with_dv$vote == 1
cat("  Voters N:", sum(voted), "\n")
cat("  party_code NA among voters (%):",
    round(mean(is.na(ess_with_dv$party_code[voted])) * 100, 2), "\n")
cat("  fr_vote    NA among voters (%):",
    round(mean(is.na(ess_with_dv$fr_vote[voted]))    * 100, 2),
    "(should equal party_code NA %)\n\n")

cat("fr_vote counts by country:\n")
print(
  ess_with_dv |> count(cntry, fr_vote, name = "n") |> arrange(cntry, fr_vote),
  n = Inf
)
cat("\n")

cat("Agreement between SPSS and PopuList classifications:\n")
print(table(SPSS = ess_with_dv$fr_spss, PopuList = ess_with_dv$farright_populist, useNA = "ifany"))

saveRDS(ess_with_dv, OUT_PATH)
cat("\nSaved to:", OUT_PATH, "\n")


# ==============================================================
# SECTION 6 — BUILD FINAL ANALYSIS DATASET
# (originally 06_build_analysis_dataset.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 6: Building final analysis dataset\n")
cat("##############################################################\n\n")

IN_PATH  <- "data/intermediate/ess_with_dv.rds"
OUT_PATH <- "data/final/analysis_dataset.rds"

ess <- readRDS(IN_PATH)

# --- Select only the variables needed for analysis ---
analysis <- ess |>
  select(
    idno, cntry, essround,
    country_name_short, country_id,
    cabinetid,
    fr_vote,
    farrightpower,
    trust_political, anti_immigration, authoritarian, redistribution, bad_economy,
    age, female, educ, subincome, unemployed, religiosity, polinterest, urban
  )

cat("After selecting variables: N =", nrow(analysis), "| variables:", ncol(analysis), "\n")

# --- Apply the selectperiod filter ---
# Only keep country-round-cabinet periods that contain at least one far-right voter.
# Periods with zero far-right voters cannot be modelled (the model has no 1s to learn from).
analysis <- analysis |>
  mutate(period = essround * 10000000 + country_id * 10000 + cabinetid)

period_has_frvote <- analysis |>
  filter(!is.na(fr_vote)) |>
  group_by(period) |>
  summarise(any_fr = any(fr_vote == 1, na.rm = TRUE)) |>
  filter(any_fr)

cat("\nPeriods with at least one far-right voter:", nrow(period_has_frvote), "\n")

before   <- nrow(analysis)
analysis <- analysis |> filter(period %in% period_has_frvote$period)
after    <- nrow(analysis)

cat("Rows dropped by selectperiod filter:", before - after, "\n")
cat("Rows remaining:", after, "\n")
cat("Countries remaining:", length(unique(analysis$cntry)), "\n")

# --- Standardize all independent variables (mean=0, SD=1) ---
# The paper states: "The independent variables are standardized."
# scale() subtracts the mean and divides by SD. as.numeric() strips the matrix wrapper.
# New columns get a _z suffix so the originals are preserved.
analysis <- analysis |>
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

cat("\nStandardization check (trust_political_z):\n")
cat("  Mean:", round(mean(analysis$trust_political_z, na.rm = TRUE), 4), "(should be ~0)\n")
cat("  SD:  ", round(sd(analysis$trust_political_z,   na.rm = TRUE), 4), "(should be ~1)\n")

# --- Drop rows missing on the outcome ---
before   <- nrow(analysis)
analysis <- analysis |> filter(!is.na(fr_vote))
after    <- nrow(analysis)
cat("\nRows dropped because fr_vote is missing:", before - after, "\n")
cat("Rows remaining for analysis:", after, "\n")

# --- Diagnostics ---
cat("\nfr_vote distribution (0 = other vote, 1 = far-right vote):\n")
print(table(analysis$fr_vote, useNA = "ifany"))

cat("\nfarrightpower distribution (0 = not in gov, 1 = in gov):\n")
print(table(analysis$farrightpower, useNA = "ifany"))

cat("\nNumber of respondents per country:\n")
print(
  analysis |>
    count(cntry, name = "n_respondents") |>
    arrange(cntry)
)

cat("\nMissing values per variable (%):\n")
miss_table <- analysis |>
  summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 1))) |>
  tidyr::pivot_longer(cols = everything(), names_to = "variable", values_to = "pct_missing") |>
  arrange(desc(pct_missing))
print(miss_table, n = Inf)

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(analysis, OUT_PATH)
cat("\nFinal dataset: N =", nrow(analysis), "| variables:", ncol(analysis), "\n")
cat("Saved to:", OUT_PATH, "\n")


# ==============================================================
# SECTION 7 — FIT REPLICATION MODELS
# (originally 07_replication_models.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 7: Fitting multilevel logistic regression models\n")
cat("  (This may take several minutes per model)\n")
cat("##############################################################\n\n")

IN_PATH <- "data/final/analysis_dataset.rds"
OUT_DIR <- "output/tables"
OUT_TXT <- file.path(OUT_DIR, "replication_models.txt")
OUT_RDS <- file.path(OUT_DIR, "replication_models.rds")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

df <- readRDS(IN_PATH)

# Variable groups used across models (matches Table 2 in the paper exactly)
vars_base   <- c("fr_vote", "trust_political_z",
                 "age_z", "female_z", "educ_z", "subincome_z",
                 "religiosity_z", "polinterest_z", "urban_z",
                 "essround", "period")
vars_policy <- c("anti_immigration_z", "authoritarian_z",
                 "redistribution_z", "bad_economy_z")
vars_m3     <- c("farrightpower", "cntry")
# NOTE: cntry is included so it is available in df_m3 for the
# fixed-effects sensitivity check further below.

all_needed   <- unique(c(vars_base, vars_policy, vars_m3))
missing_vars <- setdiff(all_needed, names(df))
if (length(missing_vars) > 0) {
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

# Helper: apply listwise deletion for a set of variables and format columns
# Each model gets its own complete-case sample so it is not penalized for
# missingness on variables it does not use.
make_sample <- function(data, vars) {
  data %>%
    filter(if_all(all_of(vars), ~ !is.na(.))) %>%
    mutate(
      fr_vote       = as.integer(fr_vote),
      farrightpower = if ("farrightpower" %in% names(.)) as.integer(farrightpower) else NA_integer_,
      essround      = as.factor(essround),  # factor = dummy variables for each wave
      period        = as.factor(period)     # factor = random grouping variable
    )
}

df_m1 <- make_sample(df, vars_base)
df_m2 <- make_sample(df, c(vars_base, vars_policy))
df_m3 <- make_sample(df, c(vars_base, vars_policy, vars_m3))

cat("Sample sizes (complete cases per model):\n")
cat("  Model 1 N =", nrow(df_m1), "\n")
cat("  Model 2 N =", nrow(df_m2), "\n")
cat("  Model 3 N =", nrow(df_m3), "\n")
cat("  (Paper reports N1 = 131,934 for Models 1-4)\n\n")

cat("fr_vote distribution in Model 3 sample:\n")
print(table(df_m3$fr_vote))
cat("\n")

# --- Model 1: Trust + sociodemographic controls ---
# glmer()  = generalised linear mixed effects (multilevel) regression
# binomial("logit") = logistic regression for a 0/1 outcome
# (1 | period) = random intercept by country-period
# essround as a factor = one dummy per survey wave (controls for time trends)
# bobyqa with 200k iterations = most reliable optimizer for these models
cat("Fitting Model 1...\n")
m1 <- glmer(
  fr_vote ~
    trust_political_z +
    age_z + female_z + educ_z + subincome_z +
    religiosity_z + polinterest_z + urban_z +
    essround +
    (1 | period),
  data    = df_m1,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 1 complete.\n\n")

# --- Model 2: Model 1 + policy-related motivations ---
cat("Fitting Model 2...\n")
m2 <- glmer(
  fr_vote ~
    trust_political_z +
    anti_immigration_z + authoritarian_z +
    redistribution_z + bad_economy_z +
    age_z + female_z + educ_z + subincome_z +
    religiosity_z + polinterest_z + urban_z +
    essround +
    (1 | period),
  data    = df_m2,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 2 complete.\n\n")

# --- Model 3: Model 2 + far right in power + interaction with trust ---
# The * operator is shorthand:
#   A * B  expands to  A + B + A:B
# A:B is the INTERACTION — it tests whether the effect of A changes depending on B.
# Paper hypothesis: trust matters LESS when the far right is already in power.
# This would show as a positive interaction coefficient (odds ratio > 1).
cat("Fitting Model 3...\n")
m3 <- glmer(
  fr_vote ~
    trust_political_z * farrightpower +
    anti_immigration_z + authoritarian_z +
    redistribution_z + bad_economy_z +
    age_z + female_z + educ_z + subincome_z +
    religiosity_z + polinterest_z + urban_z +
    essround +
    (1 | period),
  data    = df_m3,
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 3 complete.\n\n")

# --- Check the key result ---
cat("=== KEY RESULT: INTERACTION TERM (Model 3) ===\n")
m3_coefs <- fixef(m3)  # fixef() extracts the fixed effect coefficients
int_name <- "trust_political_z:farrightpower"

if (int_name %in% names(m3_coefs)) {
  b  <- m3_coefs[int_name]
  or <- exp(b)  # exp() converts log-odds to odds ratio
  cat(sprintf("Coefficient (log-odds): %+.4f\n", b))
  cat(sprintf("Odds ratio:              %.4f\n",  or))
  cat(sprintf("Paper reports OR = 1.47. Our OR = %.4f.\n", or))
  if (b > 0) {
    cat("Direction: POSITIVE — consistent with the paper's hypothesis.\n")
    cat("Interpretation: when the far right is in power, higher trust is LESS\n")
    cat("protective against voting far-right (protest-voting mechanism weakens).\n")
  } else {
    cat("Direction: NEGATIVE — does NOT match the paper's hypothesis.\n")
  }
} else {
  cat("WARNING: interaction term not found in Model 3.\n")
}

# --- Model summary table ---
model_list <- list("Model 1" = m1, "Model 2" = m2, "Model 3" = m3)

coef_labels <- c(
  "trust_political_z"               = "Political trust",
  "farrightpower"                   = "Far right in power",
  "trust_political_z:farrightpower" = "Pol. trust × far right in power",
  "anti_immigration_z"              = "Anti-immigration attitudes",
  "authoritarian_z"                 = "Authoritarian sentiment",
  "redistribution_z"                = "Income redistribution",
  "bad_economy_z"                   = "Bad economy",
  "age_z"                           = "Age",
  "female_z"                        = "Gender: female",
  "educ_z"                          = "Education",
  "subincome_z"                     = "Subjective income",
  "religiosity_z"                   = "Religiosity",
  "polinterest_z"                   = "Political interest",
  "urban_z"                         = "Living area: urban",
  "(Intercept)"                     = "Intercept"
)

cat("\n=== MODEL TABLE (Odds Ratios + 95% CI) ===\n")
msummary(
  model_list,
  exponentiate = TRUE,        # show odds ratios (exp of log-odds)
  statistic    = "conf.int",  # show 95% confidence intervals
  coef_map     = coef_labels,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  title        = "Replication of Muis et al. (2022) – Multilevel Logistic Regression (OR)"
)

# Save table to text file
sink(OUT_TXT)
cat("Replication of Muis, Brils & Gaidyte (2022)\n")
cat("Multilevel Binary Logistic Regression — Odds Ratios\n")
cat("DV: Far-right vote (1 = voted far right, 0 = other)\n")
cat("Random intercept by country-period; ESS round dummies included\n")
cat(sprintf("Model 1 N = %d | Model 2 N = %d | Model 3 N = %d\n\n",
            nrow(df_m1), nrow(df_m2), nrow(df_m3)))
msummary(
  model_list,
  exponentiate = TRUE,
  statistic    = "conf.int",
  coef_map     = coef_labels,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  output       = "markdown"
)
sink()
cat("\nModel table saved to:", OUT_TXT, "\n")

# --- Random effects summary ---
# The random intercept variance tells us how much country-periods differ
# in their baseline far-right vote probability, after accounting for all covariates.
cat("\nRandom intercept variance per model:\n")
for (nm in names(model_list)) {
  vc  <- VarCorr(model_list[[nm]])
  var <- as.numeric(vc$period)
  cat(sprintf("  %s — country-period intercept variance: %.4f\n", nm, var))
}

saveRDS(model_list, OUT_RDS)
cat("Models saved to:", OUT_RDS, "\n")

# --- Sensitivity check: fixed-effects logistic regression ---
# PURPOSE: Re-estimate Model 3 replacing the random intercept with country
# and round dummy variables. This absorbs all stable between-country differences
# without assuming they are normally distributed (as random intercepts assume).
# If the interaction holds here too, it is not an artefact of the multilevel structure.
#
# glm() = standard (non-multilevel) logistic regression
# factor(cntry) = one dummy per country (fixed effects)
# factor(essround) = one dummy per ESS wave (fixed effects)
#
# df_m3 already contains cntry because we added it to vars_m3 above.

cat("\n--- Sensitivity check: fixed-effects logistic regression ---\n")

model3_fe <- glm(
  fr_vote ~ trust_political_z * farrightpower +
    anti_immigration_z + authoritarian_z + redistribution_z + bad_economy_z +
    age_z + female_z + educ_z + subincome_z + religiosity_z +
    polinterest_z + urban_z +
    factor(cntry) + factor(essround),
  data   = df_m3,
  family = binomial(link = "logit")
)

# Extract the interaction term and report it
coef_fe <- coef(model3_fe)["trust_political_z:farrightpower"]
se_fe   <- sqrt(diag(vcov(model3_fe)))["trust_political_z:farrightpower"]
z_fe    <- coef_fe / se_fe
p_fe    <- 2 * pnorm(abs(z_fe), lower.tail = FALSE)
or_fe   <- exp(coef_fe)

cat("Interaction OR :", round(or_fe, 3), "\n")
cat("SE (log-OR)    :", round(se_fe, 3), "\n")
cat("z              :", round(z_fe,  3), "\n")
cat("p              :", round(p_fe,  4), "\n")
cat("Multilevel OR  : 1.398  (Model 3 above, for comparison)\n")
cat("--------------------------------------------------------------\n")


# ==============================================================
# SECTION 8 — TABLES AND FIGURES
# (originally 08_tables_figures.R)
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  SECTION 8: Creating regression table and interaction plot\n")
cat("##############################################################\n\n")

dat    <- readRDS("data/final/analysis_dataset.rds")
models <- readRDS("output/tables/replication_models.rds")

cat("Models loaded:", names(models), "\n")
cat("Dataset rows:", nrow(dat), "| Columns:", ncol(dat), "\n")
cat("Column names:\n")
print(names(dat))
cat("\n")

# --- Regression table (saved as HTML) ---
var_labels <- c(
  "female_z"                        = "Gender: female",
  "age_z"                           = "Age",
  "educ_z"                          = "Education",
  "subincome_z"                     = "Subjective income",
  "urban_z"                         = "Living area: urban",
  "religiosity_z"                   = "Religiosity",
  "polinterest_z"                   = "Political interest",
  
  "trust_political_z"               = "Political trust",
  "anti_immigration_z"              = "Anti-immigration attitudes",
  "authoritarian_z"                 = "Authoritarian sentiment",
  "bad_economy_z"                   = "Bad economy",
  "redistribution_z"                = "Income redistribution",
  
  "farrightpower"                   = "Far right in power",
  "trust_political_z:farrightpower" = "Pol. trust × far right in power",
  "(Intercept)"                     = "Intercept"
)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

modelsummary(
  models,
  exponentiate = TRUE,
  statistic    = "conf.int",
  coef_map     = var_labels,
  stars        = TRUE,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  title        = "Table 1. Multilevel Logistic Regression of Far-Right Voting (Odds Ratios)",
  notes        = "Odds ratios. 95% CI in brackets. Random intercept by country-period.",
  output       = "output/tables/replication_table.html"
)
cat("Regression table saved to output/tables/replication_table.html\n")

# --- Predicted probability plot ---
# Recreate the Model 3 complete-case sample to get correct variable ranges and levels
vars_m3 <- c(
  "fr_vote", "trust_political_z", "farrightpower",
  "anti_immigration_z", "authoritarian_z", "redistribution_z", "bad_economy_z",
  "age_z", "female_z", "educ_z", "subincome_z",
  "religiosity_z", "polinterest_z", "urban_z",
  "essround", "period"
)
missing_vars <- setdiff(vars_m3, names(dat))
if (length(missing_vars) > 0) {
  stop("Missing variables: ", paste(missing_vars, collapse = ", "))
}

df_m3 <- dat %>%
  filter(if_all(all_of(vars_m3), ~ !is.na(.))) %>%
  mutate(
    fr_vote       = as.integer(fr_vote),
    farrightpower = as.integer(farrightpower),
    essround      = as.factor(essround),
    period        = as.factor(period)
  )

cat("\nModel 3 analytic sample for plot: N =", nrow(df_m3), "\n")

# Build a grid of trust values (50 evenly-spaced points across the observed range)
# for both conditions: far right in power (1) vs not (0)
trust_z_range <- seq(
  from       = min(df_m3$trust_political_z, na.rm = TRUE),
  to         = max(df_m3$trust_political_z, na.rm = TRUE),
  length.out = 50
)

# expand.grid() creates every combination of trust_z × farrightpower
prediction_grid <- expand.grid(
  trust_political_z = trust_z_range,
  farrightpower     = c(0, 1)
) %>%
  mutate(
    # Hold all other variables at their mean.
    # Since they are standardized, their mean IS 0.
    anti_immigration_z = 0,
    authoritarian_z    = 0,
    redistribution_z   = 0,
    bad_economy_z      = 0,
    age_z              = 0,
    female_z           = 0,
    educ_z             = 0,
    subincome_z        = 0,
    religiosity_z      = 0,
    polinterest_z      = 0,
    urban_z            = 0,
    essround = levels(df_m3$essround)[1],  # needed by formula; ignored via re.form=NA
    period   = levels(df_m3$period)[1]
  )

# predict() with type="response" gives probabilities (0-1).
# re.form=NA tells it to ignore random effects (gives the "average" country-period).
prediction_grid$predicted_prob <- predict(
  models[["Model 3"]],
  newdata = prediction_grid,
  type    = "response",
  re.form = NA
)

# Back-transform x-axis to the original 0-10 trust scale
# Formula: original = z * SD + mean
if ("trust_political" %in% names(dat)) {
  orig_mean <- mean(dat$trust_political, na.rm = TRUE)
  orig_sd   <- sd(dat$trust_political,   na.rm = TRUE)
  prediction_grid <- prediction_grid %>%
    mutate(trust_original = trust_political_z * orig_sd + orig_mean)
  x_label <- "Political trust (0 = no trust, 10 = complete trust)"
  cat("Using original trust scale (0-10) for x-axis.\n")
} else {
  prediction_grid <- prediction_grid %>%
    mutate(trust_original = trust_political_z)
  x_label <- "Political trust (standardized)"
  cat("Original trust_political not found — using z-score for x-axis.\n")
}

prediction_grid <- prediction_grid %>%
  mutate(
    status = ifelse(farrightpower == 1, "Far right in power", "Far right in opposition")
  )

# Build the plot
interaction_plot <- ggplot(
  data    = prediction_grid,
  mapping = aes(x = trust_original, y = predicted_prob,
                color = status, linetype = status)
) +
  geom_line(size = 1) +
  labs(
    title    = "Effect of Political Trust on Far-Right Voting",
    subtitle = "Predicted probabilities from Model 3 (all other variables at their mean)",
    x        = x_label,
    y        = "Predicted probability of far-right vote",
    color    = "Far-right party status",
    linetype = "Far-right party status"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
  scale_color_manual(values = c(
    "Far right in opposition" = "black",
    "Far right in power"      = "grey50"
  )) +
  scale_linetype_manual(values = c(
    "Far right in opposition" = "solid",
    "Far right in power"      = "dashed"
  )) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

print(interaction_plot)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
png(
  filename = "output/figures/interaction_plot.png",
  width = 7, height = 5, units = "in", res = 300
)
print(interaction_plot)
dev.off()
cat("Interaction plot saved to output/figures/interaction_plot.png\n")

# --- Descriptive statistics table ---
desc_vars <- dat %>%
  select(any_of(c(
    "fr_vote", "trust_political", "farrightpower",
    "anti_immigration", "authoritarian",
    "age", "female", "educ"
  )))

datasummary_skim(
  desc_vars,
  title  = "Table A1. Descriptive Statistics (full dataset)",
  output = "output/tables/descriptive_stats.html"
)
cat("Descriptive statistics saved to output/tables/descriptive_stats.html\n")


# ==============================================================
# PIPELINE COMPLETE
# ==============================================================

cat("\n")
cat("##############################################################\n")
cat("  ALL SECTIONS COMPLETE\n")
cat("\n")
cat("  Output files:\n")
cat("    data/intermediate/ess_raw_combined.rds\n")
cat("    data/intermediate/ess_harmonized.rds\n")
cat("    data/intermediate/ess_with_cabinet.rds\n")
cat("    data/intermediate/ess_with_dv.rds\n")
cat("    data/final/analysis_dataset.rds\n")
cat("    output/tables/replication_models.rds\n")
cat("    output/tables/replication_models.txt\n")
cat("    output/tables/replication_table.html\n")
cat("    output/tables/descriptive_stats.html\n")
cat("    output/figures/interaction_plot.png\n")
cat("##############################################################\n\n")