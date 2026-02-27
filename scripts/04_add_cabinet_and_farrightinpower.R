# ============================================================
# 04_add_cabinet_and_farrightinpower.R
#
# PURPOSE:
#   Add two country-level context variables to the ESS dataset:
#     1. cabinetid    – which government was in power when each
#                       respondent was interviewed
#     2. farrightpower – was a far-right party in that government?
#                       (1 = yes, 0 = no)
#
# HOW IT WORKS:
#   The interview date of each respondent decides which cabinet they
#   belong to. Within a country, cabinets are separated by start
#   dates. The dates go from earliest to latest, the
#   LAST cutoff that is still BEFORE the interview date wins.
#   This logic comes directly from the authors' SPSS syntax file
#   "(3a) (cabinet IDs)" and "(3b) (far-right cabinet IDs)".
#
# DATA SOURCES USED:
#   - data/intermediate/ess_harmonized.rds   (ESS data so far)
#   - data/raw/author_materials/(3a) ESS1-8_Cabinet_ID_addition...sps
#   - data/raw/author_materials/(3b) ESS1-8_Far_right_in_government...sps
#   - data/raw/parlgov/view_cabinet.csv  (to get country_id)
# ============================================================

library(dplyr)
library(readr)
library(stringr)

# ── File paths ───

ESS_IN    <- "data/intermediate/ess_harmonized.rds"
SPS_CAB   <- "data/raw/author_materials/(3a) ESS1-8 Cabinet ID addition with date of Interview.sps"
SPS_FR    <- "data/raw/author_materials/(3b) ESS1-8 Far right in government addition with Cabinet ID.sps"
PARLGOV   <- "data/raw/parlgov/view_cabinet.csv"
OUT_PATH  <- "data/intermediate/ess_with_cabinet.rds"

# ── Step 1: Load the ESS data ───

ess <- readRDS(ESS_IN)

# ── Step 2: Map ESS country codes to ParlGov country IDs ───
#
# ESS uses 2-letter codes (e.g. "DE" for Germany)
# ParlGov uses numeric country IDs (e.g. 35 for Germany)
# I link them using ParlGov's view_cabinet.csv, which contains:
#   - country_name_short (ISO-3, e.g. DEU)
#   - country_id (ParlGov numeric)
#

# 2-letter ESS codes → 3-letter ISO codes
cntry_map <- data.frame(
  cntry              = c("AT","BE","BG","CH","CZ","DE","DK","EE","ES",
                         "FI","FR","GB","GR","HR","HU","IE","IT","LT","LV","NL",
                         "NO","PL","PT","SE","SI","SK"),
  country_name_short = c("AUT","BEL","BGR","CHE","CZE","DEU","DNK","EST","ESP",
                         "FIN","FRA","GBR","GRC","HRV","HUN","IRL","ITA","LTU","LVA","NLD",
                         "NOR","POL","PRT","SWE","SVN","SVK"),
  stringsAsFactors   = FALSE
)

# Read ParlGov cabinet file – only one row per country to get country_id
parlgov_countries <- read_csv(PARLGOV, show_col_types = FALSE) |>
  select(country_name_short, country_id) |>
  distinct()

# Join the two maps together so we have: cntry | country_name_short | country_id
cntry_map <- cntry_map |>
  left_join(parlgov_countries, by = "country_name_short")

# Hard stop if any country_id is missing (prevents silent country loss)
missing_id <- cntry_map |> filter(is.na(country_id))
if (nrow(missing_id) > 0) {
  stop(
    "ERROR: country_id is missing for these ESS countries: ",
    paste(missing_id$cntry, collapse = ", "),
    "\nTheir ISO-3 codes (country_name_short) did not match ParlGov view_cabinet.csv."
  )
}

# Add country_id to the ESS data
ess <- ess |>
  left_join(cntry_map, by = "cntry")

# Hard stop if ESS now has any missing country_id
if (any(is.na(ess$country_id))) {
  stop(
    "ERROR: ESS rows with missing country_id after join for cntry: ",
    paste(sort(unique(ess$cntry[is.na(ess$country_id)])), collapse = ", ")
  )
}

# ── Step 3: Build the cabinet cutoff rules from the SPSS syntax file ───

sps_lines <- readLines(SPS_CAB)

rule_lines <- sps_lines[str_detect(sps_lines, "interviewdate") &
                          str_detect(sps_lines, "cabinetid")]

extracted <- str_match(
  rule_lines,
  "country_id=\\s*(\\d+).*?interviewdate\\s*>\\s*(\\d+).*?cabinetid\\s*=\\s*(\\d+)"
)

cabinet_rules <- data.frame(
  country_id   = as.integer(extracted[, 2]),
  cutoff_date  = as.integer(extracted[, 3]),
  cabinetid    = as.integer(extracted[, 4]),
  stringsAsFactors = FALSE
)

cabinet_rules <- cabinet_rules[complete.cases(cabinet_rules), ]
cabinet_rules <- distinct(cabinet_rules)
cabinet_rules <- arrange(cabinet_rules, country_id, cutoff_date)

# ── Step 4: Assign cabinetid to each ESS respondent ───
#
# HOW THIS WORKS:
# Every respondent is linked to every cabinet cutoff rule for their country
# (many-to-many join). Then only rules where the cutoff date is
# BEFORE the respondent's interview date are kept. The rule with the LATEST cutoff
# date that still satisfies this condition is the correct cabinet.
#
# IMPORTANT! — rows with NA interviewdate:
# In R, the comparison NA > any_number evaluates to NA (not TRUE or FALSE).
# filter() drops NA results, so respondents with a missing interview date
# are silently removed here. We check for this explicitly before and after
# so the loss is visible and documented, not hidden.

# Count and report respondents with missing interview date BEFORE the join.
# They will be dropped silently by filter() below.
n_missing_date <- sum(is.na(ess$interviewdate))
if (n_missing_date > 0) {
  message("WARNING: ", n_missing_date, " respondents have NA interviewdate.")
  message("  They will be dropped during cabinet assignment because we cannot")
  message("  determine which cabinet was in power when they were interviewed.")
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

ess_cab <- ess |>
  left_join(cabinet_rules, by = "country_id",
            relationship = "many-to-many") |>
  filter(interviewdate > cutoff_date) |>
  group_by(cntry, idno, essround, interviewdate) |>
  slice_max(order_by = cutoff_date, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(-cutoff_date)

# Report how many respondents were lost due to missing date or no matching rule
#n_rows_after_join <- nrow(ess_cab)
#n_lost <- n_rows_before_join - n_rows_after_join
#message("\nRows before cabinet assignment: ", n_rows_before_join)
#message("Rows after  cabinet assignment: ", n_rows_after_join)
#message("Rows lost (missing date or before first cutoff): ", n_lost)

#n_missing_cab <- sum(is.na(ess_cab$cabinetid))
#message("Respondents with no cabinet assigned (cabinetid = NA): ", n_missing_cab)

# ── Step 5: Hard-code the far-right cabinet IDs ───

sps_fr_lines <- readLines(SPS_FR)
fr_lines <- sps_fr_lines[str_detect(sps_fr_lines, "farrightpower\\s*=\\s*1")]
fr_extracted <- str_match(fr_lines, "cabinetid=\\s*(\\d+)")

fr_cabinets <- data.frame(
  cabinetid     = as.integer(fr_extracted[, 2]),
  farrightpower = 1L,
  stringsAsFactors = FALSE
) |>
  distinct(cabinetid, .keep_all = TRUE)

# ── Step 6: Add farrightpower to the ESS data ───

ess_cab <- ess_cab |>
  left_join(fr_cabinets, by = "cabinetid") |>
  mutate(
    farrightpower = if_else(is.na(farrightpower), 0L, farrightpower)
  )

# ── Step 7: Compute the "period" variable ───

ess_cab <- ess_cab |>
  mutate(
    period = (essround * 10000000L) + (country_id * 10000L) + cabinetid
  )

# ── Step 8: Quick summary checks ──

#message("Total rows in final dataset: ", nrow(ess_cab))
#message("Rows with farrightpower = 1: ", sum(ess_cab$farrightpower == 1, na.rm = TRUE))
#message("Rows with farrightpower = 0: ", sum(ess_cab$farrightpower == 0, na.rm = TRUE))
#message("Number of unique periods:    ", n_distinct(ess_cab$period, na.rm = TRUE))

#message("\nCountries present in ess_with_cabinet:")
#print(sort(unique(ess_cab$cntry)))

#message("\nRespondents by farrightpower:")
#print(table(ess_cab$farrightpower, useNA = "ifany"))

# ── Step 9: Save ───

saveRDS(ess_cab, OUT_PATH)
