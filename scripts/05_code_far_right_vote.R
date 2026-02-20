# ============================================================
# 05_code_far_right_vote.R
# Create fr_vote (0/1) using the SPSS RECODE table embedded in
# (1) ESS1-8-PARTIES_NEWEST.sps  (NOT the Families Excel sheet)
# Input:  data/intermediate/ess_with_cabinet.rds + ess_raw_combined.rds
# Output: data/intermediate/ess_with_dv.rds
# Deliverable: table of fr_vote counts by country (sanity check)
# ============================================================

library(dplyr)
library(haven)
library(stringr)
library(readr)

ESS_CAB_IN  <- "data/intermediate/ess_with_cabinet.rds"
ESS_RAW_IN  <- "data/intermediate/ess_raw_combined.rds"
SPSS_FILE   <- "data/raw/author_materials/(1)_ESS1-8-PARTIES_NEWEST.sps"
OUT_PATH    <- "data/intermediate/ess_with_dv.rds"

# ============================================================
# 1. Parse RECODE block from SPSS syntax -> party_key : partyfam
# ============================================================
# Lines look like:  (  334809,00  =  1  )  or  (  334809.00  =  1  )
spss_lines <- readLines(SPSS_FILE, encoding = "UTF-8")

recode_lines <- spss_lines[str_detect(spss_lines,
                                      "^\\s*\\(\\s*[0-9]+[,.]?[0-9]*\\s*=\\s*[0-9]+\\s*\\)")]

recode_map <- tibble(raw = recode_lines) %>%
  mutate(
    # Extract the numeric party key (handles comma as decimal separator)
    party_key = as.integer(round(
      parse_number(
        str_extract(raw, "[0-9]+[,.]?[0-9]*(?=\\s*=)"),
        locale = locale(decimal_mark = ",", grouping_mark = ".")
      )
    )),
    # Extract the family code (integer after the =)
    partyfam = as.integer(str_extract(raw, "(?<==\\s*)\\d+"))
  ) %>%
  filter(!is.na(party_key), !is.na(partyfam)) %>%
  distinct(party_key, .keep_all = TRUE)   # first match wins (like SPSS RECODE)

cat("Parsed", nrow(recode_map), "entries from SPSS RECODE block.\n")
cat("Far-right entries (partyfam==1):", sum(recode_map$partyfam == 1), "\n")

# ============================================================
# 2. ISO numeric (cnr) crosswalk — same codes used in SPSS
#    party = essround*100000 + cnr*100 + mean(vote_vars)
# ============================================================
cnr_map <- tibble(
  cntry = c("AT","BE","CH","CZ","DE","DK","EE","ES","FI","FR",
            "GB","HU","IE","IT","LV","NL","NO","PL","PT","SE","SI","SK"),
  cnr   = c(  40,  56, 756, 203, 276, 208, 233, 724, 246, 250,
              826, 348, 372, 380, 428, 528, 578, 616, 620, 752, 705, 703)
)

# ============================================================
# 3. Load data and bring in party-vote columns
# ============================================================
ess <- readRDS(ESS_CAB_IN)
raw <- readRDS(ESS_RAW_IN)

vote_cols <- grep("^(prtvt|prtv)", names(raw), value = TRUE)

raw_votes <- raw %>%
  select(cntry, idno, essround, vote, all_of(vote_cols)) %>%
  mutate(
    vote = as.numeric(zap_labels(vote)),
    across(all_of(vote_cols), ~ as.numeric(zap_labels(.)))
  )

ess2 <- ess %>%
  left_join(raw_votes, by = c("cntry", "idno", "essround")) %>%
  left_join(cnr_map,   by = "cntry")

# ============================================================
# 4. Compute party key — EXACT SPSS formula
#    compute party = (essround*100000) + (cnr*100) + mean(prtvtat to prtvtcua)
# ============================================================
vote_mat   <- as.matrix(sapply(ess2[vote_cols], as.numeric))
party_mean <- rowMeans(vote_mat, na.rm = TRUE)
party_mean[is.nan(party_mean)] <- NA_real_

ess2 <- ess2 %>%
  mutate(
    party_code = as.integer(round(party_mean)),
    party_key  = as.integer(essround * 100000 + cnr * 100 + party_code)
  )

# ============================================================
# 5. Apply RECODE map -> partyfam -> fr_vote
#    SPSS logic: recode partyfam (0=sysmis); compute radicalright=(partyfam==1)
# ============================================================
ess_with_dv <- ess2 %>%
  left_join(recode_map %>% select(party_key, partyfam), by = "party_key") %>%
  mutate(
    partyfam = ifelse(partyfam == 0, NA_integer_, as.integer(partyfam)),
    fr_vote  = case_when(
      is.na(partyfam) ~ NA_integer_,
      partyfam == 1L  ~ 1L,
      TRUE            ~ 0L
    )
  ) %>%
  select(-all_of(vote_cols))   # keep dataset clean

# ============================================================
# 6. Save
# ============================================================
dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_with_dv, OUT_PATH)

# ============================================================
# 7. Deliverable: fr_vote counts by country
# ============================================================
print(
  ess_with_dv %>%
    count(cntry, fr_vote, name = "n") %>%
    arrange(cntry, fr_vote)
)

# Minimal diagnostics
cat("\nShare missing party_code:", mean(is.na(ess_with_dv$party_code)), "\n")
cat("Share missing partyfam:",   mean(is.na(ess_with_dv$partyfam)),   "\n")

voters <- !is.na(ess_with_dv$vote) & ess_with_dv$vote == 1
cat("\nAmong voters (vote==1):\n")
cat("  share missing party_code:", mean(is.na(ess_with_dv$party_code[voters])), "\n")
cat("  share missing partyfam:",   mean(is.na(ess_with_dv$partyfam[voters])),   "\n")
cat("  share missing fr_vote:",    mean(is.na(ess_with_dv$fr_vote[voters])),    "\n")