# ============================================================
# 05_code_far_right_vote.R
#
# Goal: Create the dependent variable fr_vote
#       (1 = voted far-right, 0 = voted other)
#
# Sources:
#   A) Author SPSS party-family recode file:
#      data/raw/author_materials/(1) ESS1-8-PARTIES NEWEST.sps
#   B) PopuList 3.0 + author crosswalk:
#      data/raw/author_materials/(2) ESS1-8 PARLGOV PARTY ID.sps
#      data/raw/popuList/The_PopuList_3.0.csv
#
# Output:
#   data/intermediate/ess_with_dv.rds
# ============================================================

  library(dplyr)
  library(haven)
  library(stringr)
  library(readr)

# ---- Paths ----
ESS_CAB_IN   <- "data/intermediate/ess_with_cabinet.rds"
ESS_RAW_IN   <- "data/intermediate/ess_raw_combined.rds"
SPSS_PARTIES <- "data/raw/author_materials/(1) ESS1-8-PARTIES NEWEST.sps"
SPSS_PARLGOV <- "data/raw/author_materials/(2) ESS1-8 PARLGOV PARTY ID.sps"
POPULIST_CSV <- "data/raw/popuList/The_PopuList_3.0.csv"
OUT_PATH     <- "data/intermediate/ess_with_dv.rds"

# ============================================================
# PART A: Author SPSS partyfam recode table (party_key -> partyfam)
# ============================================================

spss_lines <- readLines(SPSS_PARTIES, encoding = "UTF-8")

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

# ============================================================
# PART B: PopuList mapping (party_key -> farright_populist)
# ============================================================

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

# ============================================================
# PART C: Compute party_key from ESS vote variables
# ============================================================

ess <- readRDS(ESS_CAB_IN)
raw <- readRDS(ESS_RAW_IN)

# ---- Country numeric code lookup ----
#
# CRITICAL: these are the codes from the author's own SPSS syntax file
# "(0) ESS1-8 General-independent-individual-variables.sps", NOT
# standard ISO 3166-1 codes. They must match exactly or every party_key
# will be wrong and no parties will be found in the lookup tables.

cnr_map <- tibble(
  cntry = c("AT", "BE", "BG", "CH", "CZ", "DE", "DK", "EE", "FI",
            "FR", "GB", "GR", "HR", "HU", "IT", "LT", "NL", "NO", "PL",
            "SE", "SI", "SK"),
  cnr   = c(  40,   56,   88,  756,  203,  276,  208,  288,  246,
              250,  826,  300,  311,  348,  380,  488,  528,  578,  616,
              752,  705,  703)
)

# Party vote columns
vote_cols <- grep("^(prtvt|prtv)", names(raw), value = TRUE)
if (length(vote_cols) == 0) stop("No prtvt/prtv vote columns found in ess_raw_combined.")

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

vote_matrix <- as.matrix(sapply(ess2[vote_cols], as.numeric))
party_mean  <- rowMeans(vote_matrix, na.rm = TRUE)
party_mean[is.nan(party_mean)] <- NA_real_

ess2 <- ess2 |>
  mutate(
    party_code = as.integer(round(party_mean)),
    party_key  = as.integer(essround * 100000 + cnr * 100 + party_code)
  )

# ============================================================
# PART D: Join codings + build fr_vote
# ============================================================

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
      # Non-voters and ineligible -> excluded from analysis
      is.na(vote) | vote != 1                               ~ NA_integer_,
      # Voted + either source flags far-right -> 1
      (!is.na(fr_spss)           & fr_spss == 1L)           ~ 1L,
      (!is.na(farright_populist) & farright_populist == 1L) ~ 1L,
      # Voted but party_code is missing -> cannot classify, drop from analysis.
      # These are voters who gave no party response (~13% of voters).
      # Treated as NA so they are excluded by filter(!is.na(fr_vote)) in script 06.
      is.na(party_code)                                     ~ NA_integer_,
      # Voted + party identified + neither source flags far-right -> 0
      # Genuine reference category: voters for non-far-right parties and blank voters.
      TRUE                                                   ~ 0L
    )
  ) |>
  select(-all_of(vote_cols))

# ============================================================
# PART E: Save and checks
# ============================================================

saveRDS(ess_with_dv, OUT_PATH)

#cat("\n--- DV missingness checks ---\n")
#voted <- !is.na(ess_with_dv$vote) & ess_with_dv$vote == 1
#cat("Voters N:", sum(voted), "\n")
#cat("party_code NA among voters (%):", round(mean(is.na(ess_with_dv$party_code[voted])) * 100, 2), "\n")
#cat("party_key  NA among voters (%):", round(mean(is.na(ess_with_dv$party_key[voted]))  * 100, 2), "\n")
#cat("fr_vote    NA among voters (%):", round(mean(is.na(ess_with_dv$fr_vote[voted]))    * 100, 2), " (should equal party_code NA %)\n")

#cat("\n--- fr_vote counts by country (all respondents) ---\n")
#print(
#  ess_with_dv |> count(cntry, fr_vote, name = "n") |> arrange(cntry, fr_vote),
#  n = Inf
#)

#cat("\n--- Agreement SPSS vs PopuList (raw flags, all respondents) ---\n")
#print(table(SPSS = ess_with_dv$fr_spss, PopuList = ess_with_dv$farright_populist, useNA = "ifany"))