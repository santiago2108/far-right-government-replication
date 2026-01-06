# ============================================================
# 02_import_ess.R
# Import ESS rounds 1–8, keep needed variables, combine, save
# ============================================================

library(haven)
library(dplyr)

# ---- Paths ----
ess_dir <- "data/raw/ess"
out_dir <- "data/intermediate"

file_1 <- paste0(ess_dir, "/ESS1_raw.dta")
file_2 <- paste0(ess_dir, "/ESS2_raw.dta")
file_3 <- paste0(ess_dir, "/ESS3_raw.dta")
file_4 <- paste0(ess_dir, "/ESS4_raw.dta")
file_5 <- paste0(ess_dir, "/ESS5_raw.dta")
file_6 <- paste0(ess_dir, "/ESS6_raw.dta")
file_7 <- paste0(ess_dir, "/ESS7_raw.dta")
file_8 <- paste0(ess_dir, "/ESS8_raw.dta")

# ---- Variables to keep ----
# Keep: IDs, interview date, voting, trust, basic controls
vars_keep <- c(
  "cntry", "idno", "essround",
  "inwyr", "inwmm", "inwdd",
  "vote",
  "trstprl", "trstplt", "trstprt",
  "agea", "gndr", "eduyrs", "eisced",
  "hinctnta", "uempla",
  "rlgblg", "rlgdgr", "rlgatnd",
  "polintr", "lrscale",
  "stfeco", "stfgov", "stfdem",
  "gincdif", "domicil"
)

# ---- Import each round ----
ess1 <- read_dta(file_1) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess2 <- read_dta(file_2) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess3 <- read_dta(file_3) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess4 <- read_dta(file_4) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess5 <- read_dta(file_5) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess6 <- read_dta(file_6) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess7 <- read_dta(file_7) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

ess8 <- read_dta(file_8) |>
  select(any_of(vars_keep), starts_with("prtvt"), starts_with("im"))

# ---- Combine ----
ess_all <- bind_rows(ess1, ess2, ess3, ess4, ess5, ess6, ess7, ess8)

# ---- Quick checks ----
dim(ess_all)
table(ess_all$essround, useNA = "ifany")

# ---- Save ----
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_all, paste0(out_dir, "/ess_raw_combined.rds"))