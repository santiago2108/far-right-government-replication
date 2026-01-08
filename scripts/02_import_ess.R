# ============================================================
# 02_combine_ess_raw.R  (FIXED + your paths)
# ============================================================

library(dplyr)
library(haven)

ESS_RAW_DIR <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/raw/ess"
OUT_PATH    <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/intermediate/ess_raw_combined.rds"

# Variables needed downstream (minimal, for Muis, Brils & Gaidytė 2022 + date harmonisation)
keep_vars <- c(
  "cntry","idno","essround",
  
  # interview date variables (round-dependent)
  "inwyr","inwmm","inwdd",          # rounds 1–2
  "inwyys","inwmms","inwdds",       # rounds 3–8 (start date)
  
  # main variables + controls
  "vote","trstprl","trstplt","trstprt",
  "agea","gndr","eduyrs","uempla","lrscale",
  "stfeco","stfgov","stfdem",
  "gincdif",
  
  # income (round-dependent)
  "hinctnt","hinctnta",
  
  # anti-immigration (paper/SPSS)
  "imbgeco","imueclt","imwbcnt",
  
  # authoritarian sentiment (paper/SPSS)
  "ipfrule","ipstrgv","ipbhprp","imptrad","impsafe"
)

read_one <- function(path) {
  df <- read_dta(path)
  
  # keep only columns that exist in this round
  df <- df %>% select(any_of(keep_vars))
  
  # ensure date columns exist (create as NA if absent in this round)
  need_date_cols <- c("inwyr","inwmm","inwdd","inwyys","inwmms","inwdds")
  for (v in need_date_cols) {
    if (!v %in% names(df)) df[[v]] <- NA
  }
  
  # harmonize interview date into common names: inwyr/inwmm/inwdd
  # ---- harmonise interview date into inwyr/inwmm/inwdd ----
  # ESS1–2: inwyr/inwmm/inwdd
  # ESS3–8: inwyys/inwmms/inwdds (start date)
  
  if (!"inwyr" %in% names(df) && "inwyys" %in% names(df)) df$inwyr <- df$inwyys
  if (!"inwmm" %in% names(df) && "inwmms" %in% names(df)) df$inwmm <- df$inwmms
  if (!"inwdd" %in% names(df) && "inwdds" %in% names(df)) df$inwdd <- df$inwdds
  
  if ("inwyys" %in% names(df)) df$inwyr <- dplyr::coalesce(df$inwyr, df$inwyys)
  if ("inwmms" %in% names(df)) df$inwmm <- dplyr::coalesce(df$inwmm, df$inwmms)
  if ("inwdds" %in% names(df)) df$inwdd <- dplyr::coalesce(df$inwdd, df$inwdds)
  
  df <- df %>% dplyr::select(-dplyr::any_of(c("inwyys","inwmms","inwdds")))
  
  # ensure income columns exist; harmonize to hinctnta
  if (!"hinctnta" %in% names(df)) df[["hinctnta"]] <- NA
  if (!"hinctnt"  %in% names(df)) df[["hinctnt"]]  <- NA
  
  df <- df %>%
    mutate(hinctnta = coalesce(hinctnta, hinctnt)) %>%
    select(-any_of("hinctnt"))
  
  df
}

files <- file.path(ESS_RAW_DIR, paste0("ESS", 1:8, "_raw.dta"))

# fail early if any file is missing
missing_files <- files[!file.exists(files)]
if (length(missing_files) > 0) {
  stop("These files are missing:\n", paste(missing_files, collapse = "\n"))
}

ess_raw_combined <- bind_rows(lapply(files, read_one))

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_raw_combined, OUT_PATH)

message("Saved combined raw file to: ", OUT_PATH)

# Quick diagnostic: date availability by round
diag <- ess_raw_combined %>%
  group_by(essround) %>%
  summarise(
    n = n(),
    pct_na_inwyr = mean(is.na(inwyr)),
    pct_na_inwmm = mean(is.na(inwmm)),
    pct_na_inwdd = mean(is.na(inwdd))
  )
print(diag)

  
