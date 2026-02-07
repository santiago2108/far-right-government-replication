# ============================================================
# 02_combine_ess_raw.R  (paper-faithful, ESS1–ESS8)
# Combine raw ESS rounds and keep only variables needed for Muis et al. (2022)
# ============================================================

library(dplyr)
library(haven)

ESS_RAW_DIR <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/raw/ess"
OUT_PATH    <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/intermediate/ess_raw_combined.rds"

# Variables needed downstream (close to paper + interview date)
keep_vars <- c(
  # identifiers
  "cntry", "idno", "essround",
  
  # interview date variables (round-dependent)
  "inwyr","inwmm","inwdd",          # rounds 1–2
  "inwyys","inwmms","inwdds",       # rounds 3–8 (start date)
  
  # vote + trust (core)
  "vote",
  "trstprl","trstplt","trstprt",
  
  # demographics / controls used in the paper (SPSS-based)
  "agea","gndr",
  "hincfel",                        # subjective income (NOT deciles)
  "mnactic",                        # main activity (for unemployment dummy)
  "rlgdgr",                         # religiosity scale
  "polintr",                        # political interest
  "domicil",                        # urban/rural dummy
  
  # education inputs (A in rounds 1–4, B in rounds 5–8)
  "edulvla","edulvlb",
  "eduyrs","eisced",                # optional backups (can help if missing)
  

  # anti-immigration
  "imbgeco","imueclt","imwbcnt",
  
  # authoritarian items 
  "ipfrule","ipstrgv","ipbhprp","imptrad","impsafe"
)

read_one <- function(path) {
  df <- read_dta(path) %>%
    select(any_of(keep_vars))
  
  # ensure date columns exist (create as NA if absent)
  date_cols <- c("inwyr","inwmm","inwdd","inwyys","inwmms","inwdds")
  for (v in date_cols) {
    if (!v %in% names(df)) df[[v]] <- NA
  }
  
  # harmonize interview date into inwyr/inwmm/inwdd
  # ESS1–2: inwyr/inwmm/inwdd
  # ESS3–8: inwyys/inwmms/inwdds (start date)
  df <- df %>%
    mutate(
      inwyr = coalesce(inwyr, inwyys),
      inwmm = coalesce(inwmm, inwmms),
      inwdd = coalesce(inwdd, inwdds)
    ) %>%
    select(-any_of(c("inwyys","inwmms","inwdds")))
  
  # ensure education columns exist; create unified "edulvl" (A/B)
  if (!"edulvla" %in% names(df)) df[["edulvla"]] <- NA
  if (!"edulvlb" %in% names(df)) df[["edulvlb"]] <- NA
  
  df <- df %>%
    mutate(edulvl = coalesce(edulvla, edulvlb)) %>%
    select(-any_of(c("edulvla","edulvlb")))
  
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

# Checks
diag_dates <- ess_raw_combined %>%
  group_by(essround) %>%
  summarise(
    n = n(),
    pct_na_inwyr = mean(is.na(inwyr)),
    pct_na_inwmm = mean(is.na(inwmm)),
    pct_na_inwdd = mean(is.na(inwdd))
  )
print(diag_dates)
