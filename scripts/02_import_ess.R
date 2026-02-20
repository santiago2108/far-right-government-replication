# ============================================================
# 02_import_ess.R
# ESS1–ESS8: import, keep required vars + party-vote vars, combine, save
# ============================================================

library(dplyr)
library(haven)

ESS_DIR  <- "data/raw/ess"
OUT_PATH <- "data/intermediate/ess_raw_combined.rds"

keep_vars <- c(
  # identifiers
  "cntry","idno","essround",
  
  # interview date (round-dependent)
  "inwyr","inwmm","inwdd",
  "inwyys","inwmms","inwdds",
  
  # vote + trust (core)
  "vote","trstprl","trstplt","trstprt",
  
  # immigration + authoritarian (paper/SPSS)
  "imbgeco","imueclt","imwbcnt",
  "ipfrule","ipstrgv","ipbhprp","imptrad","impsafe",
  
  # econ/redistribution
  "gincdif","stfeco",
  
  # demographics used in SPSS logic
  "agea","gndr","hincfel","mnactic","rlgdgr","polintr","domicil",
  
  # education inputs (A vs B)
  "edulvla","edulvlb",
  
  # left-right
  "lrscale"
)

read_one <- function(path) {
  df <- read_dta(path) %>%
    select(any_of(keep_vars), starts_with("prtvt"), starts_with("prtv"))
  
  # harmonize interview date into inwyr/inwmm/inwdd
  if (!("inwyr" %in% names(df))) df$inwyr <- NA
  if (!("inwmm" %in% names(df))) df$inwmm <- NA
  if (!("inwdd" %in% names(df))) df$inwdd <- NA
  
  if ("inwyys" %in% names(df)) df$inwyr <- coalesce(df$inwyr, df$inwyys)
  if ("inwmms" %in% names(df)) df$inwmm <- coalesce(df$inwmm, df$inwmms)
  if ("inwdds" %in% names(df)) df$inwdd <- coalesce(df$inwdd, df$inwdds)
  
  df <- df %>% select(-any_of(c("inwyys","inwmms","inwdds")))
  
  # unify education into one column edulvl (keep raw A/B inputs out of the way)
  if (!("edulvla" %in% names(df))) df$edulvla <- NA
  if (!("edulvlb" %in% names(df))) df$edulvlb <- NA
  
  df <- df %>%
    mutate(edulvl = coalesce(edulvla, edulvlb)) %>%
    select(-any_of(c("edulvla","edulvlb")))
  
  df
}

files <- file.path(ESS_DIR, paste0("ESS", 1:8, "_raw.dta"))

ess_raw_combined <- bind_rows(lapply(files, read_one))

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_raw_combined, OUT_PATH)