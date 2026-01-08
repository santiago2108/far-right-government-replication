# ============================================================
# 03_harmonize_ess.R
# Harmonise ESS1–ESS8 to paper/SPSS indices and save ess_harmonized.rds
# ============================================================

library(dplyr)
library(haven)

RAW_ESS_PATH <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/intermediate/ess_raw_combined.rds"
OUT_PATH     <- "/home/santiagocal09/LMU/ResearchDesign_WS2526/term_paper/data/intermediate/ess_harmonized.rds"

# SPSS MEAN(): mean of available items, NA only if all missing
row_mean_spss <- function(mat) {
  out <- rowMeans(mat, na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

# Anti-immigration recode EXACTLY as SPSS:
# (0=10) (1=9) (2=8) (3=7) (4=6) (5...8=2) (9=1) (10=0) else NA
rec_antimmi <- function(x) {
  x <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  out[x == 0]  <- 10
  out[x == 1]  <- 9
  out[x == 2]  <- 8
  out[x == 3]  <- 7
  out[x == 4]  <- 6
  out[x >= 5 & x <= 8] <- 2
  out[x == 9]  <- 1
  out[x == 10] <- 0
  out
}

# Authoritarian sentiment recode EXACTLY as SPSS:
# (1=5) (2=4) (3=3) (4=2) (5=1) (6=0) else NA
rec_1_6_to_5_0 <- function(x) {
  x <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok <- x %in% 1:6
  out[ok] <- 6 - x[ok]
  out
}

# Reverse 0–10 -> 10–0
rev_0_10 <- function(x) {
  x <- as.numeric(zap_labels(x))
  ifelse(is.na(x), NA_real_, 10 - x)
}

# Reverse 1–5 -> 4–0
rev_1_5 <- function(x) {
  x <- as.numeric(zap_labels(x))
  out <- rep(NA_real_, length(x))
  ok <- x %in% 1:5
  out[ok] <- 5 - x[ok]
  out
}

ess <- readRDS(RAW_ESS_PATH)

# Countries used in paper (22)
countries_used <- c(
  "AT","BE","CH","CZ","DE","DK","EE","ES","FI","FR","GB",
  "HU","IE","IT","NL","NO","PL","PT","SE","SI","SK","LV"
)
ess <- ess %>% filter(cntry %in% countries_used)

# Build interviewdate (YYYYMMDD) from harmonised inwyr/inwmm/inwdd
ess <- ess %>%
  mutate(
    inwyr = as.numeric(zap_labels(inwyr)),
    inwmm = as.numeric(zap_labels(inwmm)),
    inwdd = as.numeric(zap_labels(inwdd)),
    interviewdate = ifelse(
      !is.na(inwyr) & !is.na(inwmm) & !is.na(inwdd),
      inwyr * 10000 + inwmm * 100 + inwdd,
      NA_real_
    )
  ) %>%
  group_by(cntry, essround) %>%
  mutate(
    interviewdate = ifelse(
      is.na(interviewdate),
      suppressWarnings(min(interviewdate, na.rm = TRUE)),
      interviewdate
    ),
    interviewdate = ifelse(is.infinite(interviewdate), NA_real_, interviewdate)
  ) %>%
  ungroup()

# If any underlying item is missing entirely, stop with a clear message.
need_items <- c(
  "trstprl","trstplt","trstprt",
  "imbgeco","imueclt","imwbcnt",
  "ipfrule","ipstrgv","ipbhprp","imptrad","impsafe",
  "gincdif","stfeco",
  "agea","gndr","eduyrs","uempla","hinctnta","lrscale","vote"
)
missing_items <- setdiff(need_items, names(ess))
if (length(missing_items) > 0) {
  stop(
    "These required variables are missing from the combined data:\n",
    paste(missing_items, collapse = ", "),
    "\nFix 02_import_ess.R keep_vars (or check raw file variable names)."
  )
}

# Compute indices + controls
ess <- ess %>%
  mutate(
    trust_political = row_mean_spss(cbind(
      as.numeric(zap_labels(trstprl)),
      as.numeric(zap_labels(trstplt)),
      as.numeric(zap_labels(trstprt))
    )),
    
    anti_immigration = row_mean_spss(cbind(
      rec_antimmi(imbgeco),
      rec_antimmi(imueclt),
      rec_antimmi(imwbcnt)
    )),
    
    authoritarian = row_mean_spss(cbind(
      rec_1_6_to_5_0(ipfrule),
      rec_1_6_to_5_0(ipstrgv),
      rec_1_6_to_5_0(ipbhprp),
      rec_1_6_to_5_0(imptrad),
      rec_1_6_to_5_0(impsafe)
    )),
    
    redistribution = rev_1_5(gincdif),
    bad_economy    = rev_0_10(stfeco),
    
    # Controls
    age       = as.numeric(zap_labels(agea)),
    female    = case_when(
      as.numeric(zap_labels(gndr)) == 2 ~ 1,
      as.numeric(zap_labels(gndr)) == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    education = as.numeric(zap_labels(eduyrs)),
    
    uempla_num = as.numeric(zap_labels(uempla)),
    unemployed = case_when(
      uempla_num == 1 ~ 1,
      uempla_num %in% c(0, 2) ~ 0,
      TRUE ~ NA_real_
    ),
    
    income  = as.numeric(zap_labels(hinctnta)),
    lrscale = as.numeric(zap_labels(lrscale)),
    
    turnout = case_when(
      as.numeric(zap_labels(vote)) == 1 ~ 1,
      as.numeric(zap_labels(vote)) == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

# Final dataset
ess_harmonized <- ess %>%
  select(
    cntry, essround, interviewdate,
    trust_political, anti_immigration, authoritarian, redistribution, bad_economy,
    age, female, education, unemployed, income, lrscale, turnout
  )

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_harmonized, OUT_PATH)

cat("Saved harmonized file to:\n", OUT_PATH, "\n\n")

# Diagnostics
cat("Missing interviewdate by round:\n")
print(ess_harmonized %>% group_by(essround) %>% summarise(pct_na_interviewdate = mean(is.na(interviewdate))))

cat("\nMissing unemployed overall:\n")
print(ess_harmonized %>% summarise(pct_na_unemployed = mean(is.na(unemployed))))

cat("\nMissing anti_immigration by round:\n")
print(ess_harmonized %>% group_by(essround) %>% summarise(pct_na_antiimm = mean(is.na(anti_immigration))))

cat("\nMissing authoritarian by round:\n")
print(ess_harmonized %>% group_by(essround) %>% summarise(pct_na_auth = mean(is.na(authoritarian))))
