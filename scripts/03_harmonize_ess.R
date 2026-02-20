# ============================================================
# 03_harmonize_ess.R
# Harmonize ESS1–ESS8 like paper/SPSS indices and save as ess_harmonized.rds
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

# Authoritarian items recode EXACTLY as SPSS:
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

# Build of interviewdate (YYYYMMDD) from harmonised inwyr/inwmm/inwdd
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
  )

# Required inputs for THIS script (paper-faithful)
need_items <- c(
  # trust
  "trstprl","trstplt","trstprt",
  # immigration
  "imbgeco","imueclt","imwbcnt",
  # authoritarian
  "ipfrule","ipstrgv","ipbhprp","imptrad","impsafe",
  # econ/redistribution
  "gincdif","stfeco",
  # demographics (SPSS)
  "agea","gndr","hincfel","mnactic","rlgdgr","polintr","domicil","edulvl",
  # other controls used later
  "lrscale"
)

missing_items <- setdiff(need_items, names(ess))
if (length(missing_items) > 0) {
  stop(
    "These required variables are missing from the combined data:\n",
    paste(missing_items, collapse = ", "),
    "\nFix 02_combine_ess_raw.R keep_vars (or check raw file variable names)."
  )
}

# ----- Education: translate SPSS block exactly -----
# SPSS:
# recode edulvlb (...) into edulvlbR.
# compute eduSC5=edulvla. then overwrite if edulvlbR exists.
# recode edusc5 (55=sysmis)
# RECODE edusc5 (1=0)(2=1)(3=2)(4=3)(5=4) into educ.

ess <- ess %>%
  mutate(
    edulvl_num = as.numeric(zap_labels(edulvl)),
    
    # edulvlbR mapping (applies only where edulvl has the detailed edulvlb codes)
    edulvlbR = case_when(
      edulvl_num %in% c(0, 113) ~ 1,
      edulvl_num %in% c(129, 212, 213, 221, 222, 223) ~ 2,
      edulvl_num %in% c(229, 311, 312, 313, 321, 322, 323) ~ 3,
      edulvl_num %in% c(412, 413, 421, 422, 423) ~ 4,
      edulvl_num %in% c(510, 520, 610, 620, 710, 720, 800) ~ 5,
      TRUE ~ NA_real_
    ),
    
    edusc5 = case_when(
      !is.na(edulvlbR) ~ edulvlbR,
      TRUE ~ edulvl_num
    ),
    
    # SPSS: recode edusc5 (55=sysmis)
    edusc5 = ifelse(edusc5 == 55, NA_real_, edusc5),
    
    # Final 0–4 version as in regression table
    educ = case_when(
      edusc5 == 1 ~ 0,
      edusc5 == 2 ~ 1,
      edusc5 == 3 ~ 2,
      edusc5 == 4 ~ 3,
      edusc5 == 5 ~ 4,
      TRUE ~ NA_real_
    )
  )

# ----- Indices + demographics (paper/SPSS-based) -----
ess <- ess %>%
  mutate(
    # Political trust scale: mean of trust in parliament, politicians, parties
    trust_political = row_mean_spss(cbind(
      as.numeric(zap_labels(trstprl)),
      as.numeric(zap_labels(trstplt)),
      as.numeric(zap_labels(trstprt))
    )),
    
    # Anti-immigration: mean of 3 recoded items
    anti_immigration = row_mean_spss(cbind(
      rec_antimmi(imbgeco),
      rec_antimmi(imueclt),
      rec_antimmi(imwbcnt)
    )),
    
    # Authoritarian sentiment: mean of 5 recoded items
    authoritarian = row_mean_spss(cbind(
      rec_1_6_to_5_0(ipfrule),
      rec_1_6_to_5_0(ipstrgv),
      rec_1_6_to_5_0(ipbhprp),
      rec_1_6_to_5_0(imptrad),
      rec_1_6_to_5_0(impsafe)
    )),
    
    redistribution = rev_1_5(gincdif),
    bad_economy    = rev_0_10(stfeco),
    
    # demographics (SPSS)
    age = as.numeric(zap_labels(agea)),
    
    female = case_when(
      as.numeric(zap_labels(gndr)) == 2 ~ 1,
      as.numeric(zap_labels(gndr)) == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # SPSS: subincome from hincfel (1->3, 2->2, 3->1, 4->0)
    subincome = case_when(
      as.numeric(zap_labels(hincfel)) == 1 ~ 3,
      as.numeric(zap_labels(hincfel)) == 2 ~ 2,
      as.numeric(zap_labels(hincfel)) == 3 ~ 1,
      as.numeric(zap_labels(hincfel)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # SPSS: unemployed dummy from mnactic (3–4 = 1, others = 0)
    mnactic_num = as.numeric(zap_labels(mnactic)),
    unemployed = case_when(
      mnactic_num %in% c(3, 4) ~ 1,
      mnactic_num %in% c(1, 2, 5, 6, 7, 8, 9) ~ 0,
      TRUE ~ NA_real_
    ),
    
    # SPSS: religiosity is 0–10
    religiosity = case_when(
      as.numeric(zap_labels(rlgdgr)) %in% c(77, 88, 99) ~ NA_real_,
      TRUE ~ as.numeric(zap_labels(rlgdgr))
    ),
    
    # SPSS: political interest recode (1->3, 2->2, 3->1, 4->0)
    polinterest = case_when(
      as.numeric(zap_labels(polintr)) == 1 ~ 3,
      as.numeric(zap_labels(polintr)) == 2 ~ 2,
      as.numeric(zap_labels(polintr)) == 3 ~ 1,
      as.numeric(zap_labels(polintr)) == 4 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # SPSS: urban dummy (domicil 1–3 = 1, 4–5 = 0)
    urban = case_when(
      as.numeric(zap_labels(domicil)) %in% 1:3 ~ 1,
      as.numeric(zap_labels(domicil)) %in% 4:5 ~ 0,
      TRUE ~ NA_real_
    ),
    
    lrscale = as.numeric(zap_labels(lrscale))
  )

# Final dataset 
ess_harmonized <- ess %>%
  select(
    idno, cntry, essround, interviewdate,
    trust_political, anti_immigration, authoritarian,
    redistribution, bad_economy,
    age, female, educ, subincome, unemployed, religiosity, polinterest, urban,
    lrscale
  )

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_harmonized, OUT_PATH)

cat("Saved harmonized file to:\n", OUT_PATH, "\n\n")

# Checks
cat("Missing interviewdate by round:\n")
print(ess_harmonized %>% group_by(essround) %>% summarise(pct_na_interviewdate = mean(is.na(interviewdate))))

cat("\nEducation (educ) distribution:\n")
print(table(ess_harmonized$educ, useNA = "ifany"))

cat("\nMissing key demographics overall:\n")
print(ess_harmonized %>%
        summarise(
          pct_na_subincome   = mean(is.na(subincome)),
          pct_na_unemployed  = mean(is.na(unemployed)),
          pct_na_religiosity = mean(is.na(religiosity)),
          pct_na_polinterest = mean(is.na(polinterest)),
          pct_na_urban       = mean(is.na(urban)),
          pct_na_educ        = mean(is.na(educ))
        )
)

cat("\nMissing indices overall:\n")
print(ess_harmonized %>%
        summarise(
          pct_na_trust = mean(is.na(trust_political)),
          pct_na_antiimm = mean(is.na(anti_immigration)),
          pct_na_auth = mean(is.na(authoritarian))
        )
)

