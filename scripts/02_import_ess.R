# ============================================================
# 02_import_ess.R

# Goal: Load ESS rounds 1-8, keep only the variables I need,
# combine them into one dataset, and save it.
# ============================================================

# --- packages ---

library(haven)
library(dplyr)

# --- File paths ---
ESS_DIR  <- "data/raw/ess"                           # ESS_DIR: the folder where the ESS .dta files are stored
OUT_PATH <- "data/intermediate/ess_raw_combined.rds" # OUT_PATH: where to save the combined output file


# --- List the variables to keep from each ESS file ---
# Only keep what I actually need for the analysis

keep_vars <- c(
  # Who is the respondent and which round/country
  "cntry", "idno", "essround",
  
  # Interview date — ESS used different names across rounds:
  # inwyr/inwmm/inwdd = year/month/day in ESS 1-2
  # inwyys/inwmms/inwdds = year/month/day in ESS 3-8
  "inwyr", "inwmm", "inwdd",
  "inwyys", "inwmms", "inwdds",
  
  # Did they vote? Trust in parliament, politicians, parties
  "vote", "trstprl", "trstplt", "trstprt",
  
  # Immigration attitudes (3 items)
  "imbgeco", "imueclt", "imwbcnt",
  
  # Authoritarian values (5 items)
  "ipfrule", "ipstrgv", "ipbhprp", "imptrad", "impsafe",
  
  # Economic attitudes
  "gincdif", "stfeco",
  
  # Demographics and controls
  "agea", "gndr", "hincfel", "mnactic", "rlgdgr", "polintr", "domicil",
  
  # Education — the ESS used two different variable names across rounds
  "edulvla", "edulvlb",
  
  # Left-right self-placement
  "lrscale"
)


# --- Function: read and clean one ESS file ---
# Wrap the steps in a function so they can be reused for all rounds.
read_one_ess <- function(path) {
  
  # Step 1: Read the .dta file
  df <- read_dta(path)
  
  # Step 2: Keep only the variables listed above + all party-vote variables
  # starts_with("prtvt") and starts_with("prtv") catch country-specific
  # party-vote columns like prtvtgb, prtvtde2, etc.
  df <- select(df, any_of(keep_vars), starts_with("prtvt"), starts_with("prtv"))
  
  # Step 3: Harmonize the interview date columns
  # ESS 1-2 used inwyr/inwmm/inwdd; ESS 3-8 used inwyys/inwmms/inwdds.
  # Consistent set of columns (inwyr, inwmm, inwdd) for all rounds.
  
  # If the column doesn't exist yet, create it as NA (empty)
  if (!("inwyr" %in% names(df))) df$inwyr <- NA
  if (!("inwmm" %in% names(df))) df$inwmm <- NA
  if (!("inwdd" %in% names(df))) df$inwdd <- NA
  
  # coalesce() takes the first non-missing value across two columns.
  # So: if inwyr is missing but inwyys has a value, use inwyys.
  if ("inwyys" %in% names(df)) df$inwyr <- coalesce(df$inwyr, df$inwyys)
  if ("inwmms" %in% names(df)) df$inwmm <- coalesce(df$inwmm, df$inwmms)
  if ("inwdds" %in% names(df)) df$inwdd <- coalesce(df$inwdd, df$inwdds)
  
  # Drop the now-redundant "s" versions — only keeps inwyr/inwmm/inwdd
  df <- select(df, -any_of(c("inwyys", "inwmms", "inwdds")))
  
  # Step 4: Harmonize education into one column
  # Early rounds used edulvla; later rounds used edulvlb.
  # They are combines into a single column called edulvl.
  
  if (!("edulvla" %in% names(df))) df$edulvla <- NA
  if (!("edulvlb" %in% names(df))) df$edulvlb <- NA
  
  # Again: use edulvla if available, otherwise use edulvlb
  df$edulvl <- coalesce(df$edulvla, df$edulvlb)
  
  # Drop the two originals — edulvl is our clean combined version
  df <- select(df, -any_of(c("edulvla", "edulvlb")))
  
  return(df)
}


# --- Build the list of file paths for ESS 1 to 8 ---
# paste0() combines text: "data/raw/ess/ESS1_raw.dta", "data/raw/ess/ESS2_raw.dta", etc.
files <- file.path(ESS_DIR, paste0("ESS", 1:8, "_raw.dta"))


# --- Read and combine all 8 rounds ---
# lapply() runs read_one_ess() on each file and returns a list of 8 data frames.
# bind_rows() stacks them on top of each other into one big data frame.
ess_raw_combined <- bind_rows(lapply(files, read_one_ess))


# --- Quick checks ---
#cat("Rows:", nrow(ess_raw_combined), "\n")
#cat("Columns:", ncol(ess_raw_combined), "\n")
#cat("Rounds present:", sort(unique(ess_raw_combined$essround)), "\n")
#cat("Countries:", length(unique(ess_raw_combined$cntry)), "\n")


# --- Save the combined dataset ---
saveRDS(ess_raw_combined, OUT_PATH)
