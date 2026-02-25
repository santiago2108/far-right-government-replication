# ============================================================
# 07_replication_models.R
#
# Replication of:
#   Muis, Brils & Gaidytė (2022), Government & Opposition
#   "Arrived in Power, and Yet Still Disgruntled?"
#
# PURPOSE:
#   Load the analysis dataset built by scripts 01-06 and fit
#   the three multilevel logistic regression models from the paper.
#
# This script assumes scripts 01-06 have already been run and
# that data/final/analysis_dataset.rds exists.
#
# Input:  data/final/analysis_dataset.rds
# Output: output/tables/replication_models.txt
#         output/tables/replication_models.rds
# ============================================================

library(dplyr)
library(lme4)
library(modelsummary)

# ---- File paths -------------------------------------------------------
IN_PATH <- "data/final/analysis_dataset.rds"
OUT_DIR <- "output/tables"
OUT_TXT <- file.path(OUT_DIR, "replication_models.txt")
OUT_RDS <- file.path(OUT_DIR, "replication_models.rds")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)


# ============================================================
# SECTION 1: LOAD AND PREPARE THE MODELLING SAMPLES
# ============================================================
# Each model uses only the variables it actually needs.
# This means listwise deletion is applied separately per model,
# so Model 1 is not penalised for missingness on the policy
# motivation variables that only appear in Models 2 and 3.
#
# Note: unemployed_z is excluded from all models to match Table 2
# exactly. The paper mentions unemployment in its methods text but
# does not show it as a row in Table 2. It is kept in the dataset
# (script 06) so it can be added back easily as a robustness check.

df <- readRDS(IN_PATH)
cat("Loaded analysis dataset: N =", nrow(df), "| Countries:", length(unique(df$cntry)), "\n")

# --- Shared base variables (needed by every model) ---
vars_base <- c(
  "fr_vote", "trust_political_z",
  "age_z", "female_z", "educ_z", "subincome_z",
  "religiosity_z", "polinterest_z", "urban_z",
  "essround", "period"
)

# --- Additional variables for Models 2 and 3 ---
vars_policy <- c(
  "anti_immigration_z", "authoritarian_z",
  "redistribution_z", "bad_economy_z"
)

# --- Additional variables for Model 3 only ---
vars_m3 <- c("farrightpower")

# Check all variables exist in the dataset before proceeding.
# setdiff() returns anything in the first list that is NOT in the second.
all_needed <- unique(c(vars_base, vars_policy, vars_m3))
missing_vars <- setdiff(all_needed, names(df))
if (length(missing_vars) > 0) {
  stop("Missing variables in analysis dataset: ",
       paste(missing_vars, collapse = ", "),
       "\nCheck that scripts 01-06 ran correctly.")
}

# Helper function: apply listwise deletion for a given set of variables,
# then convert key columns to the types glmer() expects.
# as.integer() gives 0/1 for the binary outcome and context variable.
# as.factor() turns essround and period into categorical variables —
# essround becomes wave dummies, period becomes the grouping variable.
make_sample <- function(data, vars) {
  data %>%
    filter(if_all(all_of(vars), ~ !is.na(.))) %>%
    mutate(
      fr_vote       = as.integer(fr_vote),
      farrightpower = if ("farrightpower" %in% names(.)) as.integer(farrightpower) else NA_integer_,
      essround      = as.factor(essround),
      period        = as.factor(period)
    )
}

# Build one dataset per model — each contains only complete cases
# for the variables that model actually uses
df_m1 <- make_sample(df, vars_base)
df_m2 <- make_sample(df, c(vars_base, vars_policy))
df_m3 <- make_sample(df, c(vars_base, vars_policy, vars_m3))

cat("\nSample sizes (complete cases per model):\n")
cat("  Model 1 N =", nrow(df_m1), "\n")
cat("  Model 2 N =", nrow(df_m2), "\n")
cat("  Model 3 N =", nrow(df_m3), "\n")
cat("  (Paper reports N1 = 131,934 for Models 1-4)\n")
cat("\nfr_vote distribution in Model 3 sample:\n")
print(table(df_m3$fr_vote))


# ============================================================
# SECTION 2: FIT THE THREE MODELS
# ============================================================
# glmer()                    = generalised linear mixed effects regression
# family = binomial("logit") = logistic regression (outcome is 0/1)
# (1 | period)               = random intercept by country-period.
#                              A country-period is one country during one
#                              specific cabinet term (e.g. Netherlands under
#                              Rutte II). This is the level-2 unit the paper
#                              reports as N2 = 139 country-periods.
# + essround                 = ESS round fixed effects (wave dummies).
#                              Because essround is a factor, R automatically
#                              creates one dummy per round, with round 1
#                              as the baseline. This controls for any
#                              general time trend across the 8 survey waves.
# bobyqa optimizer           = most reliable for these models; 200k iterations
#                              to ensure convergence.
#
# All _z variables are the standardized versions built in script 06.
# The three models follow Table 2 of the paper exactly:
#   Model 1 = trust + sociodemographic controls + wave dummies
#   Model 2 = Model 1 + policy-related motivations
#   Model 3 = Model 2 + far right in power + interaction with trust

cat("\n--- Fitting Model 1 (trust + sociodemographic controls only) ---\n")
m1 <- glmer(
  fr_vote ~
    trust_political_z +                            # key predictor: political trust
    age_z + female_z + educ_z + subincome_z +      # sociodemographic controls
    religiosity_z + polinterest_z + urban_z +      # (Table 2 controls exactly)
    essround +                                     # ESS wave fixed effects
    (1 | period),                                  # random intercept by country-period
  data    = df_m1,                                 # complete cases for m1 vars only
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 1 done.\n")

cat("\n--- Fitting Model 2 (+ policy-related motivations) ---\n")
m2 <- glmer(
  fr_vote ~
    trust_political_z +
    anti_immigration_z + authoritarian_z +         # policy-related motivations
    redistribution_z + bad_economy_z +             # added in Model 2
    age_z + female_z + educ_z + subincome_z +
    religiosity_z + polinterest_z + urban_z +
    essround +
    (1 | period),
  data    = df_m2,                                 # complete cases for m2 vars
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 2 done.\n")

cat("\n--- Fitting Model 3 (+ far right in power + interaction) ---\n")
# The * operator is shorthand in R:
#   trust_political_z * farrightpower expands to:
#   trust_political_z + farrightpower + trust_political_z:farrightpower
# The colon term (:) is the interaction. It tests whether the effect of
# trust changes depending on whether the far right is in power.
# The paper's hypothesis: trust matters less as a barrier to far-right
# voting when the far right is already in government. This would appear
# as a positive interaction coefficient (odds ratio > 1).
m3 <- glmer(
  fr_vote ~
    trust_political_z * farrightpower +            # main effects + interaction
    anti_immigration_z + authoritarian_z +
    redistribution_z + bad_economy_z +
    age_z + female_z + educ_z + subincome_z +
    religiosity_z + polinterest_z + urban_z +
    essround +
    (1 | period),
  data    = df_m3,                                 # complete cases for m3 vars
  family  = binomial(link = "logit"),
  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))
)
cat("Model 3 done.\n")


# ============================================================
# SECTION 3: CHECK THE KEY RESULT
# ============================================================
# The paper's central finding: the interaction term is positive
# and significant, meaning trust matters less against far-right
# voting when the far right is already in government.

cat("\n=== KEY RESULT: INTERACTION TERM (Model 3) ===\n")
m3_coefs <- fixef(m3)   # fixef() extracts the fixed effect coefficients
int_name <- "trust_political_z:farrightpower"

if (int_name %in% names(m3_coefs)) {
  b  <- m3_coefs[int_name]
  or <- exp(b)
  cat(sprintf("Coefficient (log-odds): %+.4f\n", b))
  cat(sprintf("Odds ratio:              %.4f\n",  or))
  if (b > 0) {
    cat("Direction: POSITIVE — consistent with the paper's hypothesis.\n")
    cat("Interpretation: when far-right is in power, higher trust is LESS\n")
    cat("protective against voting far-right (protest-voting weakens).\n")
  } else {
    cat("Direction: NEGATIVE — does NOT match the paper's hypothesis.\n")
  }
} else {
  cat("WARNING: interaction term not found in Model 3.\n")
}


# ============================================================
# SECTION 4: RESULTS TABLE
# ============================================================

model_list <- list("Model 1" = m1, "Model 2" = m2, "Model 3" = m3)

# Human-readable labels for the table
coef_labels <- c(
  "trust_political_z"                  = "Political trust",
  "farrightpower"                      = "Far right in power (0/1)",
  "trust_political_z:farrightpower"    = "Trust x Far right in power",
  "anti_immigration_z"                 = "Anti-immigration attitudes",
  "authoritarian_z"                    = "Authoritarian values",
  "redistribution_z"                   = "Redistribution preference",
  "bad_economy_z"                      = "Economic dissatisfaction",
  "age_z"                              = "Age",
  "female_z"                           = "Female",
  "educ_z"                             = "Education",
  "subincome_z"                        = "Subjective income",
  "religiosity_z"                      = "Religiosity",
  "polinterest_z"                      = "Political interest",
  "urban_z"                            = "Urban (1=yes)",
  "(Intercept)"                        = "Intercept"
)

# Print to console
cat("\n=== MODEL TABLE (Odds Ratios + 95% CI) ===\n")
msummary(
  model_list,
  exponentiate = TRUE,        # show odds ratios instead of log-odds
  statistic    = "conf.int",  # show 95% confidence intervals
  coef_map     = coef_labels,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  title        = "Replication of Muis et al. (2022) – Multilevel Logistic Regression (OR)"
)

# Save table to text file
sink(OUT_TXT)
cat("Replication of Muis, Brils & Gaidyte (2022)\n")
cat("Multilevel Binary Logistic Regression — Odds Ratios\n")
cat("DV: Far-right vote (1 = voted far right, 0 = other party or blank vote)\n")
cat("Random intercept by country-period; ESS round dummies included\n")
cat(sprintf("Model 1 N = %d | Model 2 N = %d | Model 3 N = %d\n\n",
            nrow(df_m1), nrow(df_m2), nrow(df_m3)))
msummary(
  model_list,
  exponentiate = TRUE,
  statistic    = "conf.int",
  coef_map     = coef_labels,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  output       = "markdown"
)
sink()
cat("\nModel table saved to:", OUT_TXT, "\n")


# ============================================================
# SECTION 5: RANDOM EFFECTS SUMMARY
# ============================================================
# The random intercept variance tells us how much country-periods
# differ in their baseline far-right vote probability, after controls.
# A larger variance = country-periods differ more from each other.

cat("\n=== RANDOM EFFECTS: country-period variance ===\n")
for (nm in names(model_list)) {
  vc  <- VarCorr(model_list[[nm]])
  var <- as.numeric(vc$period)
  cat(sprintf("%s — country-period intercept variance: %.4f\n", nm, var))
}

# Save model objects (used by figures/tables scripts downstream)
saveRDS(model_list, OUT_RDS)
cat("\nModel objects saved to:", OUT_RDS, "\n")
cat("\nDone. Script 07 complete.\n")