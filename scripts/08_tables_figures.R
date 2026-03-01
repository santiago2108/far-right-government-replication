# ============================================================
# 08_tables_figures.R
# Create the main regression table and key figure
#
# WHAT THIS SCRIPT DOES:
#   1. Loads the analysis dataset and the fitted models from script 07
#   2. Makes a regression table (Table 2 from the paper)
#   3. Makes a predicted probability plot (Figure 5 from the paper):
#      - x-axis: political trust (0-10, back-transformed from standardized)
#      - y-axis: predicted probability of voting far-right
#      - two lines: far-right IN power vs NOT in power
#
# IMPORTANT: all predictors in script 07 are standardized (_z suffix).
# The random grouping variable is "period" (country-period), not "cntry".
# There is no lrscale in the models.
#
# OUTPUTS:
#   output/tables/replication_table.html
#   output/figures/interaction_plot.png
#   output/tables/descriptive_stats.html
# ============================================================


# ---- 0. PACKAGES ----

library(dplyr)        # data wrangling
library(ggplot2)      # plots
library(modelsummary) # regression tables
library(scales)       # percent formatting on y-axis


# ---- 1. LOAD DATA AND MODELS ----

dat    <- readRDS("data/final/analysis_dataset.rds")
models <- readRDS("output/tables/replication_models.rds")

cat("Models loaded:", names(models), "\n")
cat("Dataset rows:", nrow(dat), "| Columns:", ncol(dat), "\n")
cat("Column names:\n")
print(names(dat))


# ---- 2. REGRESSION TABLE ----

# These labels must match the exact variable names used in script 07.
# All continuous predictors end in _z because script 07 standardized them.
var_labels <- c(
  "trust_political_z"                = "Political trust",
  "farrightpower"                    = "Far right in power",
  "trust_political_z:farrightpower"  = "Political trust × Far right in power",
  "anti_immigration_z"               = "Anti-immigration attitudes",
  "authoritarian_z"                  = "Authoritarian values",
  "redistribution_z"                 = "Redistribution preference",
  "bad_economy_z"                    = "Economic dissatisfaction",
  "age_z"                            = "Age",
  "female_z"                         = "Female",
  "educ_z"                           = "Education",
  "subincome_z"                      = "Subjective income",
  "religiosity_z"                    = "Religiosity",
  "polinterest_z"                    = "Political interest",
  "urban_z"                          = "Urban",
  "(Intercept)"                      = "Intercept"
)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

modelsummary(
  models,
  exponentiate = TRUE,       # show Odds Ratios instead of log-odds
  statistic    = "conf.int", # show 95% confidence intervals
  coef_map     = var_labels,
  stars        = TRUE,
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  title        = "Table 1. Multilevel Logistic Regression of Far-Right Voting (Odds Ratios)",
  notes        = "Odds ratios. 95% CI in brackets. Random intercept by country-period.",
  output       = "output/tables/replication_table.html"
)

cat("Regression table saved to output/tables/replication_table.html\n")


# ---- 3. PREDICTED PROBABILITY PLOT ----

# Script 07 uses three separate datasets (df_m1, df_m2, df_m3) with their own
# listwise deletion. We recreate df_m3 here (the Model 3 sample) so our means
# are computed on exactly the same rows the model was fitted on.

vars_m3 <- c(
  "fr_vote",
  "trust_political_z", "farrightpower",
  "anti_immigration_z", "authoritarian_z", "redistribution_z", "bad_economy_z",
  "age_z", "female_z", "educ_z", "subincome_z",
  "religiosity_z", "polinterest_z", "urban_z",
  "essround", "period"
)

# Check all needed variables exist before trying to use them
missing_vars <- setdiff(vars_m3, names(dat))
if (length(missing_vars) > 0) {
  stop("These variables are missing from analysis_dataset.rds:\n",
       paste(missing_vars, collapse = ", "),
       "\nCheck script 06.")
}

df_m3 <- dat %>%
  filter(if_all(all_of(vars_m3), ~ !is.na(.))) %>%
  mutate(
    fr_vote       = as.integer(fr_vote),
    farrightpower = as.integer(farrightpower),
    essround      = as.factor(essround),
    period        = as.factor(period)
  )

cat("\nModel 3 analytic sample: N =", nrow(df_m3), "\n")


# --- Build the prediction grid ---

# The model uses trust_political_z (the standardized version).
# To show a meaningful x-axis (0-10 original scale), we:
#   1. Vary trust_political_z across its realistic range
#   2. Also store the back-transformed original value for the x-axis label
#
# The standardization formula was: z = (x - mean) / sd
# So to go back: x = z * sd + mean

trust_mean <- mean(df_m3$trust_political_z, na.rm = TRUE)  # should be ~0 after standardizing
trust_sd   <- sd(df_m3$trust_political_z,   na.rm = TRUE)  # should be ~1 after standardizing

# We create z-values that correspond to original trust scores 0-10.
# But since the variable IS the z-score, we just vary it across its observed range.
# seq() from min to max gives us a smooth curve.
trust_z_range <- seq(
  from = min(df_m3$trust_political_z, na.rm = TRUE),
  to   = max(df_m3$trust_political_z, na.rm = TRUE),
  length.out = 50   # 50 evenly-spaced points = smooth line
)

# expand.grid() creates every combination of trust_z and farrightpower
prediction_grid <- expand.grid(
  trust_political_z = trust_z_range,
  farrightpower     = c(0, 1)
)

# Add all other variables held at their mean (= 0, since they are standardized)
# Standardized variables have mean = 0 by definition, so this is easy.
prediction_grid <- prediction_grid %>%
  mutate(
    anti_immigration_z = 0,   # mean of a standardized variable is always 0
    authoritarian_z    = 0,
    redistribution_z   = 0,
    bad_economy_z      = 0,
    age_z              = 0,
    female_z           = 0,
    educ_z             = 0,
    subincome_z        = 0,
    religiosity_z      = 0,
    polinterest_z      = 0,
    urban_z            = 0,
    
    # period and essround are needed by the formula but will be ignored
    # because we set re.form = NA (see below)
    essround = levels(df_m3$essround)[1],
    period   = levels(df_m3$period)[1]
  )


# --- Get predicted probabilities ---

# type = "response" gives probabilities (0-1) instead of log-odds
# re.form = NA ignores country-period random effects
#           = we get the prediction for a "typical" country-period
prediction_grid$predicted_prob <- predict(
  models[["Model 3"]],
  newdata = prediction_grid,
  type    = "response",
  re.form = NA
)


# --- Prepare for plotting ---

# We want the x-axis to show the original 0-10 trust scale, not z-scores.
# We need to know the original mean and SD to back-transform.
# IMPORTANT: trust_political_z was created from trust_political in script 06.
# We need those original values. Check if trust_political exists in dat.

if ("trust_political" %in% names(dat)) {
  # Use the original variable to get the true mean and SD
  orig_mean <- mean(dat$trust_political, na.rm = TRUE)
  orig_sd   <- sd(dat$trust_political,   na.rm = TRUE)
  
  # Back-transform: original = z * sd + mean
  prediction_grid <- prediction_grid %>%
    mutate(trust_original = trust_political_z * orig_sd + orig_mean)
  
  x_var   <- "trust_original"
  x_label <- "Political trust (0 = no trust, 10 = complete trust)"
  cat("Using original trust scale (0-10) for x-axis.\n")
  
} else {
  # If original variable not available, just use the z-score on x-axis
  prediction_grid <- prediction_grid %>%
    mutate(trust_original = trust_political_z)
  
  x_var   <- "trust_original"
  x_label <- "Political trust (standardized)"
  cat("Original trust_political not found — using z-score for x-axis.\n")
}

# Add readable legend label
prediction_grid <- prediction_grid %>%
  mutate(
    status = ifelse(
      farrightpower == 1,
      "Far right in power",
      "Far right in opposition"
    )
  )


# --- Build the plot ---

interaction_plot <- ggplot(
  data    = prediction_grid,
  mapping = aes(
    x        = trust_original,
    y        = predicted_prob,
    color    = status,
    linetype = status
  )
) +
  geom_line(size = 1) +
  labs(
    title    = "Effect of Political Trust on Far-Right Voting",
    subtitle = "Predicted probabilities from Model 3 (all other variables at their mean)",
    x        = x_label,
    y        = "Predicted probability of far-right vote",
    color    = "Far-right party status",
    linetype = "Far-right party status"
  ) +
  scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +  scale_color_manual(values = c(
    "Far right in opposition" = "black",
    "Far right in power"      = "grey50"
  )) +
  scale_linetype_manual(values = c(
    "Far right in opposition" = "solid",
    "Far right in power"      = "dashed"
  )) +
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

print(interaction_plot)

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Instead of ggsave(), use base R's png device
png(
  filename = "output/figures/interaction_plot.png",
  width    = 7,
  height   = 5,
  units    = "in",
  res      = 300
)
print(interaction_plot)
dev.off()


# ---- 4. DESCRIPTIVE STATISTICS TABLE ----

# Select the original (non-standardized) versions where available,
# since those are more interpretable in a descriptive table.
# We use any_of() so it doesn't crash if a column is missing.

desc_vars <- dat %>%
  select(any_of(c(
    "fr_vote", "trust_political", "farrightpower",
    "anti_immigration", "authoritarian",
    "age", "female", "educ"
  )))

datasummary_skim(
  desc_vars,
  title  = "Table A1. Descriptive Statistics (full dataset)",
  output = "output/tables/descriptive_stats.html"
)

cat("Descriptive statistics saved to output/tables/descriptive_stats.html\n")
cat("\nScript 08 complete!\n")