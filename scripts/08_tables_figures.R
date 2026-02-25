# ============================================================
# 08_tables_figures.R
# Create the main regression table and key figure
#
# WHAT THIS SCRIPT DOES:
#   1. Loads the analysis dataset and the fitted models from script 07
#   2. Makes a regression table (Table 2 from the paper)
#   3. Makes a predicted probability plot (Figure 5 from the paper):
#      - x-axis: political trust (0-10)
#      - y-axis: predicted probability of voting far-right
#      - two lines: far-right IN power vs NOT in power
#
# INPUT:
#   data/final/analysis_dataset.rds       (built by script 06)
#   output/tables/replication_models.rds  (built by script 07)
#
# OUTPUTS:
#   output/tables/replication_table.html   (Word table)
#   output/figures/interaction_plot.png    (the key figure)
#   output/tables/descriptive_stats.html   (descriptive statistics)
# ============================================================


# ---- 0. PACKAGES ----

# The same packages from script 01 are used here, plus a few new ones.
# - modelsummary : makes publication-ready regression tables
# - ggplot2      : makes plots (part of tidyverse)
# - dplyr        : data wrangling (part of tidyverse)
# - scales       : formats axis labels (e.g. percentages)

library(dplyr)
library(ggplot2)
library(modelsummary)
library(scales)


# ---- 1. LOAD DATA AND MODELS ----

# Load the analysis dataset (built in script 06)
dat <- readRDS("data/final/analysis_dataset.rds")

# Load the list of fitted models (built in script 07).
# Script 07 saves models to output/tables/replication_models.rds.
# The list contains: "Model 1", "Model 2", "Model 3"
models <- readRDS("output/tables/replication_models.rds")

# Quick check: what models are loaded?
cat("Models loaded:", names(models), "\n")
cat("Dataset rows:", nrow(dat), "| Countries:", length(unique(dat$cntry)), "\n\n")


# ---- 2. REGRESSION TABLE ----

# modelsummary() takes a list of models and makes a formatted table.
# The following are customized:
#   - which statistics to show (odds ratios + confidence intervals)
#   - variable names replaced with readable labels
#   - a title added

# Variable labels: left side = name in model, right side = label in table
var_labels <- c(
  "trust_political"               = "Political trust (0-10)",
  "farrightpower"                 = "Far right in power",
  "trust_political:farrightpower" = "Political trust × Far right in power",
  "anti_immigration"              = "Anti-immigration attitudes",
  "authoritarian"                 = "Authoritarian values",
  "redistribution"                = "Redistribution preference",
  "bad_economy"                   = "Economic dissatisfaction",
  "age"                           = "Age",
  "female"                        = "Female",
  "educ"                          = "Education (0-4)",
  "subincome"                     = "Subjective income (0-3)",
  "unemployed"                    = "Unemployed",
  "religiosity"                   = "Religiosity (0-10)",
  "polinterest"                   = "Political interest (0-3)",
  "urban"                         = "Urban (1=yes)",
  "lrscale"                       = "Left-right placement (0-10)",
  "(Intercept)"                   = "Intercept"
)

dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)

# Make the table and save as a Word document.
# NOTE: requires the 'officer' package. If not installed, run:
#   install.packages("officer")
# HTML files can be opened directly in any browser
# for an HTML version that can be opened in a browser.
modelsummary(
  models,
  exponentiate = TRUE,            # show Odds Ratios (not log-odds)
  statistic    = "conf.int",      # show 95% confidence intervals
  coef_map     = var_labels,      # apply the nicer variable names
  stars        = TRUE,            # add * p<0.05, ** p<0.01, *** p<0.001
  gof_omit     = "AIC|BIC|Log|Deviance|RMSE",
  title        = "Table 1. Multilevel Logistic Regression of Far-Right Voting",
  notes        = "Odds ratios shown. 95% confidence intervals in brackets.",
  output       = "output/tables/replication_table.html"
)

cat("Regression table saved to output/tables/replication_table.html\n")


# ---- 3. PREDICTED PROBABILITY PLOT ----

# The key finding: the effect of political trust on far-right voting is
# WEAKER when the far right is in government.
#
# To show this visually:
#   Step 1: A "prediction grid" is created — a small data frame covering all
#           combinations of trust (0-10) and farrightpower (0 or 1),
#           with all other variables held at their mean or median.
#   Step 2: The model is used to predict probabilities for each row.
#   Step 3: Trust is plotted on the x-axis, predicted probability on the
#           y-axis, with separate lines for "in power" vs "not in power".

# --- Step 3a: Make the prediction grid ---

# The model uses the raw 0-10 trust variable (trust_political),
# so no standardization is needed. The grid covers the full 0-10 range.
# All other continuous controls are held at their mean from the data.
# Binary controls (female, urban) are held at their median (0 and 1).

prediction_grid <- expand.grid(
  trust_political = seq(0, 10, by = 0.5),  # trust from 0 to 10 in steps of 0.5
  farrightpower   = c(0, 1)                # both conditions: in power and not
)

# Add all other model variables, held at their mean/median
prediction_grid <- prediction_grid %>%
  mutate(
    anti_immigration = mean(dat$anti_immigration, na.rm = TRUE),
    authoritarian    = mean(dat$authoritarian,    na.rm = TRUE),
    redistribution   = mean(dat$redistribution,   na.rm = TRUE),
    bad_economy      = mean(dat$bad_economy,       na.rm = TRUE),
    age              = mean(dat$age,               na.rm = TRUE),
    educ             = mean(dat$educ,              na.rm = TRUE),
    subincome        = mean(dat$subincome,         na.rm = TRUE),
    religiosity      = mean(dat$religiosity,       na.rm = TRUE),
    polinterest      = mean(dat$polinterest,       na.rm = TRUE),
    lrscale          = mean(dat$lrscale,           na.rm = TRUE),
    female           = 0,   # held at median (male = most common)
    urban            = 1,   # held at median (urban = most common)
    unemployed       = 0,
    # ESS round held at round 5 (middle of the study period)
    essround         = factor(5, levels = levels(factor(dat$essround)))
  )

# --- Step 3b: Get predicted probabilities ---

# predict() on a glmer model gives log-odds by default.
# type = "response" converts them to probabilities (0-1 scale).
# re.form = NA ignores random effects, giving the "average country" prediction.
prediction_grid$predicted_prob <- predict(
  models[["Model 3"]],      # use Model 3 (the one WITH the interaction)
  newdata = prediction_grid,
  type    = "response",     # return probabilities, not log-odds
  re.form = NA              # ignore country random effects (average country)
)

# --- Step 3c: Make the plot ---

# farrightpower is turned into a readable label for the legend
prediction_grid <- prediction_grid %>%
  mutate(
    status = ifelse(farrightpower == 1,
                    "Far right in power",
                    "Far right in opposition")
  )

# Build the plot with ggplot2.
# ggplot works in layers:
#   ggplot()       sets up the axes (aes = "aesthetics")
#   geom_line()    draws the lines
#   labs()         adds titles and axis labels
#   theme_bw()     sets a clean black-and-white style

interaction_plot <- ggplot(
  data    = prediction_grid,
  mapping = aes(
    x        = trust_political,  # x-axis: political trust (0-10)
    y        = predicted_prob,   # y-axis: predicted probability
    color    = status,           # different color per line
    linetype = status            # different line type per line
  )
) +
  
  # Draw the lines
  geom_line(linewidth = 1) +
  
  # Labels for axes, title, and legend
  labs(
    title    = "Effect of Political Trust on Far-Right Voting",
    subtitle = "Predicted probabilities from Model 3 (other variables held at mean)",
    x        = "Political trust (0 = no trust, 10 = full trust)",
    y        = "Predicted probability of far-right vote",
    color    = "Far-right party status",
    linetype = "Far-right party status"
  ) +
  
  # Format the y-axis as percentages
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  
  # Colors: black for opposition, grey for in power
  scale_color_manual(values = c(
    "Far right in opposition" = "black",
    "Far right in power"      = "grey50"
  )) +
  
  # Line types: solid for opposition, dashed for in power
  scale_linetype_manual(values = c(
    "Far right in opposition" = "solid",
    "Far right in power"      = "dashed"
  )) +
  
  # Clean theme
  theme_bw(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title      = element_text(face = "bold")
  )

# Display the plot in RStudio
print(interaction_plot)

# --- Step 3d: Save the plot ---

dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "output/figures/interaction_plot.png",
  plot     = interaction_plot,
  width    = 7,
  height   = 5,
  dpi      = 300,
  device   = png   # use base R png device to avoid graphics API conflicts
)

cat("Interaction plot saved to output/figures/interaction_plot.png\n")


# ---- 4. DESCRIPTIVE STATISTICS TABLE ----

# A simple summary table of the main variables.
# Useful for the "Data" section of the paper.

desc_vars <- dat %>%
  select(
    fr_vote,
    trust_political,
    anti_immigration,
    authoritarian,
    farrightpower,
    age, female, educ
  )

# datasummary_skim() from modelsummary gives a quick summary of all variables
datasummary_skim(
  desc_vars,
  title  = "Table A1. Descriptive Statistics",
  output = "output/tables/descriptive_stats.html"
)

cat("Descriptive statistics saved to output/tables/descriptive_stats.html\n")

cat("\nScript 08 complete!\n")