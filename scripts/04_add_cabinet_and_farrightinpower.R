# ============================================================
# 04_add_cabinet_and_farrightinpower.R
# ============================================================

library(dplyr)
library(readxl)

ESS_IN   <- "data/intermediate/ess_harmonized.rds"
PG_XLSX  <- "data/raw/author_materials/parlgov-new.xlsx"
GOV_XLSX <- "data/raw/author_materials/ESS government ID round ALL.xlsx"
OUT_PATH <- "data/intermediate/ess_with_cabinet.rds"

ess <- readRDS(ESS_IN)

cntry_map <- data.frame(
  cntry = c("AT","BE","CH","CZ","DE","DK","EE","ES","FI","FR","GB",
            "HU","IE","IT","NL","NO","PL","PT","SE","SI","SK","LV"),
  country_name_short = c("AUT","BEL","CHE","CZE","DEU","DNK","EST","ESP","FIN","FRA","GBR",
                         "HUN","IRL","ITA","NLD","NOR","POL","PRT","SWE","SVN","SVK","LVA"),
  stringsAsFactors = FALSE
)

pg_country <- read_excel(PG_XLSX, sheet = "cabinet") %>%
  select(country_name_short, country_id) %>%
  distinct()

ess <- ess %>%
  left_join(cntry_map, by = "cntry") %>%
  left_join(pg_country, by = "country_name_short")

cab_raw <- read_excel(GOV_XLSX, sheet = "ParlGov-cabinetid", col_names = FALSE)

rules <- cab_raw %>%
  transmute(
    country_id = suppressWarnings(as.integer(`...2`)),
    cutoff     = suppressWarnings(as.integer(`...4`)),
    cabinetid  = suppressWarnings(as.integer(`...6`))
  ) %>%
  filter(!is.na(country_id), !is.na(cutoff), !is.na(cabinetid)) %>%
  distinct(country_id, cutoff, cabinetid) %>%
  arrange(country_id, cutoff)

ess_cab <- ess %>%
  left_join(rules, by = "country_id") %>%
  filter(interviewdate > cutoff) %>%
  group_by(cntry, idno, essround, interviewdate) %>%
  slice_max(order_by = cutoff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-cutoff)

fr_raw <- read_excel(GOV_XLSX, sheet = "FarRight-in-power", col_names = FALSE)

fr_map <- fr_raw %>%
  transmute(cabinetid = suppressWarnings(as.integer(`...2`))) %>%
  filter(!is.na(cabinetid)) %>%
  distinct(cabinetid) %>%
  mutate(farrightpower = 1L)

ess_cab <- ess_cab %>%
  left_join(fr_map, by = "cabinetid") %>%
  mutate(farrightpower = ifelse(is.na(farrightpower), 0L, farrightpower))

dir.create(dirname(OUT_PATH), recursive = TRUE, showWarnings = FALSE)
saveRDS(ess_cab, OUT_PATH)
