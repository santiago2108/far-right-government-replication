# far-right-government-replication

Replication and extension of:
Muis, J., Brils, T., & Gaidytė, T. (2022). Arrived in Power, and Yet Still Disgruntled? How Government Inclusion Moderates “Protest Voting” for Far-Right Populist Parties in Europe. Government & Opposition.

## Purpose
This repository documents a term paper project for the LMU seminar “Research Design (WS 2025)”.
Goals:
1) Replicate the central regression analysis (main interaction effect).
2) Extend the analysis by adding immigration issue salience.
3) Extend the analysis by including newer ESS waves (Rounds 9–10).

## Data sources (public)
- European Social Survey (ESS), Rounds 1–10
- ParlGov database
- The PopuList (cross-check of party classification)

## Author-provided materials
The original authors did not publish an R replication script. Variable construction is reconstructed using SPSS syntax and auxiliary files shared by the corresponding author (stored locally under `data/raw/author_materials/`). All data preparation and analysis scripts in this repository are written in R.

## Folder structure
- `data/raw/`: raw input data (not tracked if large / restricted)
- `data/intermediate/`: cleaned/harmonized datasets
- `data/final/`: final analysis dataset
- `scripts/`: R scripts (run in numerical order)
- `output/`: tables and figures
- `paper/`: handout, paper draft, appendix, references

## How to run
1. Open the RStudio project file (`.Rproj`).
2. Run scripts in `scripts/` in numerical order (01 → 09).
3. Outputs are written to `output/`.

## Notes on reproducibility
- Raw survey microdata (ESS) may be subject to download terms; therefore it is not committed to this repository.
- Key decisions (variable harmonization, party coding, and merges) are documented in code comments and the appendix.
