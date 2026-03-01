# far-right-government-replication

Replication of:
Muis, J., Brils, T., & Gaidytė, T. (2022). *Arrived in Power, and Yet Still Disgruntled? How Government Inclusion Moderates “Protest Voting” for Far-Right Populist Parties in Europe.* Government & Opposition.

## Purpose
This repository documents a term paper project for the LMU seminar “15106 Research Design: Causal and Descriptive Analyses Using R”.

Goal:
- Replicate the central regression analysis (main interaction effect) reported in the original study.

## Data sources
- European Social Survey (ESS), Rounds 1–8
- ParlGov database
- The PopuList (cross-check of party classification)

## ESS license and attribution
This repository includes European Social Survey (ESS) microdata. ESS data are licensed under **Creative Commons Attribution–NonCommercial–ShareAlike 4.0 International (CC BY-NC-SA 4.0)**.

- **Attribution (BY):** Please cite ESS correctly when using these data.
- **NonCommercial (NC):** ESS data may not be used for commercial purposes.
- **ShareAlike (SA):** If you share adapted material, you must do so under the same license terms.

Any files produced by recoding, harmonisation, or merging should be treated as adapted material derived from ESS data and shared accordingly under CC BY-NC-SA 4.0.

(For the authoritative ESS license statement, see the ESS data disclaimer page on the ESS website. For license terms, see the Creative Commons CC BY-NC-SA 4.0 legal code.)

## Author-provided materials
The original authors did not publish an R replication script. Variable construction is reconstructed using SPSS syntax and auxiliary files shared by the corresponding author.

Author-provided materials are stored under:
- `data/raw/author_materials/`

## Folder structure
- `data/raw/`: raw input data (ESS + external datasets + author materials)
- `data/intermediate/`: cleaned/harmonized datasets
- `data/final/`: final analysis dataset
- `scripts/`: R scripts 
- `output/`: tables and figures
- `paper/`: paper PDF

## How to run
### Option A (recommended): one-command pipeline
Run:
- `scripts/09_run_all.R`

### Option B: step-by-step
1. Open the RStudio project file (`.Rproj`).
2. Run scripts in `scripts/` in numerical order (01 → 09).
3. Outputs are written to `output/`.

## Reproducibility notes
- Script 09 uses relative paths (no machine-specific paths).
- Package loading/installation is handled in `scripts/01_load_packages.R`.
- The pipeline produces the main regression table and figures in `output/`.
