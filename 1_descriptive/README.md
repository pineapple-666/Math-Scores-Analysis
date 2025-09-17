# PSME_1_ Data Analysis

This repository contains R code for analyzing the PSME_1_.csv dataset. The script performs basic data cleaning, descriptive statistics, and visualizations for survey responses and math scores by district.

## Files
- `my_console.R`: Main R script for data analysis (cleaned and focused on PSME_1_.csv)
- `README.md`: This file
- `.gitignore`: (Recommended) Ignore unnecessary files
- `PSME_1_.csv`: (Not included if sensitive; see below)

## How to Use
1. Place `PSME_1_.csv` in the same directory as the R script.
2. Open `my_console.R` in RStudio or your preferred R environment.
3. Run the script to generate summary statistics and plots.

## Data Format
- The CSV should include at least the columns: `CAP04`, `DISTRICT`, and (optionally) `Math_Score_rounded`.
- `CAP04` is expected to be coded 1-5 (Likert scale).
- `DISTRICT` should be numeric (e.g., 11, 12, 13, 14).

## Requirements
- R packages: `dplyr`, `ggplot2`

## Notes
- Do not upload sensitive or private data to GitHub. If the data is sensitive, provide a sample or describe the format in this README.

## License
Add a license if you wish to specify usage rights (e.g., MIT, GPL).
