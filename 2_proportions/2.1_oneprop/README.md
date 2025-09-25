# One-proportion test: ComputerHome vs US population (2016)

Purpose
- Compare the sample proportion of students who have a computer/laptop at home (`ComputerHome`) to the 2016 US population reference proportion p0 = 0.77.

Scenario
- According to the 2016 census, 77% of US households had a computer or laptop.
- We use the `ComputerHome` variable (0 = no, 1 = yes) and test whether the sample proportion differs from 0.77.

Summary of results
- The sample proportion of students that had a computer or laptop in their home is 0.93 (phat ≈ 0.9276).
- This was compared to the reference proportion p0 = 0.77.
- The observed difference (phat − p0) ≈ 0.158 was statistically significant at α = 0.10 (z ≈ 36.03, p < .001).
- Conclusion: Reject the null hypothesis. There is strong evidence the student sample proportion of households with a computer/laptop differs from the countrywide 2016 proportion of 0.77.

Files
- `oneprop_analysis.R` — cleaned R script that:
  - Loads `ComputerHome` (data frame or CSV),
  - Computes sample proportion `phat`, n, sd,
  - Runs a z-based one-proportion test vs p0 = 0.77,
  - Computes a 95% CI for the sample proportion,
  - Prints results and compares to R's `prop.test()` (approximation).

How to run
- In R interactive session: source the script
  ```r
  source("2_proportion/oneprop_analysis.R")
  ```
- From PowerShell (Rscript must be on PATH):
  ```powershell
  Rscript .\2_proportion\oneprop_analysis.R
  ```

Notes
- The script first looks for an object named `ComputerHome` in the R environment. If not present, it looks for a CSV in `data/ComputerHome.csv` (or `ComputerHome.csv` at repository root).
- The script assumes `ComputerHome` values are 0/1 (or a single-column data frame). It removes NAs before calculating.
