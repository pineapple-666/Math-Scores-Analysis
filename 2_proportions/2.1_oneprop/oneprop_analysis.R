# oneprop_analysis.R
# One-proportion test comparing the sample proportion of ComputerHome to p0 = 0.77 (2016 US)

# ---- Load data (flexible) ----
# If a variable ComputerHome exists in the environment, script will use it.
# Otherwise, script tries to read data/ComputerHome.csv or ComputerHome.csv

if (!exists("ComputerHome")) {
  data_path1 <- file.path("data", "ComputerHome.csv")
  data_path2 <- "ComputerHome.csv"
  if (file.exists(data_path1)) {
    ComputerHome <- read.csv(data_path1, stringsAsFactors = FALSE)
  } else if (file.exists(data_path2)) {
    ComputerHome <- read.csv(data_path2, stringsAsFactors = FALSE)
  } else {
    stop("ComputerHome dataset not found. Provide an object `ComputerHome` (data.frame or vector), or place a CSV at data/ComputerHome.csv")
  }
}

# If ComputerHome is a data.frame, extract the column named 'ComputerHome' (or the first column)
if (is.data.frame(ComputerHome)) {
  if ("ComputerHome" %in% names(ComputerHome)) {
    vec <- ComputerHome$ComputerHome
  } else if (ncol(ComputerHome) == 1) {
    vec <- ComputerHome[[1]]
  } else {
    stop("ComputerHome dataframe must contain a column named 'ComputerHome' or be a single-column data.frame.")
  }
} else {
  vec <- ComputerHome
}

# Convert to numeric (expect 0/1); remove NAs
vec <- as.numeric(vec)
vec <- vec[!is.na(vec)]
n <- length(vec)
phat <- mean(vec)
sd_vec <- sd(vec)

cat("Sample size (n):", n, "\n")
cat("Sample proportion (phat):", phat, "\n")
cat("Sample SD (of 0/1):", sd_vec, "\n\n")

# ---- One-proportion z-test function ----
oneprop_test <- function(phat, n, p = NULL, conf.level = 0.95, alternative = "two.sided") {
  if (is.null(p)) stop("Null proportion 'p' must be provided.")
  # Standard error under H0 for test statistic:
  SE_null <- sqrt(p * (1 - p) / n)
  z_stat <- (phat - p) / SE_null

  # p-value by alternative
  if (alternative == "two.sided") {
    p.value <- 2 * (1 - pnorm(abs(z_stat)))
  } else if (alternative == "greater") {
    p.value <- 1 - pnorm(z_stat)
  } else if (alternative == "less") {
    p.value <- pnorm(z_stat)
  } else {
    stop("alternative must be 'two.sided', 'greater', or 'less'")
  }

  # Confidence interval using phat's SE
  SE_phat <- sqrt(phat * (1 - phat) / n)
  z_alpha <- qnorm(1 - (1 - conf.level) / 2)
  cint <- phat + c(-1, 1) * z_alpha * SE_phat

  return(list(
    estimate = phat,
    n = n,
    z = z_stat,
    p.value = p.value,
    SE_null = SE_null,
    SE_phat = SE_phat,
    conf.level = conf.level,
    cint = cint
  ))
}

# ---- Run test vs p0 = 0.77 ----
p0 <- 0.77
res <- oneprop_test(phat = phat, n = n, p = p0, conf.level = 0.95, alternative = "two.sided")

cat("One-proportion z-test results (H0: p = ", p0, ")\n", sep = "")
cat("  estimate (phat):", round(res$estimate, 6), "\n")
cat("  n:", res$n, "\n")
cat("  z statistic:", round(res$z, 2), "\n")
cat("  p-value:", formatC(res$p.value, digits = 6, format = "e"), "\n")
cat("  95% CI for phat: [", res$cint[1], ", ", res$cint[2], "]\n\n", sep = "")

# ---- Compare with R's built-in prop.test (approximate) ----
x <- sum(vec) # number of successes (1s)
cat("prop.test (R built-in, no continuity correction):\n")
print(prop.test(x = x, n = n, p = p0, correct = FALSE))

# End of script
