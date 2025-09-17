# Analysis of PSME_1_.csv
# This script includes grouped histograms, population pyramid, and boxplots for PSME_1_.csv

library(dplyr)
library(ggplot2)

# Load data (update path as needed)
PSME_1_ <- read.csv("PSME_1_.csv")

# --- Histograms - grouped ---
if("Math_Score_rounded" %in% names(PSME_1_)) {
  histogram <- ggplot(PSME_1_, aes(x = as.numeric(Math_Score_rounded)))
  histogram + geom_histogram(binwidth = 5)
  histogram + geom_histogram(binwidth = 5, color= "black", fill= "white") +
    theme_classic() +
    labs(x="MCAS Science Raw Score", y="Frequency") +
    scale_x_continuous(breaks= c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
    coord_cartesian(ylim=c(0, 200), expand= FALSE)
}

# --- Population Pyramid ---
if(all(c("S_MALE", "StudentBehavior") %in% names(PSME_1_))) {
  PSME_1_$S_MALE <- factor(PSME_1_$S_MALE, levels = c("0", "1"), labels = c("non-male", "male"))
  ggplot(data = PSME_1_, aes(x = StudentBehavior, fill = S_MALE)) + 
    geom_histogram(data = subset(PSME_1_, S_MALE == "non-male"), binwidth = 0.5, color = "white", position = "identity") +
    geom_histogram(data = subset(PSME_1_, S_MALE == "male"), binwidth = 0.5, color = "white", position = "identity",
                   aes(y = -after_stat(count))) +
    scale_x_continuous("Student Behavior", breaks = seq(0, 5, by = 0.5)) +
    scale_y_continuous("Frequency", breaks = seq(-2000, 5000, 500), labels = abs) +
    scale_fill_grey(name = "Gender") +
    coord_flip() +
    theme_classic()
}

# --- Boxplot for Math_Score_rounded by District ---
if(all(c("DISTRICT", "Math_Score_rounded") %in% names(PSME_1_))) {
  clean_data <- na.omit(PSME_1_[, c("DISTRICT", "Math_Score_rounded")])
  ggplot(clean_data, aes(x = factor(DISTRICT), y = as.numeric(Math_Score_rounded))) +
    geom_boxplot(
      fill = "grey", 
      outlier.shape = 21, 
      outlier.colour = "black", 
      outlier.fill = "black", 
      outlier.size = 2
    ) +
    coord_flip() +
    labs(
      y = "Math State Test Score",
      x = "District"
    ) +
    scale_y_continuous(breaks = seq(0, max(clean_data$Math_Score_rounded, na.rm = TRUE), by = 50)) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(t = 10)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# --- Single Boxplot for Math_Score_rounded ---
if("Math_Score_rounded" %in% names(PSME_1_)) {
  ggplot(PSME_1_, aes(x = "", y = as.numeric(Math_Score_rounded))) +
    geom_boxplot(fill = "grey") +
    coord_flip() +
    labs(
      y = "Math State Test Score",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(t = 10)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# Note: MCASdata and DISTRICT_12_13 code is omitted as this script is focused on PSME_1_.csv only.
