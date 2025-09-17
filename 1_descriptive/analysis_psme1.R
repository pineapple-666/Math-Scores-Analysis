# Analysis of PSME_1_.csv
# This script includes grouped histograms, population pyramid, and boxplots for PSME_1_.csv
}

# --- Additional Statistical Summaries and Utilities (from interactive session) ---

# View the data (uncomment to use in RStudio)
# View(PSME_1_)

# Filter for District 12 and CAP04 not N/A
math_att_d12 <- PSME_1_ %>% filter(CAP04 != "N/A" & DISTRICT == 12)

# Length and frequency summaries
length(math_att_d12$CAP04)
library(dplyr)

# Mode function for CAP04
Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Modes(math_att_d12$CAP04)

# Cumulative table (requires 'sur' package)
# library(sur)
# cumulative.table(math_att_d12$CAP04)

# Range, median, IQR, quantiles
range(math_att_d12$CAP04)
median(math_att_d12$CAP04)
IQR(math_att_d12$CAP04)
quantile(math_att_d12$CAP04, c(.25), na.rm = TRUE)
quantile(math_att_d12$CAP04, c(.75), na.rm = TRUE)

# Count of CAP04 == 4
math_att_d12 %>% filter(CAP04 == 4) %>% count()

# Frequency table
ftable <- table(math_att_d12$CAP04)
ftable

# Bar chart for all districts
math_att <- PSME_1_ %>% filter(CAP04 != 'N/A')
unique(math_att$DISTRICT)
bar <- ggplot(math_att, aes(DISTRICT)) +
  geom_bar(color = "black", fill = "white") +
  theme_classic() +
  labs(x = "District", y = "Frequency") +
  coord_cartesian(ylim = c(0, 1200), expand = FALSE) +
  scale_x_continuous(breaks = c(11, 12, 13, 14), labels = c("District 11", "District 12", "District 13", "District 14"))
print(bar)

# CAP04 as factor with labels for all districts
math_att$CAP04 <- factor(math_att$CAP04, levels = 1:5,
  labels = c("Totally Untrue", "Mostly Untrue", "Somewhat", "Mostly True", "Totally True"))
bar <- ggplot(math_att, aes(x = CAP04)) +
  geom_bar(color = "black", fill = "white") +
  theme_classic() +
  labs(x = "Response", y = "Frequency") +
  scale_y_continuous(limits = c(0, 1200), breaks = seq(0, 1200, 300))
print(bar)

# Pie chart for CAP04 (uncomment to use)
# slices <- c(161, 148, 624, 669, 1155)
# lbls <- c("Totally Untrue", "Mostly Untrue", "Somewhat", "Mostly True", "Totally True")
# colors <- c("white", "gray", "black")
# pie(slices, labels=lbls, col=colors)

# Math score summaries
math_score <- PSME_1_ %>% filter(Math_Score_rounded != "N/A")
mean(math_score$Math_Score_rounded)
var(math_score$Math_Score_rounded)
# library(e1071)
# skewness(math_score$Math_Score_rounded)
# kurtosis(math_score$Math_Score_rounded)
sqrt(var(math_score$Math_Score_rounded))
sum(math_score$Math_Score_rounded == 396)
length(math_score$Math_Score_rounded)
mean <- mean(math_score$Math_Score_rounded)
library(ggplot2)
(396-mean)/sd

# Histogram for math_score (with ggh4x, if installed)
# library(ggh4x)
# histogram <- ggplot(math_score, aes(Math_Score_rounded)) +
#   geom_histogram(binwidth = 5, color = "black", fill = "grey") +
#   theme_classic() +
#   labs(x = "Math Score (Rounded)", y = "Frequency") +
#   theme(axis.title = element_text(size = 12, family = "sans"),
#         axis.text = element_text(size = 10, family = "sans")) +
#   coord_axes_inside()
# print(histogram)

# Load data (update path as needed)
PSME_1_ <- read.csv("PSME_1_.csv")

# Comprehensive Analysis of PSME_1_.csv
# Includes summary statistics, grouped histograms, population pyramid, and boxplots

library(dplyr)
library(ggplot2)

# Load data (update path as needed)
PSME_1_ <- read.csv("PSME_1_.csv")

# --- Data Cleaning ---
psme_clean <- PSME_1_ %>% filter(CAP04 != "N/A" & DISTRICT %in% c(11,12,13,14))

# --- Descriptive statistics for CAP04 by district ---
if(all(c("CAP04", "DISTRICT") %in% names(psme_clean))) {
  summary_stats <- psme_clean %>%
    group_by(DISTRICT) %>%
    summarise(
      count = n(),
      mean_CAP04 = mean(as.numeric(CAP04), na.rm = TRUE),
      median_CAP04 = median(as.numeric(CAP04), na.rm = TRUE),
      sd_CAP04 = sd(as.numeric(CAP04), na.rm = TRUE)
    )
  print(summary_stats)
}

# --- Bar plot: Frequency of CAP04 responses by district ---
if("CAP04" %in% names(psme_clean)) {
  psme_clean$CAP04 <- factor(psme_clean$CAP04, levels = 1:5,
    labels = c("Totally Untrue", "Mostly Untrue", "Somewhat", "Mostly True", "Totally True"))
  bar <- ggplot(psme_clean, aes(x = CAP04, fill = as.factor(DISTRICT))) +
    geom_bar(position = "dodge", color = "black") +
    theme_classic() +
    labs(x = "Response", y = "Frequency", fill = "District")
  print(bar)
}


# --- Histograms - grouped ---
if("Math_Score_rounded" %in% names(PSME_1_)) {
  histogram <- ggplot(PSME_1_, aes(x = as.numeric(Math_Score_rounded)))
  print(histogram + geom_histogram(binwidth = 5))
  print(histogram + geom_histogram(binwidth = 5, color= "black", fill= "white") +
    theme_classic() +
    labs(x="MCAS Science Raw Score", y="Frequency") +
    scale_x_continuous(breaks= c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)) +
    coord_cartesian(ylim=c(0, 200), expand= FALSE))
}

# --- Population Pyramid ---
if(all(c("S_MALE", "StudentBehavior") %in% names(PSME_1_))) {
  PSME_1_$S_MALE <- factor(PSME_1_$S_MALE, levels = c("0", "1"), labels = c("non-male", "male"))
  print(
    ggplot(data = PSME_1_, aes(x = StudentBehavior, fill = S_MALE)) + 
      geom_histogram(data = subset(PSME_1_, S_MALE == "non-male"), binwidth = 0.5, color = "white", position = "identity") +
      geom_histogram(data = subset(PSME_1_, S_MALE == "male"), binwidth = 0.5, color = "white", position = "identity",
                     aes(y = -after_stat(count))) +
      scale_x_continuous("Student Behavior", breaks = seq(0, 5, by = 0.5)) +
      scale_y_continuous("Frequency", breaks = seq(-2000, 5000, 500), labels = abs) +
      scale_fill_grey(name = "Gender") +
      coord_flip() +
      theme_classic()
  )
}

# --- Boxplot for Math_Score_rounded by District ---
if(all(c("DISTRICT", "Math_Score_rounded") %in% names(PSME_1_))) {
  clean_data <- na.omit(PSME_1_[, c("DISTRICT", "Math_Score_rounded")])
  print(
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
  )
}

# --- Single Boxplot for Math_Score_rounded ---
if("Math_Score_rounded" %in% names(PSME_1_)) {
  print(
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
  )
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
