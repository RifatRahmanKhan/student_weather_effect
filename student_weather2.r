#libraries
library(ggplot2)
library(pwr)

df <- read.delim("student_Weather_8c.txt",
                 sep = "\t", header = TRUE,
                 stringsAsFactors = FALSE, check.names = FALSE)

head(df)
cat("Variables:", ncol(df), "\nObservations:", nrow(df), "\n")
names(df)

# calculating the medians of the ratings for mild, moderate and severe weather conditions for willingness to attend in-person classes.
median(df$Mild_Weather_Rating, na.rm = TRUE)
median(df$Moderate_Weather_Rating, na.rm = TRUE)
median(df$Severe_Weather_Rating, na.rm = TRUE)

# Calculating IQRs
IQR(df$Mild_Weather_Rating, na.rm = TRUE)
IQR(df$Moderate_Weather_Rating, na.rm = TRUE)
IQR(df$Severe_Weather_Rating, na.rm = TRUE)

# Contingency table ------------------------------------------------------
# Individual frequency tables for each weather condition (ratings 1–5)
table_mild     <- table(factor(df$Mild_Weather_Rating,     levels = 1:5))
table_moderate <- table(factor(df$Moderate_Weather_Rating, levels = 1:5))
table_severe   <- table(factor(df$Severe_Weather_Rating,   levels = 1:5))

contingency_table <- rbind(
  Mild     = table_mild,
  Moderate = table_moderate,
  Severe   = table_severe
)

contingency_table

# Save original contingency table as CSV
write.csv(contingency_table, "contingency_table.csv", row.names = TRUE)

# ---- MERGE COLUMNS: (1+2) and (3+4+5) -----------------------------------
contingency_table_merged <- cbind(
  `1-2`   = rowSums(contingency_table[, c("1", "2"), drop = FALSE]),
  `3-5`   = rowSums(contingency_table[, c("3", "4", "5"), drop = FALSE])
)

contingency_table_merged

# Save merged contingency table as CSV
write.csv(contingency_table_merged, "contingency_table_merged.csv", row.names = TRUE)

# Bar chart --------------------------------------------------------------
# (Keep EXACTLY as before: shows distribution for 1–5; merging only affects chi-square table)
ratings <- data.frame(
  Condition = c(rep("Mild",     length(df$Mild_Weather_Rating)),
                rep("Moderate", length(df$Moderate_Weather_Rating)),
                rep("Severe",   length(df$Severe_Weather_Rating))),
  Rating = c(df$Mild_Weather_Rating,
             df$Moderate_Weather_Rating,
             df$Severe_Weather_Rating)
)

p <- ggplot(ratings, aes(x = factor(Rating), fill = Condition)) +
  geom_bar(position = "dodge") +
  labs(x = "Willingness Rating (1–5)",
       y = "Number of Students",
       title = "Distribution of Willingness Ratings by Weather Condition") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

p

# Save bar chart as PNG
ggsave("bar_chart_weather_ratings.png", plot = p, width = 7, height = 5, dpi = 300)

# Chi-squared test of independence (USING MERGED TABLE) -------------------
chi <- chisq.test(contingency_table_merged, correct = FALSE)

# Effect size (Cramer's V) using merged table
V <- sqrt(
  chi$statistic /
    (sum(contingency_table_merged) *
       (min(nrow(contingency_table_merged), ncol(contingency_table_merged)) - 1))
)

chi
V

# Power analysis (UPDATE df to match merged table) ------------------------
# df = (r-1)*(c-1); here r=3 rows, c=2 cols => df = (3-1)*(2-1) = 2
pwr.chisq.test(w = V, df = 2, N = sum(contingency_table_merged), sig.level = 0.05)


# -------------------------------------------------------------------------
# Cochran's Q test (most appropriate for repeated-measures categorical data)
# We binarize ratings: 1–2 = 0 (low), 3–5 = 1 (moderate/high)
# -------------------------------------------------------------------------

library(DescTools)

# Ensure ratings are numeric (in case they were read as character)
df$Mild_Weather_Rating     <- as.numeric(df$Mild_Weather_Rating)
df$Moderate_Weather_Rating <- as.numeric(df$Moderate_Weather_Rating)
df$Severe_Weather_Rating   <- as.numeric(df$Severe_Weather_Rating)

# Create binary variables: 1 if rating is 3–5, else 0 (for 1–2)
df$mild_bin     <- ifelse(df$Mild_Weather_Rating     >= 3, 1, 0)
df$moderate_bin <- ifelse(df$Moderate_Weather_Rating >= 3, 1, 0)
df$severe_bin   <- ifelse(df$Severe_Weather_Rating   >= 3, 1, 0)

# Keep only complete cases for the three repeated measures
bin_df <- df[complete.cases(df[, c("mild_bin", "moderate_bin", "severe_bin")]),
             c("mild_bin", "moderate_bin", "severe_bin")]

# Save respondent-level binary responses (one row per participant)
write.csv(bin_df, "respondent_binary_responses_by_condition.csv", row.names = FALSE)

# Cochran's Q test
cq <- CochranQTest(as.matrix(bin_df))
cq

# -------------------------------------------------------------------------
# Empirical power estimate (simulation via bootstrap resampling of students)
# This preserves within-student correlation across conditions.
# NOTE: This is a post-hoc / observed-data-based power estimate (exploratory).
# -------------------------------------------------------------------------

set.seed(123)

alpha <- 0.05
B <- 10000  # increase for more stable estimate
n <- nrow(bin_df)

pvals <- replicate(B, {
  samp <- bin_df[sample.int(n, size = n, replace = TRUE), , drop = FALSE]
  DescTools::CochranQTest(as.matrix(samp))$p.value
})

power_est <- mean(pvals < alpha, na.rm = TRUE)
cat("Empirical (bootstrap) power estimate for Cochran's Q at alpha =",
    alpha, ":", round(power_est, 3), "\n")
