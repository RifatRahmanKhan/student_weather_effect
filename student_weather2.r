#libraries
library(ggplot2)
library(pwr)

df <- read.delim("Student_Weather_AllColumns_Concise.txt",
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
