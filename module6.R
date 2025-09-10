# ==============================================
# Module 8: Waste Management Simulation Evaluation
# Statistical Significance Test + Visualization
# ==============================================

# Install necessary packages (run only once)
# install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# -------------------------------
# STEP 1: Load Simulation Data
# -------------------------------
# Replace with your actual simulation CSV files:
# events <- read.csv("simulation_events.csv")
# bins <- read.csv("simulation_bin_log.csv")

# ---- Example dataset (for demonstration) ----
set.seed(123)
zones <- c("North","South","East","West")

# Simulated collections data
events <- data.frame(
  Zone = sample(zones, 50, replace = TRUE),
  Collections = c(
    rpois(15, lambda = 22),  # North - moderately high
    rpois(10, lambda = 15),  # South - medium
    rpois(15, lambda = 30),  # East - highest
    rpois(10, lambda = 12)   # West - lowest
  )
)

# -------------------------------
# STEP 2: ANOVA Statistical Test
# -------------------------------
anova_model <- aov(Collections ~ Zone, data = events)
anova_result <- summary(anova_model)

cat("=== ANOVA Results ===\n")
print(anova_result)

# Extract p-value
p_value <- anova_result[[1]]$`Pr(>F)`[1]
cat("\nP-value for ANOVA:", p_value, "\n")

# Interpretation
if (p_value < 0.05) {
  cat("Result: Statistically significant difference between zones (p < 0.05)\n\n")
} else {
  cat("Result: No statistically significant difference between zones (p >= 0.05)\n\n")
}

# -------------------------------
# STEP 3: Tukey Post-Hoc Test (Optional)
# -------------------------------
if (p_value < 0.05) {
  tukey_result <- TukeyHSD(anova_model)
  cat("=== Tukey Post-Hoc Test ===\n")
  print(tukey_result)
}

# -------------------------------
# STEP 4: Summary Statistics
# -------------------------------
cat("=== Summary Statistics ===\n")

# Average and total collections per zone
summary_stats <- events %>%
  group_by(Zone) %>%
  summarise(
    Total_Collections = sum(Collections),
    Average_Collections = mean(Collections),
    .groups = "drop"
  )

print(summary_stats)

# Identify zone with max collections
max_zone <- summary_stats %>% filter(Total_Collections == max(Total_Collections))
cat("\nZone with highest total collections:\n")
print(max_zone)

# -------------------------------
# STEP 5: Visualization - Bar Charts
# -------------------------------

# 1) Average Collections by Zone
ggplot(summary_stats, aes(x = Zone, y = Average_Collections, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = paste("Average Collections by Zone (p =", round(p_value, 4), ")"),
       x = "Zone", y = "Average Collections") +
  theme_minimal()

# 2) Total Collections by Zone
ggplot(summary_stats, aes(x = Zone, y = Total_Collections, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Collections by Zone",
       x = "Zone", y = "Total Collections") +
  theme_minimal()

# 3) Collections Distribution (Boxplot)
ggplot(events, aes(x = Zone, y = Collections, fill = Zone)) +
  geom_boxplot() +
  labs(title = "Collections Distribution by Zone",
       x = "Zone", y = "Collections per Time Step") +
  theme_minimal()

# -------------------------------
# STEP 6: Export Results (Optional)
# -------------------------------
write.csv(summary_stats, "Zone_Collection_Summary.csv", row.names = FALSE)
cat("\nSummary data saved to 'Zone_Collection_Summary.csv'\n")

# -------------------------------
# Final Notes
# -------------------------------
cat("\n--- Evaluation Complete ---\n")
if (p_value < 0.05) {
  cat("The differences between zones are statistically significant.\n")
} else {
  cat("No significant differences detected between zones.\n")
}
