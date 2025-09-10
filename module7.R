# ==============================================
# Module 8: Comparing Proposed Model with Two Existing Models
# Smart City Waste Management
# ==============================================

# Install required packages if not installed
# install.packages(c("dplyr", "ggplot2"))

library(dplyr)
library(ggplot2)

# -------------------------------
# STEP 1: Create/Load Data
# -------------------------------
# Replace this section with actual CSV files if available:
# existing_model1 <- read.csv("existing_model1.csv")
# existing_model2 <- read.csv("existing_model2.csv")
# proposed_model <- read.csv("proposed_model.csv")

set.seed(123)
zones <- c("North", "South", "East", "West")

# Simulate collection totals for each model
# Existing Model 1 (Traditional)
model1 <- data.frame(
  Zone = rep(zones, each = 10),
  Collections = c(rpois(10, 25), rpois(10, 20), rpois(10, 35), rpois(10, 18)),
  Model = "Existing_Model_1"
)

# Existing Model 2 (Semi-Optimized)
model2 <- data.frame(
  Zone = rep(zones, each = 10),
  Collections = c(rpois(10, 22), rpois(10, 18), rpois(10, 30), rpois(10, 15)),
  Model = "Existing_Model_2"
)

# Proposed Model (Smart Waste Management)
proposed <- data.frame(
  Zone = rep(zones, each = 10),
  Collections = c(rpois(10, 15), rpois(10, 14), rpois(10, 18), rpois(10, 12)),
  Model = "Proposed_Model"
)

# Combine all into one dataset
all_models <- rbind(model1, model2, proposed)

# -------------------------------
# STEP 2: Summary Statistics
# -------------------------------
summary_stats <- all_models %>%
  group_by(Model, Zone) %>%
  summarise(
    Total_Collections = sum(Collections),
    Avg_Collections = mean(Collections),
    .groups = "drop"
  )

cat("=== Summary Statistics ===\n")
print(summary_stats)

# -------------------------------
# STEP 3: ANOVA Test
# -------------------------------
anova_model <- aov(Collections ~ Model, data = all_models)
anova_result <- summary(anova_model)

cat("\n=== ANOVA Results ===\n")
print(anova_result)

# Extract p-value
p_value <- anova_result[[1]]$`Pr(>F)`[1]
cat("\nP-value for ANOVA:", p_value, "\n")

if (p_value < 0.05) {
  cat("Result: Statistically significant difference between models (p < 0.05)\n\n")
} else {
  cat("Result: No significant difference between models (p >= 0.05)\n\n")
}

# -------------------------------
# STEP 4: Visualization - Bar Charts
# -------------------------------

# 1) Bar chart of average collections per zone by model
ggplot(summary_stats, aes(x = Zone, y = Avg_Collections, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = paste("Average Collections per Zone by Model (p =", round(p_value, 4), ")"),
       x = "Zone", y = "Average Collections") +
  theme_minimal()

# 2) Total collections per model (aggregated across all zones)
total_by_model <- all_models %>%
  group_by(Model) %>%
  summarise(Total_Collections = sum(Collections), .groups = "drop")

ggplot(total_by_model, aes(x = Model, y = Total_Collections, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Collections by Model",
       x = "Model Type", y = "Total Collections") +
  theme_minimal()

# 3) Boxplot for distribution of collections across models
ggplot(all_models, aes(x = Model, y = Collections, fill = Model)) +
  geom_boxplot() +
  labs(title = "Distribution of Collections Across Models",
       x = "Model", y = "Collections per Time Step") +
  theme_minimal()

# -------------------------------
# STEP 5: Interpretation
# -------------------------------
cat("=== Interpretation ===\n")
if (p_value < 0.05) {
  cat("There is a statistically significant difference between the models.\n")
  cat("Proposed model shows improved performance with fewer collections compared to existing systems.\n")
} else {
  cat("No statistically significant improvement detected.\n")
}
