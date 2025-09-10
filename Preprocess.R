# ==============================================
# Preprocessing Waste Management Data
# ==============================================

# Step 1: Install packages (first time only)
# install.packages(c("dplyr","ggplot2","randomForest"))

library(dplyr)
library(ggplot2)
library(randomForest)

# Step 2: Load the provided dataset
data <- read.csv("synthetic_waste_mgmt_data.csv")

# View first few rows
head(data)
cat("✅ Dataset Loaded. Rows:", nrow(data), "Columns:", ncol(data), "\n")

# Step 3: Identify numeric and categorical columns
num_cols <- sapply(data, is.numeric)
cat_cols <- !num_cols

# Step 4: Missing Value Handling ----------------------
# Numeric columns - replace NA with mean
for (col in names(data)[num_cols]) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# Categorical columns - replace NA with mode
for (col in names(data)[cat_cols]) {
  mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_val
}

cat("✅ Missing values handled\n")

# Step 5: Standardization (Z-score scaling) -----------
data_standard <- data
data_standard[num_cols] <- scale(data[num_cols])

cat("✅ Standardization complete (mean=0, sd=1)\n")

# Step 6: Random Forest for Feature Importance --------
# Convert categorical columns to factors
for (col in names(data)[cat_cols]) {
  data[[col]] <- as.factor(data[[col]])
}

set.seed(123)
rf_model <- randomForest(Waste_Generated_kg ~ ., data=data, ntree=100, importance=TRUE)
cat("✅ Random Forest model built for feature importance\n")

# View importance
importance_vals <- importance(rf_model)
print(importance_vals)
varImpPlot(rf_model, main="Feature Importance - Random Forest")

# Step 7: Calculate Means for Visualization -----------
raw_means <- colMeans(data[num_cols])
std_means <- colMeans(data_standard[num_cols])

raw_df <- data.frame(Feature = names(raw_means), MeanValue = raw_means)
std_df <- data.frame(Feature = names(std_means), MeanValue = std_means)

compare_df <- data.frame(
  Feature = rep(names(raw_means), 2),
  MeanValue = c(raw_means, std_means),
  Type = rep(c("Raw","Preprocessed"), each = length(raw_means))
)

# -------------------------------
# Graph 1: Raw Data
ggplot(raw_df, aes(x=Feature, y=MeanValue, fill=Feature)) +
  geom_bar(stat="identity") +
  labs(title="Raw Data (Feature Means)", x="Features", y="Mean Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -------------------------------
# Graph 2: Preprocessed Data
ggplot(std_df, aes(x=Feature, y=MeanValue, fill=Feature)) +
  geom_bar(stat="identity") +
  labs(title="Preprocessed Data (Standardized Features)", 
       x="Features", y="Mean Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -------------------------------
# Graph 3: Comparison (Raw vs Preprocessed)
ggplot(compare_df, aes(x=Feature, y=MeanValue, fill=Type)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Raw vs Preprocessed Data", x="Features", y="Mean Value") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values=c("Raw"="tomato","Preprocessed"="steelblue"))
