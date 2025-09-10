# ==============================================
# Smart City Waste Management Modeling - DT & SVM
# ==============================================

# Step 1: Install packages (only if not installed)
# install.packages(c("dplyr","ggplot2","randomForest","caret","e1071","rpart","rpart.plot"))

library(dplyr)
library(ggplot2)
library(caret)
library(e1071)        # SVM
library(rpart)        # Decision Tree
library(rpart.plot)   # Decision Tree plot
library(randomForest)

# Step 2: Load the dataset
data <- read.csv("synthetic_waste_mgmt_data.csv")

# View first few rows
cat("First 6 rows of the dataset:\n")
print(head(data))

# Show basic summary
cat("\nSummary of the dataset:\n")
print(summary(data))

# Step 3: Identify numeric and categorical columns
num_cols <- sapply(data, is.numeric)
cat_cols <- !num_cols

# Convert categorical variables to factors
for (col in names(data)[cat_cols]) {
  data[[col]] <- as.factor(data[[col]])
}

# Step 4: Handle Missing Values -----------------------
# Numeric columns - replace NA with mean
for (col in names(data)[num_cols]) {
  data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
}

# Categorical columns - replace NA with mode
for (col in names(data)[cat_cols]) {
  mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
  data[[col]][is.na(data[[col]])] <- mode_val
}

cat("\n✅ Missing values handled\n")

# Step 5: Standardization -----------------------------
data_standard <- data
data_standard[num_cols] <- scale(data[num_cols])

cat("✅ Standardization complete (mean=0, sd=1)\n")

# Step 6: Split into training and testing sets --------
set.seed(123)
trainIndex <- createDataPartition(data$Waste_Generated_kg, p = 0.7, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

cat("✅ Dataset split into training (70%) and testing (30%)\n")

# Step 7: Decision Tree Modeling ----------------------
dt_model <- rpart(Waste_Generated_kg ~ Population + Recycled_kg + 
                    Organic_Percent + Plastic_Percent + 
                    Collection_Frequency + Complaint_Count + 
                    Sensor_Failure_Rate,
                  data = train, method = "anova")

cat("\nDecision Tree Model Summary:\n")
print(summary(dt_model))

# Plot the Decision Tree
rpart.plot(dt_model, main="Decision Tree - Waste Generation Prediction")

# Step 8: Support Vector Machine (SVM) Modeling ------
svm_model <- svm(Waste_Generated_kg ~ Population + Recycled_kg + 
                   Organic_Percent + Plastic_Percent + 
                   Collection_Frequency + Complaint_Count + 
                   Sensor_Failure_Rate,
                 data = train)

cat("\n✅ SVM Model Trained\n")

# Step 9: Predictions ---------------------------------
dt_predictions <- predict(dt_model, test)
svm_predictions <- predict(svm_model, test)

# Step 10: Model Evaluation ---------------------------
# RMSE for Decision Tree
rmse_dt <- sqrt(mean((dt_predictions - test$Waste_Generated_kg)^2))

# RMSE for SVM
rmse_svm <- sqrt(mean((svm_predictions - test$Waste_Generated_kg)^2))

cat("\nModel Evaluation Metrics:\n")
cat("Decision Tree RMSE:", rmse_dt, "\n")
cat("SVM RMSE:", rmse_svm, "\n")

# Step 11: Graph of Actual vs Predicted (DT and SVM) --
results_df <- data.frame(
  Actual = test$Waste_Generated_kg,
  DT_Predicted = dt_predictions,
  SVM_Predicted = svm_predictions
)

# Plot Actual vs DT Predictions
ggplot(results_df, aes(x=Actual, y=DT_Predicted)) +
  geom_point(color="orange", alpha=0.6) +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dashed") +
  labs(title="Decision Tree - Actual vs Predicted Waste",
       x="Actual Waste Generated (kg)",
       y="Predicted Waste Generated (kg)")

# Plot Actual vs SVM Predictions
ggplot(results_df, aes(x=Actual, y=SVM_Predicted)) +
  geom_point(color="green", alpha=0.6) +
  geom_abline(intercept=0, slope=1, color="blue", linetype="dashed") +
  labs(title="SVM - Actual vs Predicted Waste",
       x="Actual Waste Generated (kg)",
       y="Predicted Waste Generated (kg)")

# Step 12: Compare RMSE of Both Models ----------------
rmse_comparison <- data.frame(
  Model = c("Decision Tree","SVM"),
  RMSE = c(rmse_dt, rmse_svm)
)

ggplot(rmse_comparison, aes(x=Model, y=RMSE, fill=Model)) +
  geom_bar(stat="identity") +
  labs(title="Model Comparison - RMSE",
       y="Root Mean Square Error") +
  scale_fill_manual(values=c("Decision Tree"="orange","SVM"="purple"))
