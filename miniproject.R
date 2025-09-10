# Step 1: Install required packages (only run once)
# install.packages("synthpop")
# install.packages("dplyr")
# install.packages("ggplot2")

# Step 2: Load the necessary libraries
library(synthpop)
library(dplyr)
library(ggplot2)

# Step 3: Create a mock "real" dataset
set.seed(123)  # Reproducibility
real_data <- data.frame(
  Zone = as.factor(sample(c("North", "South", "East", "West"), 500, replace = TRUE)),
  Population = sample(5000:15000, 500, replace = TRUE),
  Waste_Generated_kg = round(runif(500, 300, 8000), 1),
  Recycled_kg = round(runif(500, 100, 4000), 1),
  Organic_Percent = round(runif(500, 10, 90), 1),
  Plastic_Percent = round(runif(500, 5, 60), 1),
  Collection_Frequency = sample(1:7, 500, replace = TRUE),  # Days/week
  Smart_Bins_Used = as.factor(sample(c("Yes", "No"), 500, replace = TRUE)),
  Complaint_Count = sample(0:30, 500, replace = TRUE),
  Sensor_Failure_Rate = round(runif(500, 0.00, 0.25), 2)
)

# Step 4: Generate synthetic data
synthetic_data <- syn(real_data)$syn

# Step 5: Save synthetic dataset
write.csv(synthetic_data, "synthetic_waste_mgmt_data.csv", row.names = FALSE)
cat("✅ Synthetic dataset created successfully.\n")
head(data)
summary(data)
