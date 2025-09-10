# ==============================================
# Module 8: Simulation Summary + Bar Charts
# Smart City Waste Management Project
# ==============================================

# Install packages if not already installed
# install.packages(c("dplyr","ggplot2"))

library(dplyr)
library(ggplot2)

# -------------------------------
# Load or Generate Simulation Data
# -------------------------------
# If you already ran the simulation, load data
# events <- read.csv("simulation_events.csv")
# bins <- read.csv("simulation_bin_log.csv")

# For demonstration, let's simulate small sample data
set.seed(123)
zones <- c("North","South","East","West")
bins_per_zone <- 10

events <- data.frame(
  Time = sample(1:200, 50, replace = TRUE),
  Bin_ID = sample(1:(bins_per_zone * length(zones)), 50, replace = TRUE),
  Zone = sample(zones, 50, replace = TRUE),
  Truck_ID = sample(1:2, 50, replace = TRUE),
  Fill_pct = round(runif(50, 80, 100), 2)
)

bins <- data.frame(
  Zone = rep(zones, each = bins_per_zone),
  Bin_ID = 1:(bins_per_zone * length(zones)),
  Fill_pct = runif(bins_per_zone * length(zones), 0, 100)
)

# -------------------------------
# SUMMARY OF SIMULATION
# -------------------------------

cat("=== Simulation Summary ===\n\n")

# Total number of collection events
total_collections <- nrow(events)
cat("Total Bins Serviced (Collections):", total_collections, "\n\n")

# Collections by zone
collections_by_zone <- events %>%
  group_by(Zone) %>%
  summarise(Collections = n(), .groups = "drop")

cat("Collections by Zone:\n")
print(collections_by_zone)
cat("\n")

# Peak time step with most collections
peak_time <- events %>%
  group_by(Time) %>%
  summarise(Collections = n(), .groups = "drop") %>%
  arrange(desc(Collections)) %>%
  slice(1)

cat("Peak Time Step:\n")
print(peak_time)
cat("\n")

# Average fill percentage of bins at the end
avg_fill_by_zone <- bins %>%
  group_by(Zone) %>%
  summarise(Average_Fill = mean(Fill_pct), .groups = "drop")

cat("Average Bin Fill Percentage by Zone (Final State):\n")
print(avg_fill_by_zone)
cat("\n")

# -------------------------------
# BAR CHARTS
# -------------------------------

# 1) Total Collections by Zone
ggplot(collections_by_zone, aes(x = Zone, y = Collections, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Collections by Zone",
       x = "Zone", y = "Number of Collections") +
  theme_minimal()

# 2) Top Time Steps by Number of Collections
collections_by_time <- events %>%
  group_by(Time) %>%
  summarise(Collections = n(), .groups = "drop")

top_times <- head(collections_by_time %>% arrange(desc(Collections)), 10)

ggplot(top_times, aes(x = factor(Time), y = Collections)) +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.8) +
  labs(title = "Top Time Steps by Number of Collections",
       x = "Time Step", y = "Collections") +
  theme_minimal()

# 3) Final Average Bin Fill Percentage by Zone
ggplot(avg_fill_by_zone, aes(x = Zone, y = Average_Fill, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Final Average Bin Fill Percentage by Zone",
       x = "Zone", y = "Average Fill (%)") +
  theme_minimal()

cat("=== End of Summary and Visualization ===\n")
