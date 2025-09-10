# ==============================================
# Waste Management Simulation with Bar Charts
# ==============================================

# install.packages(c("dplyr","ggplot2"))  # Run once if not installed
library(dplyr)
library(ggplot2)

set.seed(123)

# -------------------------------
# PARAMETERS
# -------------------------------
zones <- c("North","South","East","West")
bins_per_zone <- 10
households_per_zone <- c(1200, 800, 1500, 1000)

sim_days <- 7
steps_per_hour <- 4           # 15-minute steps
hours_per_day <- 24
time_steps <- sim_days * hours_per_day * steps_per_hour

bin_capacity_kg <- 200
initial_fill_pct <- runif(bins_per_zone * length(zones), 0, 20)

# Waste generation
avg_kg_per_household_day <- c(0.6, 0.5, 0.8, 0.55)

# Threshold for collection
collect_threshold_pct <- 85

# Trucks
num_trucks <- 2
truck_service_time_min <- 8
avg_travel_time_min <- 10

# -------------------------------
# CREATE INITIAL DATA
# -------------------------------
bins <- data.frame(
  Zone = rep(zones, each = bins_per_zone),
  Bin_ID = 1:(bins_per_zone * length(zones)),
  Capacity_kg = bin_capacity_kg,
  Fill_kg = initial_fill_pct / 100 * bin_capacity_kg,
  Fill_pct = initial_fill_pct,
  stringsAsFactors = FALSE
)

# Assign households equally to bins
bins$Households <- rep(households_per_zone, each = bins_per_zone) / bins_per_zone

# Waste generation per step
zone_index <- match(bins$Zone, zones)
bins$WasteGen_kg_per_step <- (avg_kg_per_household_day[zone_index] / (24 * steps_per_hour)) * bins$Households

# Initialize trucks
trucks <- data.frame(
  Truck_ID = 1:num_trucks,
  Available_at_step = rep(0, num_trucks),
  stringsAsFactors = FALSE
)

# -------------------------------
# SIMULATION LOGS
# -------------------------------
events <- data.frame(Time = integer(0), Bin_ID = integer(0), Zone = character(0),
                     Truck_ID = integer(0), Fill_pct = numeric(0), stringsAsFactors = FALSE)

# -------------------------------
# SIMULATION LOOP
# -------------------------------
for (t in 0:(time_steps - 1)) {
  # Waste generation (stochastic)
  noise <- rnorm(nrow(bins), mean = 1, sd = 0.15)
  noise[noise < 0.5] <- 0.5
  bins$Fill_kg <- bins$Fill_kg + bins$WasteGen_kg_per_step * noise
  bins$Fill_kg <- pmin(bins$Fill_kg, bins$Capacity_kg)
  bins$Fill_pct <- round(100 * bins$Fill_kg / bins$Capacity_kg, 2)
  
  # Bins over threshold
  candidates <- bins %>% filter(Fill_pct >= collect_threshold_pct)
  candidates <- candidates[sample(nrow(candidates)), ]  # random shuffle
  
  free_trucks <- trucks %>% filter(Available_at_step <= t)
  
  if (nrow(free_trucks) > 0 && nrow(candidates) > 0) {
    assign_n <- min(nrow(free_trucks), nrow(candidates))
    for (i in 1:assign_n) {
      truck_id <- free_trucks$Truck_ID[i]
      bin_id <- candidates$Bin_ID[i]
      zone_name <- candidates$Zone[i]
      
      total_min <- avg_travel_time_min + truck_service_time_min
      busy_steps <- ceiling(total_min / 15)
      trucks$Available_at_step[trucks$Truck_ID == truck_id] <- t + busy_steps
      
      # Empty the bin
      bins$Fill_kg[bins$Bin_ID == bin_id] <- 0
      bins$Fill_pct[bins$Bin_ID == bin_id] <- 0
      
      # Log event
      events <- rbind(events, data.frame(
        Time = t,
        Bin_ID = bin_id,
        Zone = zone_name,
        Truck_ID = truck_id,
        Fill_pct = candidates$Fill_pct[i]
      ))
    }
  }
}

# -------------------------------
# ANALYSIS AND BAR CHARTS
# -------------------------------

# 1) Total Collections by Zone
zone_collections <- events %>%
  group_by(Zone) %>%
  summarise(Collections = n(), .groups = "drop")

ggplot(zone_collections, aes(x = Zone, y = Collections, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Collections by Zone",
       x = "Zone", y = "Number of Collections") +
  theme_minimal()

# 2) Top Time Steps by Number of Collections
collections_by_time <- events %>%
  group_by(Time) %>%
  summarise(Collections = n(), .groups = "drop")

top_times <- head(collections_by_time %>% arrange(desc(Collections)), 15)

ggplot(top_times, aes(x = factor(Time), y = Collections)) +
  geom_bar(stat = "identity", fill = "tomato", alpha = 0.7) +
  labs(title = "Top Time Steps by Number of Collections",
       x = "Time Step", y = "Collections") +
  theme_minimal()

# 3) Average Bin Fill by Zone (final step)
final_fill <- bins %>%
  group_by(Zone) %>%
  summarise(Average_Fill = mean(Fill_pct), .groups = "drop")

ggplot(final_fill, aes(x = Zone, y = Average_Fill, fill = Zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Bin Fill Percentage by Zone (Final State)",
       x = "Zone", y = "Average Fill (%)") +
  theme_minimal()
