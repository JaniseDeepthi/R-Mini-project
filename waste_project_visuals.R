# Load libraries
library(DiagrammeR)
library(ggplot2)
library(readr)

# ---- 1. Architecture Diagram ----
graph_code <- "
digraph waste_management {
  graph [layout = dot, rankdir = LR]
  node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Helvetica]
  
  Household [label = 'Households\\n(Smart Bins / Citizens)']
  Collection [label = 'Collection\\n(Trucks & Crews)']
  Processing [label = 'Processing Center']
  Recycling [label = 'Recycling Plant']
  Landfill [label = 'Landfill']
  Analysis [label = 'Data Analysis in R']

  Household -> Collection -> Processing
  Processing -> Recycling
  Processing -> Landfill
  Household -> Analysis
  Collection -> Analysis
  Recycling -> Analysis
}
"

grViz(graph_code)  # This shows the diagram in RStudio's Viewer

# ---- 2. Load your synthetic dataset ----
data <- read_csv("synthetic_waste_mgmt_data.csv")

# ---- 3. Graphs with ggplot2 ----

# Histogram: Waste Generated
ggplot(data, aes(x = Waste_Generated_kg)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Waste Generated", x = "Waste (kg)", y = "Count")

# Scatter Plot: Population vs Waste
ggplot(data, aes(x = Population, y = Waste_Generated_kg, color = Zone)) +
  geom_point() +
  labs(title = "Population vs Waste Generated", x = "Population", y = "Waste (kg)")

# Bar Plot: Smart Bin Usage by Zone
ggplot(data, aes(x = Zone, fill = Smart_Bins_Used)) +
  geom_bar(position = "dodge") +
  labs(title = "Smart Bin Usage by Zone", x = "Zone", y = "Count")

