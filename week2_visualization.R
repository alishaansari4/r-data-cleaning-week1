# Week 2: Data Visualization and Insight Communication using R
# Dataset: Palmer Penguins (Cleaned)

library(tidyverse)

# 1. Load Data
penguins <- read_csv("penguins_cleaned.csv", show_col_types = FALSE)

# 2. Visualization 1: Distribution of Body Mass (Histogram)
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(bins = 15, alpha = 0.6, position = "identity", color = "white") +
  labs(title = "Distribution of Body Mass Across Penguin Species",
       x = "Body Mass (g)", y = "Frequency (Count)", fill = "Species") +
  theme_minimal()

# 3. Visualization 2: Bill Dimensions & Simpson's Paradox (Scatter + Fit)
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.75, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Bill Length vs Bill Depth (Simpson's Paradox)",
       x = "Bill Length (mm)", y = "Bill Depth (mm)", color = "Species") +
  theme_minimal()

# 4. Visualization 3: Geographic Distribution Across Islands (Stacked Bar)
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "stack", width = 0.55) +
  labs(title = "Penguin Species Inhabiting Each Island",
       x = "Island", y = "Number of Penguins", fill = "Species") +
  theme_minimal()

# 5. Visualization 4: Sexual Dimorphism (Grouped Boxplot)
penguins %>%
  filter(sex != "Unknown") %>%
  ggplot(aes(x = species, y = flipper_length_mm, fill = sex)) +
  geom_boxplot(width = 0.5) +
  labs(title = "Flipper Length Variation by Species and Sex",
       x = "Species", y = "Flipper Length (mm)", fill = "Sex") +
  theme_minimal()
