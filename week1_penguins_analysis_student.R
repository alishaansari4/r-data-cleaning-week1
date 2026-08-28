# Week 1 - Data Cleaning and Preliminary Analysis
# Palmer Penguins dataset

library(tidyverse)

url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv"
penguins <- read_csv(url, show_col_types = FALSE)

# Check the data
dim(penguins)
names(penguins)
summary(penguins)
sum(is.na(penguins))

# Missing values
colSums(is.na(penguins))

numeric_vars <- c("bill_length_mm", "bill_depth_mm",
                  "flipper_length_mm", "body_mass_g")

# Remove rows where all four measurements are missing
penguins_clean <- penguins %>%
  filter(!if_all(all_of(numeric_vars), is.na))

# Fill numerical missing values with species median
penguins_clean <- penguins_clean %>%
  group_by(species) %>%
  mutate(across(all_of(numeric_vars),
                ~replace_na(.x, median(.x, na.rm = TRUE)))) %>%
  ungroup()

# Keep missing sex values as Unknown
penguins_clean <- penguins_clean %>%
  mutate(sex = replace_na(sex, "Unknown"))

dim(penguins_clean)

# Outlier check using IQR
outlier_check <- map_dfr(numeric_vars, function(v) {
  x <- penguins_clean[[v]]
  q1 <- quantile(x, .25, na.rm = TRUE)
  q3 <- quantile(x, .75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr

  tibble(variable = v,
         lower = lower,
         upper = upper,
         outliers = sum(x < lower | x > upper, na.rm = TRUE))
})

print(outlier_check)

# Standardisation
penguins_scaled <- penguins_clean %>%
  mutate(across(all_of(numeric_vars), ~as.numeric(scale(.x))))

# Basic analysis
table(penguins_clean$species)

penguins_clean %>%
  group_by(species) %>%
  summarise(
    n = n(),
    mean_bill_length = mean(bill_length_mm),
    mean_bill_depth = mean(bill_depth_mm),
    mean_flipper_length = mean(flipper_length_mm),
    mean_body_mass = mean(body_mass_g)
  )

# Correlation
cor(penguins_clean %>% select(all_of(numeric_vars)))

# Plots
ggplot(penguins_clean, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species")

ggplot(penguins_clean,
       aes(x = flipper_length_mm, y = body_mass_g, colour = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Flipper Length vs Body Mass")
