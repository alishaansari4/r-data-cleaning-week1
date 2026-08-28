library(tidyverse)
library(caret)

# 1. Ingestion and Cleaning
raw_penguins <- read_csv("https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv", show_col_types = FALSE)
numeric_vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")

penguins_clean <- raw_penguins %>%
  filter(!if_all(all_of(numeric_vars), is.na)) %>%
  group_by(species) %>%
  mutate(across(all_of(numeric_vars), ~replace_na(.x, median(.x, na.rm = TRUE)))) %>%
  ungroup() %>%
  mutate(sex = replace_na(sex, "Unknown"))

# 2. Exploratory Analysis & Visualization
ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.75) + geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Simpson's Paradox in Bill Dimensions") + theme_minimal()

# 3. Statistical Modeling
modeling_data <- penguins_clean %>% filter(sex != "Unknown")
set.seed(42)
train_idx <- createDataPartition(modeling_data$body_mass_g, p = 0.8, list = FALSE)
train_set <- modeling_data[train_idx, ]
test_set  <- modeling_data[-train_idx, ]

model <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + species + sex, data = train_set)
summary(model)

# 4. Evaluation
preds <- predict(model, newdata = test_set)
test_rmse <- sqrt(mean((test_set$body_mass_g - preds)^2))
test_r2 <- cor(test_set$body_mass_g, preds)^2
cat("Final Test RMSE:", round(test_rmse, 2), "g | Test R2:", round(test_r2, 4), "\n")