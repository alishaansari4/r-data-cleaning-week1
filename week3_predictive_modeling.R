library(tidyverse)
library(caret)

# 1. Load Cleaned Dataset
penguins <- read_csv("penguins_cleaned.csv", show_col_types = FALSE) %>%
  filter(sex != "Unknown")

# 2. Hypothesis Testing
# ANOVA across species
anova_res <- aov(body_mass_g ~ species, data = penguins)
summary(anova_res)

# Two-sample t-test across sex
t_test_res <- t.test(body_mass_g ~ sex, data = penguins)
print(t_test_res)

# 3. Train/Test Split (80/20)
set.seed(42)
train_idx <- createDataPartition(penguins$body_mass_g, p = 0.8, list = FALSE)
train_set <- penguins[train_idx, ]
test_set  <- penguins[-train_idx, ]

# 4. Multiple Linear Regression Model
model <- lm(body_mass_g ~ flipper_length_mm + bill_length_mm + bill_depth_mm + species + sex, 
            data = train_set)
summary(model)

# 5. Model Predictions and Evaluation
predictions <- predict(model, newdata = test_set)
test_rmse <- sqrt(mean((test_set$body_mass_g - predictions)^2))
test_r2 <- cor(test_set$body_mass_g, predictions)^2

cat("Test RMSE:", round(test_rmse, 2), "g\n")
cat("Test R2:", round(test_r2, 4), "\n")

# 6. Diagnostic Plots
par(mfrow = c(1, 2))
plot(model, which = c(1, 2))