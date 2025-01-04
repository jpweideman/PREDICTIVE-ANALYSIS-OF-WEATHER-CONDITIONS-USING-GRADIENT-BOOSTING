# Load necessary libraries
library(tidyverse)
library(caret)
library(gbm)
library(ggplot2)
library(tidyr)

# Import the dataset
data <- read.csv("Data/Szeged_cleaned.csv", header = TRUE, sep = ",")
str(data)
data <- data %>% mutate_if(is.character, as.factor)
# Remove irrelevant columns: X and Formatted.Date
data <- data %>% select(-X, -Formatted.Date)
str(data)


# create separate dataframes for temperature and apparent temperature
data_temp <- data %>% select(-Apparent.Temperature..C.)
data_apparent <- data %>% select(-Temperature..C.)



## Temperature

# Plot the distrubution of the temperature
ggplot(data_temp, aes(x = Temperature..C.)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Temperature (C)",
       x = "Temperature (C)", y = "Frequency")
# Save the plot
ggsave("Model_Plots/Temperature_Distribution.png", width = 4, height = 6)
# We can make the assumption that the temperature is normally distributed for the model

# Grid search for hyperparameter tuning

# Hyperparameter tuning for Temperature model
set.seed(123)

grid <- expand.grid(
  n.trees = 1200,
  interaction.depth = c(5, 10, 20 ),
  shrinkage = c(0.001, 0.01, 0.1),
  n.minobsinnode = c(8, 20)
)

# Define control for training
control <- trainControl(
  method = "cv",          # Cross-validation
  number = 5,             # Number of folds
  verboseIter = TRUE      # Show progress
)

# Perform hyperparameter tuning
set.seed(123)
gbm_tuned <- train(
  Temperature..C. ~ ., 
  data = data_temp,
  method = "gbm",
  trControl = control,
  tuneGrid = grid,
  metric = "RMSE",        # Optimize for RMSE
  verbose = FALSE
)

# Print the best parameters and results
print(gbm_tuned$bestTune)
print(gbm_tuned)

#save the grid search results
write.csv(gbm_tuned$results, "Results/GridSearch_Temperature.csv", row.names = FALSE)
# Visualize the tuning results
gbm_grid_plot <- plot(gbm_tuned)
# Save the plot
png("Model_Plots/GridSearch_Temperature.png", width = 8, height = 8, units = "in", res = 300)
print(gbm_grid_plot)
dev.off()

# Train the Gradient Boosting model for Temperature

# We train the model with the best hyperparameters three times 
# with different cv.folds values to show the impact on the model performance
cv_folds_values <- c(2, 5, 20)
# Loop over the cv.folds values
for (cv_folds in cv_folds_values) {
  # Train the model
  set.seed(123)
  gbm_temp <- gbm(
    formula = Temperature..C. ~ .,
    data = data_temp,
    distribution = "gaussian",
    n.trees = 1200,          # Number of trees
    interaction.depth = 20,  # Maximum tree depth
    shrinkage = 0.1,         # Learning rate
    train.fraction = 0.75,   # Fraction of data for training
    cv.folds = cv_folds,     # Cross-validation folds
    n.minobsinnode = 20      # Minimum observations in terminal nodes
  )
  # Save the model
  save(gbm_temp, file = paste0("Models/GBM_Temperature_cv_", cv_folds, ".rda"))

  # Print the rmse of the model
  best_iter <- gbm.perf(gbm_temp, method = "cv")
  pred_temp <- predict(gbm_temp, data_temp, n.trees = best_iter)
  rmse_temp <- sqrt(mean((pred_temp - data_temp$Temperature..C.)^2))
  print(paste("RMSE for Temperature (C) with cv.folds =", cv_folds, ":", rmse_temp))
}
# Higher cv.folds values lead to better model performance, but at the cost of exponentially increasing computation time

# Feature Importance for Temperature model
summary(gbm_temp, n.trees = best_iter, plotit = TRUE)

# Determine the best number of trees from cross-validation
best_iter_cv <- gbm.perf(gbm_temp, method = "cv")
best_iter_cv
# Determine the best number of trees from the train/test split
best_iter_test <- gbm.perf(gbm_temp, method = "test")
best_iter_test

# Plotting the training, testing, and cross-validation errors
png("Model_Plots/Training_Testing_Error_Temperature.png", width = 8, height = 10, units = "in", res = 300)
plot(gbm_temp$train.error, type = "l", xlab = "Number of Trees", ylab = "Error", col = "blue", lty = 1)
lines(gbm_temp$valid.error, col = "red", lty = 1)
lines(gbm_temp$cv.error, col = "green", lty = 1)
abline(v = best_iter_cv, col = "black", lty = 2)
abline(v = best_iter_test, col = "purple", lty = 2)
title(main = "Training, Testing and Cross Validation Error: Temperature Model")
legend("topright", legend = c("Training", "Testing", "Cross-Validation", "Best Iteration (CV)", "Best Iteration (Test)"), col = c("blue", "red", "green", "black", "purple"), lty = c(1, 1, 1, 2, 2))
dev.off()


## Apparent Temperature

# Plot the distribution of the apparent temperature
ggplot(data_apparent, aes(x = Apparent.Temperature..C.)) +
  geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Apparent Temperature (C)",
       x = "Apparent Temperature (C)", y = "Frequency")
# Save the plot
ggsave("Model_Plots/Apparent_Temperature_Distribution.png", width = 4, height = 6)
# We can make the assumption that the apparent temperature is normally distributed for the model.
# Gradient boosting is robust to some deviations from normality, even for bimodal distributions.

# Grid search for hyperparameter tuning

# Hyperparameter tuning for Apparent Temperature model
set.seed(123)

grid <- expand.grid(
  n.trees = 1200,
  interaction.depth = c(10, 20),
  shrinkage = c(0.01, 0.1),
  n.minobsinnode = c(8, 20)
)

# Define control for training
control <- trainControl(
  method = "cv",          # Cross-validation
  number = 2,             # Number of folds
  verboseIter = TRUE      # Show progress
)

# Perform hyperparameter tuning
set.seed(123)
gbm_tuned <- train(
  Apparent.Temperature..C. ~ .,
  data = data_apparent,
  method = "gbm",
  trControl = control,
  tuneGrid = grid,
  metric = "RMSE",        # Optimize for RMSE
  verbose = FALSE
)

# Print the best parameters and results
print(gbm_tuned$bestTune)
print(gbm_tuned)

# Save the grid search results
write.csv(gbm_tuned$results, "Results/GridSearch_Apparent_Temperature.csv", row.names = FALSE)
# Visualize the tuning results
gbm_grid_plot <- plot(gbm_tuned)
# Save the plot
png("Model_Plots/GridSearch_Apparent_Temperature.png", width = 8, height = 8, units = "in", res = 300)
print(gbm_grid_plot)
dev.off()

# Train the Gradient Boosting model for Apparent Temperature

# Train the model
set.seed(123) # nolint: indentation_linter.
gbm_apparent <- gbm( # nolint: indentation_linter.
  formula = Apparent.Temperature..C. ~ .,
  data = data_apparent,
  distribution = "gaussian",
  n.trees = 1200,          # Number of trees
  interaction.depth = 20,  # Maximum tree depth
  shrinkage = 0.1,         # Learning rate
  train.fraction = 0.75,   # Fraction of data for training
  cv.folds = 20,     # Cross-validation folds
  n.minobsinnode = 20      # Minimum observations in terminal nodes
)
# Save the model
save(gbm_apparent, file = paste0("Models/GBM_Apparent_Temperature_cv_20.rda"))

# Print the RMSE of the model
best_iter <- gbm.perf(gbm_apparent, method = "cv")
pred_apparent <- predict(gbm_apparent, data_apparent, n.trees = best_iter)
rmse_apparent <- sqrt(mean((pred_apparent - data_apparent$Apparent.Temperature..C.)^2))
print(paste("RMSE for Apparent Temperature (C) with cv.folds = 20 :", rmse_apparent))
# Feature Importance for Apparent Temperature model
summary(gbm_apparent, n.trees = best_iter, plotit = TRUE)

# Determine the best number of trees from cross-validation for Apparent Temperature
best_iter_cv_apparent <- gbm.perf(gbm_apparent, method = "cv")
best_iter_cv_apparent
# Determine the best number of trees from the train/test split for Apparent Temperature
best_iter_test_apparent <- gbm.perf(gbm_apparent, method = "test")
best_iter_test_apparent

# Plotting the training, testing, and cross-validation errors for Apparent Temperature
png("Model_Plots/Training_Testing_Error_Apparent_Temperature.png", width = 8, height = 10, units = "in", res = 300)
plot(gbm_apparent$train.error, type = "l", xlab = "Number of Trees", ylab = "Error", col = "blue", lty = 1)
lines(gbm_apparent$valid.error, col = "red", lty = 1)
lines(gbm_apparent$cv.error, col = "green", lty = 1)
abline(v = best_iter_cv_apparent, col = "black", lty = 2)
abline(v = best_iter_test_apparent, col = "purple", lty = 2)
title(main = "Training, Testing and Cross Validation Error: Apparent Temperature Model")
legend("topright", legend = c("Training", "Testing", "Cross-Validation", "Best Iteration (CV)", "Best Iteration (Test)"), col = c("blue", "red", "green", "black", "purple"), lty = c(1, 1, 1, 2, 2))
dev.off()



##########################################################################################################################################################################


## XGBoost Model for Temperature Prediction

library(xgboost)
library(dplyr)

# Ensure all columns except Temperature..C. are numeric
data_temp_numeric <- data_temp %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  select(-Temperature..C.)

# Convert data to matrix
data_matrix <- as.matrix(data_temp_numeric)
labels <- data_temp$Temperature..C.

# Create DMatrix for XGBoost
dtrain <- xgb.DMatrix(data = data_matrix, label = labels)

# Define parameters
params <- list(
  objective = "reg:squarederror",  # For regression
  eta = 0.1,
  max_depth = 20,
  subsample = 0.75
  #colsample_bytree = 0.75
)

# Perform cross-validation
set.seed(123)
xgb_cv_results <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 1000,  # Number of boosting rounds
  nfold = 20,  # Number of folds
  metrics = "rmse",  # Metric to evaluate
  early_stopping_rounds = 15,  # Stop if no improvement for 15 rounds
  verbose = 1
)
xgb_cv_results
# Save the cross-validation results
save(xgb_cv_results, file = "Models/XGBoost_CrossValidation_Temperature.rda")
# Final RMSE from cross-validation
final_rmse <- min(xgb_cv_results$evaluation_log$test_rmse_mean)
print(paste("Final RMSE from Cross-Validation:", final_rmse))

# Feature importance
xgb_model <- xgboost(data = dtrain, params = params, nrounds = xgb_cv_results$best_iteration)
xgb_importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance_matrix = xgb_importance)

# Plot training and testing RMSE
# Extract the evaluation log
eval_log <- xgb_cv_results$evaluation_log
# Reshape the data for ggplot2
eval_log_long <- eval_log %>%
  pivot_longer(
    cols = c("train_rmse_mean", "test_rmse_mean"),
    names_to = "Metric",
    values_to = "RMSE"
  )
# Plot training and testing RMSE
ggplot(eval_log_long, aes(x = iter, y = RMSE, color = Metric)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Training and Testing RMSE Over Boosting Rounds",
    x = "Boosting Rounds",
    y = "Root Mean Square Error (RMSE)"
  ) +
  theme_minimal() +
  scale_color_manual(
    values = c("train_rmse_mean" = "blue", "test_rmse_mean" = "red"),
    labels = c("Train RMSE", "Test RMSE")
  ) +
  theme(legend.title = element_blank())
# Save the plot
ggsave("Model_Plots/XGBoost_CrossValidation_Training_Testing_Error_Temperature.png", width = 6, height = 8, bg = "white")



##########################################################################################################################################################################

## Model Comparison of GBM and XGBoost

# Load GBM model
load("Models/GBM_Temperature_cv_20.rda")

# Load XGBoost cross-validation results
load("Models/XGBoost_CrossValidation_Temperature.rda")

# Predict using GBM model
gbm_best_iter <- gbm.perf(gbm_temp, method = "cv")
gbm_pred <- predict(gbm_temp, data_temp, n.trees = gbm_best_iter)

# Compute RMSE 
gbm_rmse <- sqrt(mean((gbm_pred - data_temp$Temperature..C.)^2))
print(paste("GBM Model RMSE:", gbm_rmse))

xgb_rmse <- min(xgb_cv_results$evaluation_log$test_rmse_mean)
print(paste("XGBoost Model RMSE:", xgb_rmse))

comparison <- data.frame(
  Model = c("GBM", "XGBoost"),
  RMSE = c(gbm_rmse, xgb_rmse)
)
print(comparison)
