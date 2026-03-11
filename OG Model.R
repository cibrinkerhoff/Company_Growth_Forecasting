library(vroom)

# Load data
growth <- vroom::vroom("CompanyGrowth.csv")

# --- Ensure Year is numeric (adjust if your column is named differently) ---
# If your year column is a date, extract year like: growth$Year <- as.integer(format(growth$Date, "%Y"))
growth$Year <- as.integer(growth$Year)

# --- Train/Test split ---
train <- subset(growth, Year < 2022)
test  <- subset(growth, Year >= 2022)

# --- Fit model on TRAIN only ---
lm_growth <- lm(PctGrowth ~ Income + Production + Savings + Unemployment, data = train)

# --- AIC/BIC from the TRAIN fit ---
train_aic <- AIC(lm_growth)
train_bic <- BIC(lm_growth)

# --- Predict on TEST and compute RMSE ---
test_pred <- predict(lm_growth, newdata = test)
test_rmse <- sqrt(mean((test$PctGrowth - test_pred)^2, na.rm = TRUE))

# (Optional) Train RMSE for comparison
train_pred <- predict(lm_growth, newdata = train)
train_rmse <- sqrt(mean((train$PctGrowth - train_pred)^2, na.rm = TRUE))

# --- Tiny summary printout ---
cat(
  "Train AIC:", round(train_aic, 3), "\n",
  "Train BIC:", round(train_bic, 3), "\n",
  "Train RMSE:", round(train_rmse, 3), "\n",
  "Test RMSE (2022–2024):", round(test_rmse, 3), "\n"
)


#Predicted values 
unem_pred
inc_pred
prod_pred
sav_pred


# --- Future Predictions using your pre-defined predictor values ---
# Build a new data frame from your forecast variables
future_data <- data.frame(
  Unemployment = unem_pred,
  Income       = inc_pred,
  Production   = prod_pred,
  Savings      = sav_pred
)
lm_growth <- lm(PctGrowth ~ Income + Production + Savings + Unemployment, data = growth)


# Predict PctGrowth using the trained model
future_predictions <- predict(lm_growth, newdata = future_data, interval = "prediction", level = 0.95)

# Combine inputs with predicted output
results <- cbind(future_data, as.data.frame(future_predictions))
colnames(results)[colnames(results) == "fit"]  <- "Predicted_PctGrowth"
colnames(results)[colnames(results) == "lwr"]  <- "Lower_95CI"
colnames(results)[colnames(results) == "uwr"]  <- "Upper_95CI"

# Print results
print(results)

cat("\nPredicted Percent Growth Values:\n")
cat(round(results$Predicted_PctGrowth, 3), "\n")
