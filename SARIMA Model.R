library(vroom)
library(forecast)

# Load data
growth <- vroom::vroom("CompanyGrowth.csv")
growth$Year <- as.integer(growth$Year)

# --- Train/Test Split ---
train <- subset(growth, Year < 2022)
test  <- subset(growth, Year >= 2022)

# --- Create time series object on TRAIN only ---
train_ts <- ts(train$PctGrowth, start = min(train$Year), frequency = 1)

# --- Fit SARIMA model on TRAIN only ---
sarima_model <- auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)

# --- Model summary ---
cat("SARIMA Model Summary:\n")
print(summary(sarima_model))
cat("AIC:", round(AIC(sarima_model), 3), "\n")
cat("BIC:", round(BIC(sarima_model), 3), "\n")

# --- Forecast h steps ahead (based on test set size) ---
h <- nrow(test)
sarima_forecast <- forecast(sarima_model, h = h)

# --- Combine test actuals with predictions ---
results <- data.frame(
  Year                = test$Year,
  Actual_PctGrowth    = test$PctGrowth,
  Predicted_PctGrowth = as.numeric(sarima_forecast$mean),
  Lower_95CI          = as.numeric(sarima_forecast$lower[, 2]),
  Upper_95CI          = as.numeric(sarima_forecast$upper[, 2])
)

# --- Test RMSE ---
test_rmse <- sqrt(mean((results$Actual_PctGrowth - results$Predicted_PctGrowth)^2, na.rm = TRUE))
train_rmse <- sqrt(mean(residuals(sarima_model)^2, na.rm = TRUE))

cat("\nTrain RMSE:", round(train_rmse, 3), "\n")
cat("Test RMSE (2022+):", round(test_rmse, 3), "\n")

# --- Print results ---
print(results)
cat("\nPredicted Percent Growth Values:\n")
cat(round(results$Predicted_PctGrowth, 3), "\n")

# --- Plot forecast vs actuals ---
plot(sarima_forecast,
     main = "SARIMA Forecast vs Actuals (Test: 2022+)",
     xlab = "Year",
     ylab = "Percent Growth")
lines(ts(test$PctGrowth, start = min(test$Year), frequency = 1), col = "red", lwd = 2)
legend("topleft", legend = c("Forecast", "Actual"), col = c("blue", "red"), lty = 1, lwd = 2)




