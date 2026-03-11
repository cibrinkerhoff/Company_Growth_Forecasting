library(vroom)
library(prophet)
library(dplyr)

# =============================================================================
# LOAD & PREPARE DATA
# =============================================================================

growth <- vroom::vroom("CompanyGrowth.csv")
growth$Year <- as.integer(growth$Year)

# Prophet requires columns named exactly 'ds' (date) and 'y' (response)
# Convert Year to a Date (Jan 1 of each year)
growth_prophet <- growth %>%
  mutate(
    ds = as.Date(paste0(Year, "-01-01")),
    y  = PctGrowth
  ) %>%
  arrange(ds)

# =============================================================================
# TRAIN / TEST SPLIT  (same cut as your LM: train < 2022, test >= 2022)
# =============================================================================

train_p <- growth_prophet %>% filter(Year < 2022)
test_p  <- growth_prophet %>% filter(Year >= 2022)

# =============================================================================
# BUILD & FIT PROPHET MODEL WITH ALL REGRESSORS
# =============================================================================

m <- prophet(yearly.seasonality  = FALSE,   # annual data – no intra-year cycles
             weekly.seasonality  = FALSE,
             daily.seasonality   = FALSE,
             seasonality.mode    = "additive")

# Add each explanatory variable as an extra regressor
m <- add_regressor(m, "Income")
m <- add_regressor(m, "Production")
m <- add_regressor(m, "Savings")
m <- add_regressor(m, "Unemployment")

# Fit on training data (must include ds, y, and all regressors)
m <- fit.prophet(m, train_p %>% select(ds, y, Income, Production, Savings, Unemployment))

# =============================================================================
# IN-SAMPLE (TRAIN) PREDICTIONS
# =============================================================================

train_forecast <- predict(m, train_p %>% select(ds, Income, Production, Savings, Unemployment))
train_pred     <- train_forecast$yhat
train_rmse     <- sqrt(mean((train_p$y - train_pred)^2, na.rm = TRUE))

# =============================================================================
# OUT-OF-SAMPLE (TEST) PREDICTIONS
# =============================================================================

test_forecast <- predict(m, test_p %>% select(ds, Income, Production, Savings, Unemployment))
test_pred     <- test_forecast$yhat
test_rmse     <- sqrt(mean((test_p$y - test_pred)^2, na.rm = TRUE))

cat("=== Prophet Model Performance ===\n")
cat("Train RMSE :", round(train_rmse, 3), "\n")
cat("Test  RMSE :", round(test_rmse,  3), "\n\n")

# =============================================================================
# FUTURE PREDICTIONS
# (Replace the vectors below with your actual forecast values)
# =============================================================================

# ---- EDIT THESE VECTORS: one value per future year you want to predict ------
future_years  <- 2025     # <-- adjust as needed
unem_pred     <- -0.1215834     # <-- your Unemployment forecasts
inc_pred      <- 0.6129535   # <-- your Income forecasts
prod_pred     <- 0.3172247    # <-- your Production forecasts
sav_pred      <- 2.320703    # <-- your Savings forecasts
# -----------------------------------------------------------------------------

future_data <- data.frame(
  ds           = as.Date(paste0(future_years, "-01-01")),
  Income       = inc_pred,
  Production   = prod_pred,
  Savings      = sav_pred,
  Unemployment = unem_pred
)

future_forecast <- predict(m, future_data)

# Prophet's built-in uncertainty intervals (yhat_lower / yhat_upper)
results <- future_data %>%
  mutate(
    Year                = future_years,
    Predicted_PctGrowth = round(future_forecast$yhat,       3),
    Lower_95CI          = round(future_forecast$yhat_lower, 3),
    Upper_95CI          = round(future_forecast$yhat_upper, 3)
  ) %>%
  select(Year, Income, Production, Savings, Unemployment,
         Predicted_PctGrowth, Lower_95CI, Upper_95CI)

cat("=== Future Predictions ===\n")
print(results)

cat("\nPredicted Percent Growth Values:\n")
cat(results$Predicted_PctGrowth, "\n")

################

m <- prophet(yearly.seasonality  = FALSE,   # annual data – no intra-year cycles
             weekly.seasonality  = FALSE,
             daily.seasonality   = FALSE,
             seasonality.mode    = "additive")

# Add each explanatory variable as an extra regressor
m <- add_regressor(m, "Income")
m <- add_regressor(m, "Production")
m <- add_regressor(m, "Savings")
m <- add_regressor(m, "Unemployment")

# Fit on training data (must include ds, y, and all regressors)
m <- fit.prophet(m, growth_prophet %>% select(ds, y, Income, Production, Savings, Unemployment))

future_forecast <- predict(m, future_data)

# Prophet's built-in uncertainty intervals (yhat_lower / yhat_upper)
results <- future_data %>%
  mutate(
    Year                = future_years,
    Predicted_PctGrowth = round(future_forecast$yhat,       3),
    Lower_95CI          = round(future_forecast$yhat_lower, 3),
    Upper_95CI          = round(future_forecast$yhat_upper, 3)
  ) %>%
  select(Year, Income, Production, Savings, Unemployment,
         Predicted_PctGrowth, Lower_95CI, Upper_95CI)

cat("=== Future Predictions ===\n")
print(results)

cat("\nPredicted Percent Growth Values:\n")
cat(results$Predicted_PctGrowth, "\n")

