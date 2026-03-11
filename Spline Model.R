
library(tidyverse)
library(splines)

growth <- vroom::vroom("CompanyGrowth.csv")

# --- Ensure Year is numeric (adjust if your column is named differently) ---
# If your year column is a date, extract year like: growth$Year <- as.integer(format(growth$Date, "%Y"))
growth$Year <- as.integer(growth$Year)

# --- Train/Test split ---
train <- subset(growth, Year < 2022)
test  <- subset(growth, Year >= 2022 & Year <= 2024)


cuts <- c(1990,2005,2015)

lm_spline <- lm(PctGrowth ~ bs(Year+((Qrtr-1)*0.25), knots = cuts, degree = 3)+Income + Production + Savings + Unemployment, data = train)

spline_pred_train <- predict(lm_spline, newdata = train)

train %>%
  ggplot(aes(x = (Year+((Qrtr-1)*0.25)), y = PctGrowth))+
  geom_point()+
  geom_line(aes(y = spline_pred_train), color = "red")+
  labs(
    title = "Spline Fit"
  )


# --- AIC/BIC from the TRAIN fit ---
train_aic <- AIC(lm_spline)
train_bic <- BIC(lm_spline)

# --- Predict on TEST and compute RMSE ---
test_pred <- predict(lm_spline, newdata = test)
test_rmse <- sqrt(mean((test$PctGrowth - test_pred)^2, na.rm = TRUE))

# (Optional) Train RMSE for comparison
train_pred <- predict(lm_spline, newdata = train)
train_rmse <- sqrt(mean((train$PctGrowth - train_pred)^2, na.rm = TRUE))

# --- Tiny summary printout ---
cat(
  "Train AIC:", round(train_aic, 3), "\n",
  "Train BIC:", round(train_bic, 3), "\n",
  "Train RMSE:", round(train_rmse, 3), "\n",
  "Test RMSE (2022–2024):", round(test_rmse, 3), "\n"
)



# --- Predictions ---

test$PredictedGrowth <- predict(lm_spline, newdata = test)
# --- Evaluation ---

mse <- mean((test$PctGrowth - test$PredictedGrowth)^2)
cat("Mean Squared Error:", mse, "\n")



test_2025 <- test[1,]
test_2025$Year <- 2025
test_2025$Qrtr <- 1
test_2025$Income <- inc_pred
test_2025$Production <- prod_pred
test_2025$Savings <- sav_pred
test_2025$Unemployment <- unem_pred

test_2025$PredictedGrowth <- predict(lm_spline, newdata = test_2025)
cat("Predicted Growth for 2025 Q1:", test_2025$PredictedGrowth, "\n")
