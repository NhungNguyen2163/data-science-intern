#========================== PART 1. PCE FORECASTING ==============================================
install.packages("forecast")
install.packages("imputeTS")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("zoo")  
install.packages("tseries")
library(lubridate)
library(tseries)
library(zoo)
library(tidyr)
library(dplyr)
library(forecast)
library(imputeTS)
library(ggplot2)
library(scales)


#================ 1. LOAD DATA
data <- read.csv("C:/Users/Nhung/Downloads/R/Forecasting/assignment/PCE.csv", header = TRUE, sep = ",")
head(data)
summary(data)
str(data)

# =============== 2. PREPROCESSING & EDA
# Check for duplicates
dup <- anyDuplicated(data$observation_date)
if (dup == 0) {print("No duplicates!")} else {print(paste("Duplicate exists at:", dup))}

# Check for the range of time
table(format(as.Date(data$observation_date), "%Y"))

# Creat time series
datasetTS <- ts(data$PCE, start = c(1959, 1), frequency = 12)
plot(datasetTS, ylab = "PCE", main = "Personal Consumption Expenditures (PCE) from 01/1959 - 12/2024")
grid()


# Visual the mising value 
cat("Number of missing values:", sum(is.na(datasetTS)), "\n")
ggplot_na_distribution(datasetTS)
statsNA(datasetTS)
ggplot_na_gapsize(datasetTS)
ggplot_na_distribution2(datasetTS)

# Handling missing values by Kalman, Moving Average, Interpolation
ts_clean1 <- na_kalman(datasetTS, model = "StructTS")
ts_clean2 <- na_ma(datasetTS, k = 4, weighting = "exponential")
ts_clean3 <- na_interpolation(datasetTS)
ts_clean4 <- na_kalman(datasetTS, model = "auto.arima")

# Plot to compare (cannot evaluate manually since no significant differences)
test <- cbind(datasetTS,ts_clean1, ts_clean2, ts_clean3, ts_clean4)
plot(test)

# Evaluate by compare MAE
# Get indices of missing values 
na_index <- which(is.na(datasetTS))
methods <- list(
  Kalman_StructTS = ts_clean1,
  MovingAvg = ts_clean2,
  Interpolation = ts_clean3,
  Kalman_ARIMA = ts_clean4
)

# Calculate average value across all methods
mae_matrix <- outer(names(methods), names(methods), Vectorize(function(i, j) {
  mean(abs(methods[[i]][na_index] - methods[[j]][na_index]))}))

mae_matrix <- round(mae_matrix, 2)
mae_matrix

# Calculate average value across all methods
average_ref <- (ts_clean1 + ts_clean2 + ts_clean3 + ts_clean4) / 4

# Calculate each method 
mae_avg <- data.frame(
  Method = c("Kalman_StructTS", "MovingAvg", "Interpolation", "Kalman_ARIMA"),
  MAE = c(
    mean(abs(ts_clean1[na_index] - average_ref[na_index])),
    mean(abs(ts_clean2[na_index] - average_ref[na_index])),
    mean(abs(ts_clean3[na_index] - average_ref[na_index])),
    mean(abs(ts_clean4[na_index] - average_ref[na_index]))
  )
)
print(mae_avg)

# With seasonally adjusted PCE data, the "StructTS" model is often preferred 
# due to its better ability to capture macroeconomic trends.=> imputate by ts_clean1

# Check stationary (using Augmented Dickey-Fuller Test)
adf.test(ts_clean1)


# Zoom in on more recent data (2004-2024)
dataset_zoom <- window(datasetTS, start = c(2004, 1))
kalman_zoom <- window(ts_clean1, start = c(2004, 1))

data_plot_zoom <- data.frame(
  date = as.Date(as.yearmon(time(dataset_zoom))),
  Kalman_StructTS = as.numeric(kalman_zoom),
  Label = ifelse(is.na(dataset_zoom), "imputed", "known")
)

# Convert to long format for ggplot
data_long_zoom <- data_plot_zoom %>%
  pivot_longer(cols = "Kalman_StructTS", names_to = "Method", values_to = "PCE")

ggplot(data_long_zoom, aes(x = date, y = PCE)) +
  geom_line(color = "grey30") + 
  geom_point(aes(color = Label), size = 1.8) +
  facet_wrap(~ Method, ncol = 1, scales = "free_y") +
  labs(
    title = "Imputed vs Known Values (2004–2024, Kalman_StructTS)",
    subtitle = "Red = imputed values | Blue = known values",
    x = "Year", y = "PCE"
  ) +
  scale_color_manual(values = c("known" = "steelblue", "imputed" = "red")) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

fc <- ts_clean1  # use cleaned data
ggplot_na_imputations(datasetTS, fc)

start_time <- start(fc)
end_time <- end(fc)
start_time
end_time
frequency_fc <- frequency(fc)
frequency_fc

# Full data - STL decomposition on cleaned series (fc)
stl_result <- stl(fc, s.window = "periodic")
plot(stl_result, main = "STL Decomposition of PCE (StructTS Imputed)")

# STL decomposition if only from 2024 (20 years before today)
ts_2004 <- window(ts_clean1, start = c(2004, 1))
stl_2004 <- stl(ts_2004, s.window = "periodic")
plot(stl_2004, main = "STL Decomposition of PCE (2004–2024, StructTS Imputed)")

# Check seasonal pattern
pce_monthly_df <- data.frame( Date = as.Date(as.yearmon(time(fc))), PCE = as.numeric(fc))

pce_monthly_df <- pce_monthly_df %>%
  mutate(Year = year(Date), Month = month(Date, label = TRUE, abbr = TRUE))

pce_recent <- pce_monthly_df %>% filter(Year >= 2004)

# Plot seasonal patterns by year
ggplot(pce_recent, aes(x = Month, y = PCE, group = Year, color = factor(Year))) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(title = "Monthly PCE Pattern by Year (2004 - 2024)", x = "Month", y = "PCE", color = "Year") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11))

ggsubseriesplot(fc) +
  labs(
    title = "Subseries Plot of PCE (1959–2024)",
    y = "PCE (Billions of USD)",
    x = "Month") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

#================== 3. MODELING
# Split train test set
train <- subset(fc, end = length(fc) - 12)
test <- subset(fc, start = length(fc) - 11)

start_time <- start(train)
end_time <- end(train)
start_time <- start(test)
end_time <- end(test)
start_time
end_time

#================= TRAINING
# 1. Simple Models
fcses <- ses(train, h = 12)
fcnaive <- naive(train, h = 12)

# 2. Exponential Smoothing
fcholt <- holt(train, h = 12)
fchw <- hw(train, h = 12)

# 3. ARIMA
model_arima <- auto.arima(train)   # Automatically selected
fcarima <- forecast(model_arima, h = 12)

# Check model summaries
summary(fcses)
summary(fcnaive)
summary(fcholt)
summary(fchw)
summary(fcarima)

# Check residuals
checkresiduals(fcses)
checkresiduals(fcholt)
checkresiduals(fchw)
checkresiduals(fcarima)
Box.test(fcarima$residuals, lag = 20, type = "Ljung-Box")

# Separate chart of ACF and PACF
acf(fcarima$residuals, main = "ACF of ARIMA Residuals")
pacf(fcarima$residuals, main = "PACF of ARIMA Residuals")

#================ TESTING
# Check accuracy on test set
acc_ses <- accuracy(fcses, test)
acc_naive <- accuracy(fcnaive, test)
acc_holt <- accuracy(fcholt, test)
acc_hw <- accuracy(fchw, test)
acc_arima <- accuracy(fcarima, test)

# Combine all results in one table
acc_results <- rbind(
  SES = acc_ses[2,],
  Naive = acc_naive[2,],
  Holt = acc_holt[2,],
  HW = acc_hw[2,],
  ARIMA = acc_arima[2,]
)
print(round(acc_results[, c("RMSE", "MAE", "MAPE")], 2))

# Overfitting check by check accuracy of training and testing set
# Accuracy on training data
acc_train_ses   <- accuracy(fcses$fitted, train)
acc_train_naive <- accuracy(fcnaive$fitted, train)
acc_train_holt  <- accuracy(fcholt$fitted, train)
acc_train_hw    <- accuracy(fchw$fitted, train)
acc_train_arima <- accuracy(farima$fitted, train)

# Extract only RMSE, MAE, MAPE
train_errors <- rbind(
  SES   = acc_train_ses[1, c("RMSE", "MAE", "MAPE")],
  Naive = acc_train_naive[1, c("RMSE", "MAE", "MAPE")],
  Holt  = acc_train_holt[1, c("RMSE", "MAE", "MAPE")],
  HW    = acc_train_hw[1, c("RMSE", "MAE", "MAPE")],
  ARIMA = acc_train_arima[1, c("RMSE", "MAE", "MAPE")]
)
test_errors <- acc_results[, c("RMSE", "MAE", "MAPE")]

# Compare train vs test performance
comparison_overfit <- cbind(
  Train = round(train_errors, 2),
  Test  = round(test_errors, 2)
)
print(comparison_overfit)

# Plot all models' forecasts together
autoplot(fc, series = "Actual") +
  autolayer(fcses$mean, series = "SES") +
  autolayer(fcnaive$mean, series = "Naïve") +
  autolayer(fcholt$mean, series = "Holt") +
  autolayer(fchw$mean, series = "Holt-Winters") +
  autolayer(fcarima$mean, series = "ARIMA") +
  ggtitle("Forecast Comparison of Multiple Models (1959 - 2024)") +
  xlab("Year") + ylab("PCE (Billions of USD)") +
  guides(colour = guide_legend(title = "Model")) +
  theme_minimal()

#Backtest with 2024
real_2024 <- subset(data, format(as.Date(data$observation_date), "%Y") == "2024")

# Forecasting for 2024
forecast_2024 <- forecast(fcarima, h = 12)

# Create comparison table
comparison_2024 <- data.frame(
  Month = format(seq(as.Date("2024-01-01"), by = "month", length.out = 12), "%b %Y"),
  Real_Value = real_2024$PCE,
  Predicted_Value = round(as.numeric(forecast_2024$mean), 2)
)
print(comparison_2024)

#================= FINAL FORECAST FOR 2025 (USING ARIMA - BEST MODEL)
# Fit ARIMA model on the full dataset
final_model <- auto.arima(fc)

# Forecasting 2025
future_forecast <- forecast(final_model, h = 12)
autoplot(future_forecast)

# Extract forecast table
forecast_values <- data.frame(
  Month = format(seq(as.Date("2025-01-01"), by = "month", length.out = 12), "%b %Y"),
  Forecast = round(as.numeric(future_forecast$mean), 2)
)
print(forecast_values)
print(future_forecast)

# Create zoomed visualization of forecast
autoplot(future_forecast) +
  coord_cartesian(xlim = c(2023, 2026), ylim = c(18000, 21500)) +
  ggtitle("12-Month Forecast of US Personal Consumption Expenditures") +
  ylab("PCE (Billions of USD)") +
  xlab("Year") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12))
