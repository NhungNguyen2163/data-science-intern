install.packages("ggplot2")
install.packages("reshape2")
install.packages("corrgram")
install.packages("VIM",dependencies = T)
library("VIM")
library('corrgram')
library('reshape2')
library('ggplot2')


# 1.READ THE FILE
data <- read.csv("C:/Users/Nhung/Downloads/R/final/order_july24.csv", header = TRUE, sep = ",")


# 2.CHECK THE DATA
head(data)
str(data)
summary(data)
# 2.1. Check the unique values of the data
sapply(data, function(x) length(unique(x)))
unique(data$age)
unique(data$ad_channel)
unique(data$voucher)
unique(data$past_spend)
unique(data$time_web)
unique(data$spend)
# 2.2. Check the distribution of each variable using bar charts
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2))
cols <- c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lightblue")
vars <- c("time_web", "spend", "past_spend", "age", "ad_channel", "voucher")
for (i in 1:6) {
  barplot(table(data[[vars[i]]]), 
          col = cols[i], 
          main = paste("Distribution of", vars[i]), 
          xlab = vars[i], 
          ylab = "Frequency",
          cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)
}


# 3. HANDLING MISSING VALUES
# 3.1. Check the proportion of missing values in each column and correlation matrix
library(VIM)
aggr(data, col = c("white", "darkred"), numbers = TRUE, proportion = FALSE, 
     cex.axis = 0.8, cex.lab = 1.2, 
     main = "Missing Data Map")
# Plot correlation before handling missing values
library(corrgram)
corrgram(data)
# Add a column indicating missing values
missdata <- data
missdata$missing <- as.numeric(!complete.cases(data))
corrgram(missdata)
# 3.2. Imputate the missing values
# Function to calculate Mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# Function to calculate Median
get_median <- function(x) {
  median(x, na.rm = TRUE)
}
# Create a copy of the data to modify (to avoid altering the original data)
data_no_na <- data
# Replace NA with Mode for specific columns
mode_columns <- c("voucher", "age", "ad_channel")
for (col in mode_columns) {
  mode_value <- get_mode(data_no_na[[col]])
  data_no_na[[col]][is.na(data_no_na[[col]])] <- mode_value
  print(paste("Replaced NA in column", col, "with mode:", mode_value))
}
# Replace NA with Median for specific columns
median_columns <- c("time_web", "spend", "past_spend")
for (col in median_columns) {
  median_value <- get_median(data_no_na[[col]])
  data_no_na[[col]][is.na(data_no_na[[col]])] <- median_value
  print(paste("Replaced NA in column", col, "with median:", median_value))
}
# Check the number of NA values after replacement
na_counts_after <- colSums(is.na(data_no_na))
print("Number of NA values after replacement:")
print(na_counts_after)
# View the summary of the modified data
summary(data_no_na)


# Create barplots for variables after replacing missing values
par(mfrow = c(2, 3), mar = c(5, 5, 4, 2)) 
cols <- c("lightblue", "lightgreen", "lightcoral", "lightyellow", "lightpink", "lightblue") 
vars <- c("time_web", "spend", "past_spend", "age", "ad_channel", "voucher") 

for (i in 1:6) {
  barplot(
    table(data_no_na[[vars[i]]]), 
    col = cols[i],  
    main = paste("Distribution of", vars[i]),  
    xlab = vars[i],  
    ylab = "Frequency",  
    cex.main = 1.5,  
    cex.lab = 1.3,  
    cex.axis = 1.2  
  )
}

# Create a correlation plot for variables after replacing missing values
library(corrgram)
corrgram(data_no_na)

# Assign data_no_na to df 
df <- data_no_na


# 4.DISTRIBUTION OF VOUCHER USAGE BY MARKETING CHANNEL
# Calculate frequency table
voucher_table <- table(df$ad_channel, df$voucher) 
voucher_rate <- prop.table(voucher_table, margin = 1) * 100 
# Create contingency table for ad_channel and voucher
contingency_table <- table(df$ad_channel, df$voucher)
print(contingency_table)
# Perform Chi-Square Test
chi_test <- chisq.test(contingency_table)
chi_test

# 5.MARKETING BY AGE ACROSS CHANNELS
ggplot(df, aes(x = as.factor(ad_channel), y = age, fill = as.factor(ad_channel))) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot of Age by Ad Channel",
    x = "Ad Channel",
    y = "Age",
    fill = "Ad Channel"
  ) + theme_minimal() + scale_fill_brewer(palette = "Set3")
# ANOVA to test the relationship between ad_channel and age
anova_model <- aov(age ~ as.factor(ad_channel), data = df)
summary(anova_model)

vars <- df[, c("time_web", "spend", "past_spend", "age")]
# Scatter plot: Spend vs Time on Website
ggplot(vars, aes(x = time_web, y = spend)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Spend vs Time on Website", x = "Time on Website", y = "Spend") +
  theme_minimal()
# Scatter plot: Spend vs Past Spend
ggplot(vars, aes(x = past_spend, y = spend)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Spend vs Past Spend", x = "Past Spend", y = "Spend") +
  theme_minimal()
# Scatter plot: Spend vs Age
ggplot(vars, aes(x = age, y = spend)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Spend vs Age", x = "Age", y = "Spend") +
  theme_minimal()

# Regression: Spend vs Time on Website
model_time_web <- lm(spend ~ time_web, data = df)
summary(model_time_web)
# Regression: Spend vs Past Spend
model_past_spend <- lm(spend ~ past_spend, data = df)
summary(model_past_spend)
# Regression: Spend vs Age
model_age <- lm(spend ~ age, data = df)
summary(model_age)


# 6. ANALYSIS OF SPEND AND PAST_SPEND WITH VOUCHER
# Separate data by voucher usage
voucher_1 <- df[df$voucher == 1, "spend"]  
voucher_0 <- df[df$voucher == 0, "spend"]  
# Calculate the mean and standard deviation for both groups
mean_yes <- mean(voucher_1)
mean_no <- mean(voucher_0)
sd_yes <- sd(voucher_1)
sd_no <- sd(voucher_0)
# Compare the two groups using a t-test
t_test_result <- t.test(voucher_1, voucher_0)
# Display the results
cat("Average spending of the group using the voucher:", mean_yes, "\n")
cat("Average spending of the group not using the voucher:", mean_no, "\n")
cat("T-test results:\n")
t_test_result


# Split data into return and new customers
return_cus <- df[df$past_spend > 0, ]  # Return customers
new_cus <- df[df$past_spend == 0, ]   # New customers
# T-test for return customers
t_test_return <- t.test(
  return_cus[return_cus$voucher == 1, "spend"], 
  return_cus[return_cus$voucher == 0, "spend"]
)
# T-test for new customers
t_test_new <- t.test(
  new_cus[new_cus$voucher == 1, "spend"], 
  new_cus[new_cus$voucher == 0, "spend"]
)
# Print results
cat("Return customers:\n")
t_test_return
cat("\nNew customers:\n")
t_test_new

# Density plot of past_spend and voucher
ggplot(df, aes(x = past_spend, color = factor(voucher), fill = factor(voucher))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Past Spend by Voucher",
       x = "Past Spend",
       y = "Density",
       color = "Voucher",
       fill = "Voucher") +
  theme_minimal()
# Density plot of spend and voucher
ggplot(df, aes(x = spend, color = factor(voucher), fill = factor(voucher))) +
  geom_density(alpha = 0.5) + 
  labs(title = "Density Plot of Spend by Voucher",
       x = "Spend",
       y = "Density",
       color = "Voucher",
       fill = "Voucher") +
  theme_minimal()

# 7. ANALYSIS OF SPEND AND PAST_SPEND BY AD_CHANNEL
# ANOVA to test the differences between ad_channel groups
anova_model <- aov(spend ~ factor(ad_channel), data = df)
summary(anova_model)
tukey_result <- TukeyHSD(anova_model)
tukey_result
mean_spend_by_channel <- aggregate(spend ~ ad_channel, data = df, mean)
mean_spend_by_channel

# Density plot of past_spend by ad_channel
ggplot(df, aes(x = past_spend, color = factor(ad_channel), fill = factor(ad_channel))) +
  geom_density(alpha = 0.5) +  # Semi-transparent for better visibility
  labs(title = "Density Plot of Past Spend by Ad Channel",
       x = "Past Spend",
       y = "Density",
       color = "Ad Channel",
       fill = "Ad Channel") +
  theme_minimal()

# Density plot of spend by ad_channel
ggplot(df, aes(x = spend, color = factor(ad_channel), fill = factor(ad_channel))) +
  geom_density(alpha = 0.5) +  # Semi-transparent for better visibility
  labs(title = "Density Plot of Spend by Ad Channel",
       x = "Spend",
       y = "Density",
       color = "Ad Channel",
       fill = "Ad Channel") +
  theme_minimal()


# 8. MODELLING & PREDICTION 
# Check multicollinearity
install.packages("car")  
library(car)
model <- lm(spend ~ ., data = df) 
vif_values <- vif(model)  
print(vif_values)

# Split data into training and testing sets
datasplit <- df  
smp_size <- floor(0.75 * nrow(datasplit))  
set.seed(44)  
train_ind <- sample(seq_len(nrow(datasplit)), size = smp_size) 
train <- datasplit[train_ind, ]  
test <- datasplit[-train_ind, ] 
cat("Training data dimensions:", dim(train)[1], "rows,", dim(train)[2], "columns\n")
cat("Test data dimensions:", dim(test)[1], "rows,", dim(test)[2], "columns\n")


#-----------LINEAR REGRESSION-----------
# Build a linear regression model on the training data
modeltrainsplit <- lm(spend ~ past_spend + age + time_web, data = train)
summary(modeltrainsplit)  
prediction <- predict(modeltrainsplit, newdata = test)
rmse <- sqrt(mean((test$spend - prediction)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")


#-----------RIDGE REGRESSION-----------
install.packages("glmnet")  
library(glmnet)
x_train <- as.matrix(train[, c("past_spend", "age", "time_web")]) 
y_train <- train$spend  
x_test <- as.matrix(test[, c("past_spend", "age", "time_web")])  
y_test <- test$spend  
ridge_model <- glmnet(x_train, y_train, alpha = 0)
cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0)  # Cross-validation for Ridge
best_lambda <- cv_ridge$lambda.min  # Optimal lambda
cat("Best lambda for Ridge:", best_lambda, "\n")
ridge_prediction <- predict(ridge_model, s = best_lambda, newx = x_test)  
ridge_rmse <- sqrt(mean((y_test - ridge_prediction)^2))  
cat("Ridge Regression RMSE:", ridge_rmse, "\n")


#----- Calculate MAE 
lr_mae <- mean(abs(test$spend - prediction))  
ridge_mae <- mean(abs(test$spend - ridge_prediction))
cat("Linear Regression MAE:", lr_mae, "\n")
cat("Ridge Regression MAE:", ridge_mae, "\n")

#------- Calculate R-squared
lr_r2 <- summary(modeltrainsplit)$r.squared
ridge_r2 <- 1 - sum((test$spend - ridge_prediction)^2) / sum((test$spend - mean(test$spend))^2)
cat("Linear Regression R-squared:", lr_r2, "\n")
cat("Ridge Regression R-squared:", ridge_r2, "\n")


# Predict on the training dataset
train_prediction <- predict(modeltrainsplit, newdata = train)
# Calculate RMSE for the training dataset
train_rmse <- sqrt(mean((train$spend - train_prediction)^2))
cat("Training RMSE:", train_rmse, "\n")

# Predict on the testing dataset
test_prediction <- predict(modeltrainsplit, newdata = test)
# Calculate RMSE for the testing dataset
test_rmse <- sqrt(mean((test$spend - test_prediction)^2))
cat("Test RMSE:", test_rmse, "\n")

# Load new customer data from CSV
predict_df <- read.csv("C:/Users/Nhung/Downloads/R/final/new_customer24.csv")
head(predict_df)

# Predict spending for the new customer data
predictions_data <- predict(modeltrainsplit, newdata = predict_df)
predict_df$predicted_spend <- round(predictions_data, 2) 
print(predict_df)

# Save the prediction results to an Excel file
install.packages("openxlsx")  
library(openxlsx)
write.xlsx(predict_df, file = "predict_df.xlsx")


