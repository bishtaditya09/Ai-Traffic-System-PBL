if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("xgboost")) install.packages("xgboost")
if (!require("Matrix")) install.packages("Matrix")

library(dplyr)
library(caret)
library(xgboost)
library(Matrix)

traffic_data <- read.csv("traffic_data.csv")

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

traffic_data$Day <- factor(traffic_data$Day, levels = days)
traffic_data$Hour <- as.numeric(traffic_data$Hour)

if (!"IsWeekend" %in% names(traffic_data)) {
  traffic_data$IsWeekend <- ifelse(traffic_data$Day %in% c("Saturday", "Sunday"), 1, 0)
}
if (!"IsRushHour" %in% names(traffic_data)) {
  traffic_data$IsRushHour <- ifelse(traffic_data$Hour %in% c(8:10, 17:19), 1, 0)
}

traffic_data <- traffic_data %>%
  mutate(
    DayFactor = as.numeric(Day),
    HourSin = sin(2 * pi * Hour / 24),
    HourCos = cos(2 * pi * Hour / 24)
  )

feature_cols <- c("Hour", "DayFactor", "HourSin", "HourCos", "IsWeekend", "IsRushHour")
non_zero_var <- feature_cols[sapply(traffic_data[, feature_cols], var) != 0]

preproc <- preProcess(traffic_data[, non_zero_var], method = c("center", "scale"))
traffic_processed <- predict(preproc, traffic_data)
traffic_processed$Congestion <- traffic_data$Congestion
traffic_processed$Day <- traffic_data$Day
traffic_processed$Hour <- traffic_data$Hour

set.seed(123)
trainIndex <- createDataPartition(traffic_processed$Congestion, p = 0.8, list = FALSE)
train_data <- traffic_processed[trainIndex, ]
test_data <- traffic_processed[-trainIndex, ]

dtrain <- xgb.DMatrix(
  data = as.matrix(train_data[, non_zero_var]),
  label = train_data$Congestion
)
dtest <- xgb.DMatrix(
  data = as.matrix(test_data[, non_zero_var]),
  label = test_data$Congestion
)

params <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

model <- xgb.train(
  params,
  data = dtrain,
  nrounds = 100,
  watchlist = list(train = dtrain, test = dtest),
  print_every_n = 10,
  early_stopping_rounds = 10
)

predictions <- predict(model, dtest)

SSE <- sum((test_data$Congestion - predictions)^2)
SST <- sum((test_data$Congestion - mean(test_data$Congestion))^2)
r_squared <- 1 - (SSE/SST)
mae <- mean(abs(test_data$Congestion - predictions))
rmse <- sqrt(mean((test_data$Congestion - predictions)^2))

cat("=== Model Evaluation ===\n")
cat(sprintf("R-squared: %.2f%%\n", r_squared * 100))
cat(sprintf("MAE: %.2f\n", mae))
cat(sprintf("RMSE: %.2f\n", rmse))

unique_days <- unique(test_data$Day)
sample_indices <- unlist(lapply(unique_days, function(day) {
  which(test_data$Day == day)[1]
}))
sample_indices <- sample_indices[!is.na(sample_indices)]

sample_results <- data.frame(
  Day = test_data$Day[sample_indices],
  Hour = test_data$Hour[sample_indices],
  Actual = test_data$Congestion[sample_indices],
  Predicted = round(predictions[sample_indices], 1)
)

cat("\n=== Sample Predictions ===\n")
print(sample_results)

xgb.save(model, "traffic_congestion_model.xgb")

cat("\n✅ Congestion level prediction completed successfully.\n")
