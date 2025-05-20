if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("xgboost")) install.packages("xgboost")
library(dplyr)
library(caret)
library(xgboost)

prepare_data <- function() {
  traffic_data <- read.csv("traffic_data.csv")
  days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  traffic_data <- traffic_data %>%
    mutate(
      Day = factor(Day, levels = days),
      Hour = as.numeric(Hour),
      IsWeekend = ifelse(Day %in% c("Saturday","Sunday"), 1, 0),
      IsRushHour = ifelse(Hour %in% c(8:10,17:19), 1, 0),
      DayFactor = as.numeric(Day),
      HourSin = sin(2*pi*Hour/24),
      HourCos = cos(2*pi*Hour/24)
    ) %>%
    filter(!is.na(Congestion))
  return(traffic_data)
}

train_model <- function(traffic_data) {
  feature_cols <- c("Hour","DayFactor","HourSin","HourCos","IsWeekend","IsRushHour")
  preproc <- preProcess(traffic_data[,feature_cols], method=c("center","scale"))
  traffic_processed <- predict(preproc, traffic_data)
  set.seed(123)
  trainIndex <- createDataPartition(traffic_data$Congestion, p=0.8, list=FALSE)
  train_data <- traffic_processed[trainIndex,]
  test_data <- traffic_processed[-trainIndex,]
  raw_test_data <- traffic_data[-trainIndex,]
  dtrain <- xgb.DMatrix(data=as.matrix(train_data[,feature_cols]), label=train_data$Congestion)
  dtest <- xgb.DMatrix(data=as.matrix(test_data[,feature_cols]), label=test_data$Congestion)
  params <- list(
    booster="gbtree",
    objective="reg:squarederror",
    eta=0.1,
    max_depth=6,
    subsample=0.8,
    colsample_bytree=0.8
  )
  model <- xgb.train(
    params,
    data=dtrain,
    nrounds=100,
    watchlist=list(train=dtrain, test=dtest),
    early_stopping_rounds=10
  )
  return(list(model=model, preproc=preproc, features=feature_cols, test_data=test_data, raw_test_data=raw_test_data))
}

evaluate_model <- function(model_info) {
  predictions <- predict(model_info$model, 
                         xgb.DMatrix(data=as.matrix(model_info$test_data[,model_info$features])))
  actuals <- model_info$test_data$Congestion
  mae <- mean(abs(actuals - predictions))
  rmse <- sqrt(mean((actuals - predictions)^2))
  r_squared <- 1 - (sum((actuals-predictions)^2)/sum((actuals-mean(actuals))^2))
  return(list(mae=mae, rmse=rmse, r_squared=r_squared, predictions=predictions, actuals=actuals))
}

print_test_cases <- function(evaluation) {
  cat("\n=== Model Evaluation Metrics ===\n")
  cat(sprintf("R-squared: %.2f%%\n", evaluation$r_squared*100))
  cat(sprintf("MAE: %.2f\n", evaluation$mae))
  cat(sprintf("RMSE: %.2f\n", evaluation$rmse))
  cat("\n=== Sample Test Cases (5 random samples) ===\n")
  sample_indices <- sample(1:length(evaluation$actuals), 5)
  sample_results <- data.frame(
    Day = model_info$raw_test_data$Day[sample_indices],
    Hour = model_info$raw_test_data$Hour[sample_indices],
    Actual = evaluation$actuals[sample_indices],
    Predicted = round(evaluation$predictions[sample_indices], 1)
  )
  print(sample_results)
}

predict_traffic <- function(model_info, day, hour) {
  input_data <- data.frame(
    Day=factor(day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")),
    Hour=as.numeric(hour)
  )
  input_data <- input_data %>%
    mutate(
      IsWeekend=ifelse(Day %in% c("Saturday","Sunday"), 1, 0),
      IsRushHour=ifelse(Hour %in% c(8:10,17:19), 1, 0),
      DayFactor=as.numeric(Day),
      HourSin=sin(2*pi*Hour/24),
      HourCos=cos(2*pi*Hour/24)
    )
  input_processed <- predict(model_info$preproc, input_data)
  prediction <- predict(model_info$model, 
                        xgb.DMatrix(data=as.matrix(input_processed[,model_info$features])))
  return(prediction)
}

cat("=== Traffic Congestion Predictor ===\n")
traffic_data <- prepare_data()
model_info <- train_model(traffic_data)
evaluation <- evaluate_model(model_info)

repeat {
  cat("\n=== New Prediction ===\n")
  day_input <- readline(prompt="Enter day (e.g., Monday): ")
  hour_input <- as.numeric(readline(prompt="Enter hour (0-23): "))
  valid_days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  if(!(day_input %in% valid_days)) {
    cat("Error: Invalid day\n")
    next
  }
  if(hour_input < 0 | hour_input > 23) {
    cat("Error: Hour must be 0-23\n")
    next
  }
  prediction <- predict_traffic(model_info, day_input, hour_input)
  cat("\n=== Prediction Result ===\n")
  cat(sprintf("Time: %s at %d:00\n", day_input, hour_input))
  cat(sprintf("Predicted congestion: %.1f%%\n", prediction))
  if(prediction >= 70) cat("Status: 🛑 High congestion\n")
  else if(prediction >= 40) cat("Status: ⚠️ Moderate congestion\n")
  else cat("Status: ✅ Low congestion\n")
  print_test_cases(evaluation)
  another <- tolower(readline(prompt="Another prediction? (y/n): "))
  if(another != "y") break
}

cat("\nSession ended. Model accuracy maintained:\n")
print_test_cases(evaluation)

