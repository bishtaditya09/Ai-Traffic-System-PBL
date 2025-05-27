if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("xgboost")) install.packages("xgboost")

library(dplyr)
library(caret)
library(xgboost)

days <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
          "Friday", "Saturday", "Sunday")
data_path <- "traffic_data.csv"

prepare_data <- function(region = "Delhi") {
  if (!file.exists(data_path)) {
    cat("Generating synthetic traffic data for region:", region, "\n")
    hours <- 0:23
    traffic_data <- expand.grid(Day = days, Hour = hours)
    set.seed(42)
    # Add region-based variation
    base_congestion <- runif(nrow(traffic_data), 20, 90)
    if (region == "Delhi") base_congestion <- base_congestion + 10
    if (region == "Mumbai") base_congestion <- base_congestion + 5
    if (region == "Bangalore") base_congestion <- base_congestion + 15
    traffic_data$Congestion <- pmin(base_congestion, 100)
    write.csv(traffic_data, data_path, row.names = FALSE)
  }

  traffic_data <- read.csv(data_path)
  traffic_data <- traffic_data %>%
    mutate(
      Day = factor(Day, levels = days),
      Hour = as.numeric(Hour),
      IsWeekend = ifelse(Day %in% c("Saturday", "Sunday"), 1, 0),
      IsRushHour = ifelse(Hour %in% c(8:10, 17:19), 1, 0),
      DayFactor = as.numeric(Day),
      HourSin = sin(2 * pi * Hour / 24),
      HourCos = cos(2 * pi * Hour / 24)
    ) %>%
    filter(!is.na(Congestion))
  return(traffic_data)
}

train_model <- function(traffic_data) {
  feature_cols <- c("Hour", "DayFactor", "HourSin", "HourCos", 
                   "IsWeekend", "IsRushHour")
  preproc <- preProcess(traffic_data[, feature_cols], 
                       method = c("center", "scale"))
  traffic_processed <- predict(preproc, traffic_data)
  set.seed(123)
  trainIndex <- createDataPartition(traffic_data$Congestion, p = 0.8, list = FALSE)
  train_data <- traffic_processed[trainIndex, ]
  dtrain <- xgb.DMatrix(data = as.matrix(train_data[, feature_cols]), label = train_data$Congestion)

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
    verbose = 0
  )

  return(list(model = model, preproc = preproc, features = feature_cols))
}

predict_traffic <- function(model_info, day, hour) {
  input_data <- data.frame(
    Day = factor(day, levels = days),
    Hour = as.numeric(hour)
  )

  input_data <- input_data %>%
    mutate(
      IsWeekend = ifelse(Day %in% c("Saturday", "Sunday"), 1, 0),
      IsRushHour = ifelse(Hour %in% c(8:10, 17:19), 1, 0),
      DayFactor = as.numeric(Day),
      HourSin = sin(2 * pi * Hour / 24),
      HourCos = cos(2 * pi * Hour / 24)
    )

  input_processed <- predict(model_info$preproc, input_data)
  prediction <- predict(model_info$model,
                        xgb.DMatrix(data = as.matrix(input_processed[, model_info$features])))
  return(prediction)
}

# === Ensure traffic_data.csv exists on script load ===
if (!file.exists("traffic_data.csv")) {
  cat("Generating traffic_data.csv...\n")
  data <- prepare_data(region = "Delhi")
  write.csv(data, "traffic_data.csv", row.names = FALSE)
}
