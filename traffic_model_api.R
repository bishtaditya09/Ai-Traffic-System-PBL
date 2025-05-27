library(plumber)
source("traffic_model_logic.R")

function(day, hour) {
  pred <- predict_traffic(model_info, day, as.numeric(hour))
  list(prediction = round(pred * 100, 1))
}

function() {
  traffic_data <- prepare_data()
  model_info <- train_model(traffic_data)
  return("Model trained and data prepared")
}

function() {
  traffic_data
}
