# Load packages
library(readxl)
library(forecast)

# Load data
enrollment <- read_excel(
  "tabn203.10.xlsx",
  sheet = "Digest 2022 Table 203.10",
  range = "A4:U38",
  col_types = c(
    "text",
    "skip",
    "skip",
    "numeric",
    "skip",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "skip",
    "numeric",
    "numeric",
    "numeric",
    "numeric",
    "numeric"
  )
)

enrollment$enrollment_total <-
  rowSums(enrollment[,-1], na.rm = TRUE)

# Prepare the time series data
enrollment_ts <-
  ts(
    enrollment$enrollment_total,
    start = 1990,
    end = 2021,
    frequency = 1
  )

# Split the data into training and testing sets
train_end <- 2015
train_ts <- window(enrollment_ts, end = train_end)
test_ts <- window(enrollment_ts, start = train_end + 1)

# Specify the exponential smoothing model components
models <- c("AAN", "MNN")

for (model_spec in models) {
  cat("Model:", model_spec, "\n")
  enrollment_model <- ets(train_ts, model = model_spec)
  
  # Review the model
  summary(enrollment_model)
  
  # Evaluate model diagnostics
  checkresiduals(enrollment_model)
  
  # Make predictions on the testing data
  enrollment_forecast <- forecast(enrollment_model, h = length(test_ts))
  
  # View forecast
  plot(enrollment_forecast, main = paste("Forecast -", model_spec))
  print(enrollment_forecast)
  
  # Evaluate model accuracy
  accuracy_results <- accuracy(enrollment_forecast, test_ts)
  print(accuracy_results)
  
  cat("\n")
}