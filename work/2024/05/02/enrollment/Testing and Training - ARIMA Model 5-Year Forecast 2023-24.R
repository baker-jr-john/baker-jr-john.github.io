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

# Visually inspect the data for seasonal patterns
plot(enrollment_ts)

# Split the data into training and testing sets
train_end <- 2015
train_ts <- window(enrollment_ts, end = train_end)
test_ts <- window(enrollment_ts, start = train_end + 1)

# Fit the model on the training data
enrollment_model <- auto.arima(train_ts)

# Review the model
summary(enrollment_model)

# Evaluate model diagnostics
checkresiduals(enrollment_model)

# Make predictions on the testing data
enrollment_forecast <- forecast(enrollment_model, h = length(test_ts))

# View forecast
plot(enrollment_forecast)
print(enrollment_forecast)

# Evaluate model accuracy
accuracy(enrollment_forecast, test_ts)

# Write forecasted values to a CSV file
# write.csv(enrollment_forecast, "enrollment_forecast.csv")

# Write model coefficients to a CSV file
# write.csv(coef(enrollment_model), "model_coefficients.csv")
