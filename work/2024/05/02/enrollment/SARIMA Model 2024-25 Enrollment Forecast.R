# Load packages
library(readr)
library(forecast)
library(dplyr)

# Load and view the data
enrollment <- read_csv("Enrollment Updates - Seasonal Enrollment History.csv")
# View(enrollment)

# Function to forecast enrollment for a given grade
forecast_enrollment <- function(grade_column, grade_name) {
  # Prepare the time series data
  enrollment_ts <- ts(enrollment[[grade_column]], start=c(2002, 9), frequency=10)
  
  # Fit the model on the training_set
  fit <- auto.arima(enrollment_ts)
  
  # Forecasting
  forecasted_values <- forecast(fit, h=6)
  
  # Extracting forecasted values and time
  forecasted_df <- data.frame(
    Time = as.numeric(time(forecasted_values$mean)),
    Forecast = as.numeric(forecasted_values$mean),
    Grade = grade_name
  )
  
  # Find the row number closest to the specific time point 2024.7
  closest_time <- which.min(abs(forecasted_df$Time - 2024.7))
  
  # Select the row with the closest time and round the forecast
  forecasted_specific_time <- forecasted_df[closest_time, ]
  forecasted_specific_time$Forecast <- round(forecasted_specific_time$Forecast)
  
  # Remove the 'Time' column
  forecasted_specific_time <- forecasted_specific_time[, c("Forecast", "Grade")]
  
  return(forecasted_specific_time)
}

# List of grade columns and their names
grades <- list(
  PK = "Pre-kindergarten",
  K = "Kindergarten",
  Grade_01 = "Grade 1",
  Grade_02 = "Grade 2",
  Grade_03 = "Grade 3",
  Grade_04 = "Grade 4",
  Grade_05 = "Grade 5",
  Grade_06 = "Grade 6",
  Grade_07 = "Grade 7",
  Grade_08 = "Grade 8",
  Grade_09 = "Grade 9",
  Grade_10 = "Grade 10",
  Grade_11 = "Grade 11",
  Grade_12 = "Grade 12",
  Grade_13_GHES = "Grade 13 Golden Hill ES",
  Grade_13_SSSI = "Grade 13 SS Seward Inst",
  Grade_14 = "Grade 14"
)

# Initialize an empty data frame to store all forecasts
all_forecasts <- data.frame()

# Apply the forecasting function to each grade and combine results
for (grade in names(grades)) {
  grade_forecasts <- forecast_enrollment(grade, grades[[grade]])
  all_forecasts <- rbind(all_forecasts, grade_forecasts)
}

# Write the combined forecast data for the specific time to a CSV file
write.csv(all_forecasts, "enrollment_forecast_2024-25.csv", row.names = FALSE)
