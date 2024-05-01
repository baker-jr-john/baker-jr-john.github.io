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

# Rename columns
colnames(enrollment) <-
  c(
    "Year",
    "Prekindergarten",
    "Kindergarten",
    "First_grade",
    "Second_grade",
    "Third_grade",
    "Fourth_grade",
    "Fifth_grade",
    "Sixth_grade",
    "Seventh_grade",
    "Eighth_grade",
    "Elementary_ungraded",
    "Ninth_grade",
    "Tenth_grade",
    "Eleventh_grade",
    "Twelfth_grade",
    "Secondary_ungraded"
  )

# Calculate enrollment total
enrollment$enrollment_total <-
  rowSums(enrollment[,-1], na.rm = TRUE)

# Function to forecast enrollment for a specific grade level
forecast_grade_level <- function(grade_level) {
  # Prepare the time series data
  enrollment_ts <-
    ts(
      enrollment[[grade_level]],
      start = 1990,
      end = 2021,
      frequency = 1
    )
  
  # Specify and fit the AAN exponential smoothing model
  cat("Model: AAN -", grade_level, "\n")
  enrollment_model <- ets(enrollment_ts, model = "AAN")
  
  # Make predictions for 2022 to 2030
  enrollment_forecast <- forecast(enrollment_model, h = 9)
  
  # Return the forecast
  return(list(mean = enrollment_forecast$mean, forecast = enrollment_forecast))
}

# Forecast enrollment for each grade level and enrollment total
grade_levels <-
  c(
    "Prekindergarten",
    "Kindergarten",
    "First_grade",
    "Second_grade",
    "Third_grade",
    "Fourth_grade",
    "Fifth_grade",
    "Sixth_grade",
    "Seventh_grade",
    "Eighth_grade",
    "Elementary_ungraded",
    "Ninth_grade",
    "Tenth_grade",
    "Eleventh_grade",
    "Twelfth_grade",
    "Secondary_ungraded",
    "enrollment_total"
  )

forecasts <- lapply(grade_levels, forecast_grade_level)

# Create a data frame with the forecasts
forecast_table <- data.frame(cbind(
  Year = 2022:2030,
  do.call(cbind, lapply(forecasts, function(x) x$mean))
))

colnames(forecast_table)[-1] <- grade_levels

# Print the forecast table
print(forecast_table)

# Adjust plot margins
par(mar = c(2, 2, 2, 1))

# Plot the forecasts
par(mfrow = c(5, 4))
for (i in 1:length(forecasts)) {
  plot(
    forecasts[[i]]$forecast,
    main = paste("Enrollment Forecast (2022-2030) -", grade_levels[i])
  )
}