# Load packages
library(readxl)
library(forecast)
library(ggplot2)

# Load data
enrollment <- read_excel(
  "data/tabn203.10.xlsx",
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

# Calculate prekindergarten through eighth-grade enrollment
enrollment$pk_8_enrollment <-
  rowSums(enrollment[, c(
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
    "Elementary_ungraded"
  )], na.rm = TRUE)

# Calculate grades nine through twelve enrollment
enrollment$grades_9_12_enrollment <-
  rowSums(enrollment[, c(
    "Ninth_grade",
    "Tenth_grade",
    "Eleventh_grade",
    "Twelfth_grade",
    "Secondary_ungraded"
  )], na.rm = TRUE)

# Calculate total enrollment
enrollment$enrollment_total <-
  rowSums(enrollment[, c("pk_8_enrollment", "grades_9_12_enrollment")], na.rm = TRUE)

# Function to forecast enrollment for a specific grade level or enrollment category
forecast_enrollment <- function(enrollment_category) {
  # Prepare the time series data
  enrollment_ts <-
    ts(enrollment[[enrollment_category]],
       start = 1990,
       end = 2021,
       frequency = 1)
  
  # Specify and fit the AAN exponential smoothing model
  enrollment_model <- ets(enrollment_ts, model = "AAN")
  
  # Make predictions for 2022 to 2030
  enrollment_forecast <- forecast(enrollment_model, h = 9)
  
  # Return the forecast
  return(list(mean = enrollment_forecast$mean, forecast = enrollment_forecast))
}

# Forecast enrollment for each grade level
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
    "Secondary_ungraded"
  )

forecasts_grade_levels <- lapply(grade_levels, forecast_enrollment)

# Forecast enrollment for enrollment categories
enrollment_categories <-
  c("pk_8_enrollment",
    "grades_9_12_enrollment",
    "enrollment_total")

forecasts_enrollment_categories <-
  lapply(enrollment_categories, forecast_enrollment)

# Create a data frame with historical and forecasted data for enrollment categories
enrollment_data <- data.frame(
  Year = rep(2010:2030, 3),
  Enrollment = c(
    enrollment$pk_8_enrollment[enrollment$Year >= 2010],
    forecasts_enrollment_categories[[1]]$mean,
    enrollment$grades_9_12_enrollment[enrollment$Year >= 2010],
    forecasts_enrollment_categories[[2]]$mean,
    enrollment$enrollment_total[enrollment$Year >= 2010],
    forecasts_enrollment_categories[[3]]$mean
  ) / 1000,
  Category = rep(c("PK-8", "Grades 9-12", "Total"), each = 21)
)

# Create the line plot
enrollment_plot <-
  ggplot(enrollment_data, aes(x = Year, y = Enrollment, color = Category)) +
  geom_line() +
  geom_vline(xintercept = 2021.5,
             linetype = "solid",
             color = "gray50") +
  annotate(
    "text",
    x = 2016,
    y = max(enrollment_data$Enrollment) * 0.9,
    label = "Actual",
    size = 4
  ) +
  annotate(
    "text",
    x = 2026,
    y = max(enrollment_data$Enrollment) * 0.9,
    label = "Projected",
    size = 4
  ) +
  scale_color_manual(values = c(
    "PK-8" = "#E69F00",
    "Grades 9-12" = "#56B4E9",
    "Total" = "#009E73"
  )) +
  scale_y_continuous(
    labels = function(x)
      format(x, scientific = FALSE)
  ) +
  labs(title = "Enrollment in public elementary and secondary schools, by level",
       x = "Year",
       y = "Enrollment, in millions") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )

# Print the plot
print(enrollment_plot)

# Create a data frame with the forecasts for grade levels
forecast_table_grade_levels <-
  data.frame(cbind(Year = 2022:2030, do.call(
    cbind, lapply(forecasts_grade_levels, function(x)
      x$mean)
  )))

colnames(forecast_table_grade_levels)[-1] <- grade_levels

# Print the forecast table for grade levels
print(forecast_table_grade_levels)

# Adjust plot margins for grade level plots
par(mar = c(2, 2, 2, 1))

# Plot the forecasts for grade levels
par(mfrow = c(4, 4))
for (i in 1:length(forecasts_grade_levels)) {
  plot(
    forecasts_grade_levels[[i]]$forecast,
    main = paste("Enrollment Forecast (2022-2030) -", grade_levels[i])
  )
}