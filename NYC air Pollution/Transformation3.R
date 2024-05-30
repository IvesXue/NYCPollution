library(tidyverse)
library(lubridate)

# Step 1: Set working directory
setwd("C:/Users/Ives Xue/Desktop/NYCPollution/NYC air Pollution/trends")

# Step 2: Identify and Load Data
file_paths <- list.files(pattern = ".*-by-modzcta.csv$", full.names = TRUE)
datasets <- lapply(file_paths, read.csv)

# Helper function to remove borough-level summaries
remove_borough_summaries <- function(location) {
  !location %in% c("CITY", "BX", "BK", "MN", "QN", "SI", "Bronx", "Brooklyn", "Manhattan", "Queens", "Staten_Island", "Citywide")
}

# Separate and transform data based on the time measurement
weekly_data <- list()
monthly_data <- list()

for (i in seq_along(datasets)) {
  data <- datasets[[i]]
  path <- file_paths[[i]]
  
  base_rate_name <- tools::file_path_sans_ext(basename(path))
  base_rate_name <- tolower(gsub("-by-modzcta", "", base_rate_name))
  
  if ("week_ending" %in% names(data)) {
    # Handle weekly data
    data <- data %>%
      pivot_longer(cols = -week_ending, names_to = "Location", values_to = base_rate_name) %>%
      mutate(Location = str_replace(Location, "^.*?_", "")) %>%  # Remove any prefix followed by underscore
      filter(remove_borough_summaries(Location)) %>%  # Remove borough level summaries
      rename(Zipcode = Location) %>%
      select(week_ending, Zipcode, base_rate_name)
    
    weekly_data[[length(weekly_data) + 1]] <- data
  } else if ("date" %in% names(data)) {
    # Handle monthly data
    data <- data %>%
      pivot_longer(cols = -date, names_to = "Location", values_to = base_rate_name) %>%
      mutate(Location = str_replace(Location, "^.*?_", "")) %>%  # Remove any prefix followed by underscore
      filter(remove_borough_summaries(Location)) %>%  # Remove borough level summaries
      rename(Zipcode = Location) %>%
      select(date, Zipcode, base_rate_name)
    
    monthly_data[[length(monthly_data) + 1]] <- data
  }
}

# Combine datasets
final_weekly_dataset <- reduce(weekly_data, full_join, by = c("week_ending", "Zipcode"))
final_monthly_dataset <- reduce(monthly_data, full_join, by = c("date", "Zipcode"))

# Output the final combined files
write.csv(final_weekly_dataset, "combined_weekly_dataset.csv", row.names = FALSE)
write.csv(final_monthly_dataset, "combined_monthly_dataset.csv", row.names = FALSE)
