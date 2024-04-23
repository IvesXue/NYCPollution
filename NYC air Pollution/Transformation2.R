library(tidyverse)
library(lubridate)

# Step 1: Set working directory
setwd("C:/Users/Ives Xue/Desktop/NYCPollution/NYC air Pollution/trends")

# Step 2: Identify and Load Data
file_paths <- list.files(pattern = ".*-by-modzcta.csv$", full.names = TRUE)
datasets <- lapply(file_paths, read.csv)

# Function to convert weekly data to monthly
average_to_monthly <- function(df, date_col) {
  df %>%
    mutate(date = parse_date_time(!!sym(date_col), orders = c("mdy", "ymd", "dmy")),
           Year = year(date),
           Month = month(date)) %>%
    group_by(Year, Month) %>%
    summarise(across(setdiff(names(df), c("Year", "Month", date_col)), 
                     ~mean(.x, na.rm = TRUE)), .groups = "drop")
}

# Normalize and transform data
processed_data <- map2(datasets, file_paths, function(data, path) {
  # Identify date column and average weekly data to monthly if necessary
  date_col <- ifelse("week_ending" %in% names(data), "week_ending", "date")
  data <- average_to_monthly(data, date_col)
  
  # Construct base name for rate column from file name
  base_rate_name <- tools::file_path_sans_ext(basename(path))
  base_rate_name <- tolower(gsub("-by-modzcta", "", base_rate_name))
  
  # Pivot data and adjust Area name values
  data_long <- pivot_longer(data, cols = -c(Year, Month), names_to = "Location", values_to = base_rate_name) %>%
    mutate(Location = str_replace(Location, "^.*?_", ""), # Remove any prefix followed by underscore
           Area = standardize_area_names(Location)) %>%
    select(-Location) %>%
    relocate(Area, .after = Month) # Ensure Area is after Month
  
  data_long
})

# Function to standardize area names
standardize_area_names <- function(area_name) {
  case_when(
    str_detect(area_name, "BX|Bronx") ~ "Bronx",
    str_detect(area_name, "BK|Brooklyn") ~ "Brooklyn",
    str_detect(area_name, "MN|Manhattan") ~ "Manhattan",
    str_detect(area_name, "QN|Queens") ~ "Queens",
    str_detect(area_name, "SI|Staten_Island") ~ "Staten Island",
    str_detect(area_name, "CITY|Citywide") ~ "Citywide",
    TRUE ~ area_name  # Default case if no match is found
  )
}

# Step 4: Combine all datasets and adjust column order
final_dataset <- reduce(processed_data, full_join, by = c("Year", "Month", "Area")) %>%
  # Reorder columns to place all rate columns after the Area
  select(Year, Month, Area, sort(setdiff(names(.), c("Year", "Month", "Area"))))

# Output the final combined file
write.csv(final_dataset, "combined_dataset.csv", row.names = FALSE)
