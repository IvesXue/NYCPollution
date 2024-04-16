# Load necessary libraries
library(tidyverse)
library(lubridate)

# Function to set the working directory
set_working_directory <- function() {
  setwd("C:/Users/YourName/Desktop/NYC air Pollution")
}

# Function to process a single file
process_file <- function(filename) {
  data <- read.csv(filename)
  
  # Transform the data as per the previous steps
  data_long <- data %>%
    pivot_longer(cols = -date, names_to = "Location", values_to = "Value") %>%
    mutate(date = parse_date_time(date, orders = c("mdy", "ymd", "dmy")),
           Year = year(date),
           Month = month(date),
           ZipCode = str_extract(Location, "\\d+"),
           Statistic = str_replace(Location, paste0("_", ZipCode), ""),
           date = NULL) %>%
    select(Year, Month, ZipCode, Statistic, Value)
  
  return(data_long)
}

# Main script to set directory and process all files
main <- function() {
  set_working_directory()
  
  # Assuming the 'trend' folder is in the working directory
  files <- list.files(path = "trend", pattern = ".*-by-modzcta.csv$", full.names = TRUE)
  
  # Process each file and store the results in a list
  list_data <- lapply(files, process_file)
  
  # Combine all data frames in the list into one
  combined_data <- bind_rows(list_data)
  
  # Reshape the combined data so each statistic ('deathrate', 'hosprate', 'postiverate') becomes a column
  final_data <- combined_data %>%
    pivot_wider(names_from = Statistic, values_from = Value)
  
  # Export the final combined dataset to a new CSV file
  write.csv(final_data, "combined_transformed_dataset.csv", row.names = FALSE)
}

# Run the main function to process all files
main()
