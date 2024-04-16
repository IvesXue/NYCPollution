# load package
library(tidyverse)
library(lubridate)

# Step 1: Load the dataset
data <- read.csv("deathrate-by-modzcta.csv")

# Step 2: Convert the data from wide to long format
data_long <- pivot_longer(data, cols = -date, names_to = "Location", values_to = "DeathRate")

# Handle various date formats by attempting to parse the date automatically
data_long <- data_long %>%
  mutate(date = parse_date_time(date, orders = c("mdy", "ymd", "dmy")), # Adjust orders based on potential date formats
         Year = year(date),
         Month = month(date),
         date = NULL) # Remove the original date column after extraction

# Generalize ZIP code or area name extraction based on a common prefix (if applicable)
# This can be adjusted or made more sophisticated based on the actual column names
data_long <- data_long %>%
  mutate(ZipCode = str_replace(Location, "DEATHRATE_", "")) %>%
  select(-Location) # Remove the 'Location' column

# Step 3: Reorder columns to match the desired output
data_long <- select(data_long, Year, Month, DeathRate, ZipCode)

# Export the transformed data to a new CSV file
write.csv(data_long, "transformed_dataset_deathrate.csv", row.names = FALSE)



