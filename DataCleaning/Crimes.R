library(tidyverse)
library(dplyr)
# Load previously cleaned towns with shortPostcode and County
towns <- read_csv("assignments/Cleaned Data/Towns.csv") %>%
  select(shortPostcode, County,District)

# Set the top-level directory where all the monthly folders are
base_dir <- 'assignments/Obtained Data/Crime_Dataset'

# Get list of all subfolders (months)
folders <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to hold dataframes
all_data <- list()

# Loop through each folder and read the CSV
for (folder in folders) {
  csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
  
  for (file in csv_files) {
    df <- read_csv(file, show_col_types = FALSE)
    all_data[[length(all_data) + 1]] <- df
  }
}

# Combine all rows into one big dataframe
Crimes <- bind_rows(all_data) %>%
  # Standardize LSOA code
  mutate(`LSOA code` = str_trim(toupper(`LSOA code`)))

# Load Postcode to LSOA map data
lsoa <- read_csv("assignments/Obtained Data/Postcode to LSOA.csv") %>%
  # Standardize postcode
  mutate(pcds = str_trim(toupper(pcds)))

# Check redundancy in lsoa
lsoa %>%
  count(lsoa11cd) %>%
  filter(n > 1)

# Remove redundancy in lsoa and derive shortPostcode
lsoa_clean <- lsoa %>%
  mutate(pcds = str_trim(toupper(pcds)),
         shortPostcode = str_trim(substring(pcds, 1, 4))) %>%
  select("LSOA code" = lsoa11cd, Postcode = pcds, shortPostcode) %>%
  distinct(`LSOA code`, .keep_all = TRUE)

# Check for missing Crime type
Crimes[!complete.cases(Crimes$`Crime type`), ]

# Group, join, and count crimes by Postcode, shortPostcode, and Crime type
cleanedCrimes <- Crimes %>%
  select(-`Reported by`, -Context) %>%
  # Join with lsoa_clean on LSOA code
  inner_join(lsoa_clean, by = "LSOA code") %>%
  left_join(towns, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  # Group by Postcode, shortPostcode, and Crime type, then count
  group_by(Postcode, shortPostcode, `Crime type`,Month) %>%
  summarise(n = n(), .groups = "drop")
cleanedCrimes
# Save cleaned dataset
write.csv(cleanedCrimes, "assignments/Cleaned Data/cleanCrimes.csv", row.names = FALSE)



