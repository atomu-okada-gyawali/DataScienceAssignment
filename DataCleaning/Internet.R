# Load required libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(janitor)

# Read performance and coverage datasets
performance = read_csv("assignments/Obtained Data/Internet_Speed/performance.csv")
coverage = read_csv("assignments/Obtained Data/Internet_Speed/coverage.csv")

# Load previously cleaned towns with shortPostcode and County
towns <- read_csv("assignments/Cleaned Data/Towns.csv") %>%
  select(shortPostcode, County,District)

# Clean column names in performance dataset
internet_perf = performance %>%
  clean_names()

# Add shortPostcode from postcode
internet_perf = internet_perf %>%
  mutate(shortPostcode = str_sub(postcode, 1, 4))

# Select columns and remove rows with NA values
internet_perf_summary <- internet_perf %>%
  left_join(towns, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  mutate(postcode=str_c(str_sub(postcode, 1, -4), " ", str_sub(postcode, -3, -1))) %>% 
  select(
    postcode,
    shortPostcode,
    average_download_speed_mbit_s,
    average_upload_speed_mbit_s,
    median_download_speed_mbit_s,
    median_upload_speed_mbit_s,
    number_of_connections_30_300_mbit_s_number_of_lines,
    number_of_connections_300_mbit_s_number_of_lines
  ) 
View(internet_perf_summary)
# Save cleaned performance data
write.csv(internet_perf_summary, "assignments/Cleaned Data/cleanPerformance.csv", row.names = FALSE)

# Add shortPostcode and select coverage columns
CoverageData = coverage %>%
  mutate(shortPostcode = str_trim(substring(pcds, 1, 4))) %>%
  left_join(towns, by = "shortPostcode") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")) %>%
  select(
    shortPostcode,
    SFBB_availability = `SFBB availability (% premises)`,
    FTTP_availability = `FTTP availability (% premises)`,
    unable_2Mbit = `% of premises unable to receive 2Mbit/s`,
    unable_5Mbit = `% of premises unable to receive 5Mbit/s`,
    unable_10Mbit = `% of premises unable to receive 10Mbit/s`,
    unable_30Mbit = `% of premises unable to receive 30Mbit/s`,
    unable_USO = `% of premises unable meet USO`
  )

# Save cleaned coverage data
write.csv(CoverageData, "assignments/Cleaned Data/cleanCoverage.csv", row.names = FALSE)
