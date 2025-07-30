library(tidyverse)

# Load data
crimes <- read.csv('assignments/Cleaned Data/cleanCrimes.csv')
internet <- read.csv('assignments/Cleaned Data/cleanPerformance.csv')
towns <- read.csv('assignments/Cleaned Data/Towns.csv')

# Extract year from month column
crimes <- crimes %>%
  mutate(Year = as.integer(substr(Month, 1, 4)))

# Step 1: Filter drug crimes in 2023
drug_2023 <- crimes %>%
  filter(Crime.type == "Drugs", Year == 2023) %>%
  group_by(shortPostcode) %>%
  summarise(drug_count = sum(n), .groups = "drop")

# Step 2: Join with towns to get population
drug_2023_pop <- drug_2023 %>%
  left_join(towns %>% select(shortPostcode, District, County, Population2023), by = "shortPostcode") %>%
  mutate(drugs_per_10k = (drug_count / Population2023) * 10000)

# Step 3: Get average download speed
internet_avg <- internet %>%
  group_by(shortPostcode) %>%
  summarise(avg_download = mean(average_download_speed_mbit_s, na.rm = TRUE), .groups = "drop")

# Step 4: Combine all
final_df <- drug_2023_pop %>%
  inner_join(internet_avg, by = "shortPostcode") %>%
  drop_na(avg_download, drugs_per_10k, County)

# Step 5: Plot
ggplot(final_df, aes(x = avg_download, y = drugs_per_10k, color = County)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(group = County), linewidth = 1.2) +
  labs(
    title = "Average Download Speed vs Drug Offense Rate per 10,000 People (2023)",
    x = "Average Download Speed (Mbps)",
    y = "Drug Offenses per 10,000 People"
  ) +
  theme_minimal()

# Step 6: Linear model
lm_model <- lm(drugs_per_10k ~ avg_download, data = final_df)
summary(lm_model)

# Step 7: Correlation
cor_value <- cor(final_df$avg_download, final_df$drugs_per_10k, use = "complete.obs")
cat("Correlation between average download speed and drug offenses per 10k:", round(cor_value, 3), "\n")
