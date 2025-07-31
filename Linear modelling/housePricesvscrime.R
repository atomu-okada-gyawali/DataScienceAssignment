# Load required packages
library(tidyverse)
library(ggplot2)

# Load datasets
crimes <- read.csv('assignments/Cleaned Data/cleanCrimes.csv')
house_prices <- read.csv('assignments/Cleaned Data/cleanHousePrices.csv')
towns <- read.csv('assignments/Cleaned Data/Towns.csv')

# Extract year from Month column in crimes
crimes <- crimes %>%
  mutate(Year = as.integer(substr(Month, 1, 4)))

# Step 1: Filter for drug crimes in 2023 and group by shortPostcode
drug_2023 <- crimes %>%
  filter(Crime.type == "Drugs", Year == 2023) %>%
  group_by(shortPostcode) %>%
  summarise(drug_count = sum(n), .groups = "drop")

# Step 2: Join drug crimes with town data to get population and calculate drugs_per_10k
drug_2023_pop <- drug_2023 %>%
  left_join(towns %>% select(shortPostcode, District, County, Population2023), by = "shortPostcode") %>%
  group_by(shortPostcode) %>%
  summarise(
    drug_count = sum(drug_count),
    Population2023 = mean(Population2023, na.rm = TRUE),
    District = first(District),
    County = first(County),
    drugs_per_10k = (sum(drug_count) / mean(Population2023, na.rm = TRUE)) * 10000,
    .groups = "drop"
  )

# Step 3: Calculate average house price per postcode for 2023
house_prices_summary <- house_prices %>%
  filter(Year == 2023) %>%
  group_by(shortPostcode) %>%
  summarise(average_price = mean(Price), .groups = "drop")

# Step 4: Join house prices with drug crime rates
house_pricesXdrugs <- house_prices_summary %>%
  inner_join(drug_2023_pop, by = "shortPostcode")

# --- Step 4.5: Remove outliers using IQR ---
remove_outliers_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  data %>% filter(data[[column]] >= lower & data[[column]] <= upper)
}

# Apply outlier removal
house_pricesXdrugs_clean <- house_pricesXdrugs %>%
  remove_outliers_iqr("average_price") %>%
  remove_outliers_iqr("drugs_per_10k")

# Step 5: Plot the data
ggplot(house_pricesXdrugs_clean, aes(x = drugs_per_10k, y = average_price, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "House Price vs Drug Crime Rate (per 10,000) in 2023",
    x = "Drug Crime Rate per 10,000 People",
    y = "Average House Price (GBP)"
  ) +
  theme_minimal()

# Step 6: Linear model summary per county
model_summary <- house_pricesXdrugs_clean %>%
  group_by(County) %>%
  summarise(
    model = list(lm(average_price ~ drugs_per_10k)),
    .groups = "drop"
  )

# Print each linear model summary
for (i in 1:nrow(model_summary)) {
  cat("\n--- Linear Model Summary for", model_summary$County[i], "---\n")
  print(summary(model_summary$model[[i]]))
}

# Step 7: Correlation between house price and drug rate by county
correlation_by_county <- house_pricesXdrugs_clean %>%
  group_by(County) %>%
  summarise(correlation = cor(average_price, drugs_per_10k), .groups = "drop")

cat("\n--- Correlation between Drug Crime Rate and House Price (by County) ---\n")
print(correlation_by_county)
