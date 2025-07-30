library(tidyverse)
marks<-read.csv('assignments/Cleaned Data/cleanKS4Final.csv')
crimes<-read.csv('assignments/Cleaned Data/cleanCrimes.csv')
towns<-read.csv('assignments/Cleaned Data/Towns.csv')
crimes <- crimes %>%
  mutate(Year = as.integer(substr(Month, 1, 4)))
colnames(marks)
# [1] "SchoolName"    "Postcode"      "shortPostcode" "Attainment8"   "Progress8"     "Year"      
colnames(crimes)
# [1] "Postcode"      "shortPostcode" "Crime.type"    "n"    "Month"
colnames(towns)
# [1] "X"              "shortPostcode"  "Town.City"      "District"       "County"         "Population2020" "Population2021" "Population2022"
# [9] "Population2023" "Population2024"
crimes <- crimes %>%
  mutate(Year = as.integer(substr(Month, 1, 4)))


# Step 1: Filter drug crimes in 2023
drug_2023 <- crimes %>%
  filter(Crime.type == "Drugs", substr(Month, 1, 4) == "2023") %>%
  group_by(shortPostcode) %>%
  summarise(drug_count = sum(n), .groups = "drop")
drug_2023
# Step 2: Join with towns to get population
drug_2023_pop <- drug_2023 %>%
  left_join(towns %>% select(shortPostcode, District, County, Population2023), by = "shortPostcode") %>%
  mutate(drugs_per_10k = (drug_count / Population2023) * 10000)

# Step 3: Get mean Attainment8 score for 2023
attainment_2023 <- marks %>%
  filter(Year == 2023) %>%
  group_by(shortPostcode) %>%
  summarise(Attainment8 = mean(Attainment8, na.rm = TRUE), .groups = "drop")

# Step 4: Combine everything
final_df <- drug_2023_pop %>%
  inner_join(attainment_2023, by = "shortPostcode")

# Step 5: Plot
ggplot(final_df, aes(x = Attainment8, y = drugs_per_10k, color = County)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(group = County), linewidth = 1.1) +
  labs(
    title = "Attainment 8 vs Drug Offenses per 10,000 People (2023)",
    x = "Average Attainment 8 Score",
    y = "Drug Offenses per 10,000 People"
  ) +
  theme_minimal()
# Step 6: Linear model summary
model <- lm(drugs_per_10k ~ Attainment8, data = final_df)
summary(model)

# Step 7: Correlation
cor_value <- cor(final_df$Attainment8, final_df$drugs_per_10k, use = "complete.obs")
cat("Correlation:", round(cor_value, 3), "\n")


