
library(tidyverse)
marks<-read.csv('assignments/Cleaned Data/cleanKS4Final.csv')
crimes<-read.csv('assignments/Cleaned Data/cleanCrimes.csv')
crimes <- crimes %>%
  mutate(Year = as.integer(substr(Month, 1, 4)))

crimesXmarks <- crimes %>%
  inner_join(marks, by = c("shortPostcode", "Year")) %>% select(District=District.x,County=County.x,Year,meanAttainment8,Crime_rate,`Crime.type`)

crimesXmarks
drug_2023 <- crimesXmarks %>%
  filter(Year == 2023,`Crime.type`  == "Drugs") %>%
  select(District, County, Crime_rate, meanAttainment8)
drug_2023
library(ggplot2)

ggplot(drug_2023, aes(x = meanAttainment8, y = Crime_rate, color = County)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Attainment 8 Score vs Drug Offense Rate (2023)",
    x = "Average Attainment 8 Score",
    y = "Drug Offense Rate per 10,000 People"
  ) +
  theme_minimal()

lm_model <- lm(Crime_rate ~ meanAttainment8, data = drug_2023)
summary(lm_model)
cor(drug_2023$meanAttainment8, drug_2023$Crime_rate, use = "complete.obs")


any(is.na(crimes))