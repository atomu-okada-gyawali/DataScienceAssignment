

library(tidyverse)
library(ggplot2)
library(dplyr)
towns<-read.csv("assignments/Cleaned Data/Towns.csv")
avg_ks4final<-read_csv("assignments/Cleaned Data/cleanKS4Final.csv") %>%   inner_join(towns,by="shortPostcode") %>% group_by(shortPostcode,District,County,Year) %>% summarise(meanAttainment8=mean(Attainment8))

# Assuming your data has columns: County, District, Attainment8, Year
data_2022 <- avg_ks4final %>%
  filter(Year == "2022") %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Boxplot for South Yorkshire
ggplot(data_2022 %>% filter(County == "SOUTH YORKSHIRE"), 
       aes(x = District, y = meanAttainment8)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Average Attainment 8 Score 2022 – South Yorkshire",
       x = "District",
       y = "Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for West Yorkshire
ggplot(data_2022 %>% filter(County == "WEST YORKSHIRE"), 
       aes(x = District, y = meanAttainment8)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Average Attainment 8 Score 2021 – West Yorkshire",
       x = "District",
       y = "Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Line graph showing trend over years by District & County
ggplot(avg_ks4final %>% filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")),
       aes(x = Year, y = meanAttainment8, color = District, group = District)) +
  stat_summary(fun = mean, geom = "line", size = 1) +  # mean line per district
  stat_summary(fun = mean, geom = "point") +
  facet_wrap(~County) +
  labs(title = "Attainment 8 Score Trends by District and County",
       x = "Year",
       y = "Average Attainment 8 Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
