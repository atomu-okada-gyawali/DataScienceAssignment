library(tidyverse)
library(scales)
house_prices<-read.csv('assignments/Cleaned Data/cleanHousePrices.csv')
internet<-read.csv('assignments/Cleaned Data/cleanPerformance.csv')

towns<-read.csv('assignments/Cleaned Data/Towns.csv')

colnames(house_prices)
# Prepareinternet_perf_summary# Prepare aggregated internet data
internet_perf_summary <- internet %>%
  select(
    shortPostcode,
    average_download_speed_mbit_s
  ) %>% group_by(shortPostcode) %>% summarise(avg_download_speed = mean(average_download_speed_mbit_s))
house_prices_summary<- house_prices %>% 
  group_by(shortPostcode,Year) %>% summarise(average_price=mean(Price))
# Join datasets
house_pricesXinternet <- house_prices_summary %>%
  select(average_price,shortPostcode)%>%
  inner_join(towns %>% select(shortPostcode, County), by = "shortPostcode") %>%
  inner_join(internet_perf_summary, by = "shortPostcode")

library(ggplot2)

ggplot(house_pricesXinternet, aes(x = avg_download_speed, y = average_price, color = County)) +
  scale_y_log10(labels = label_number(scipen = 999)) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "House Price vs Average Download Speed by County",
    x = "Average Download Speed (Mbps)",
    y = "House Price (GBP)"
  ) +
  theme_minimal()

correlation <- cor(house_pricesXinternet$Price, house_pricesXinternet$average_download_speed_mbit_s, use = "complete.obs")
print(paste("Correlation:", round(correlation, 3)))

