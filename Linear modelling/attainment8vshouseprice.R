library(tidyverse)
library(ggplot2)
library(dplyr)
house_prices<-read.csv('assignments/Cleaned Data/cleanHousePrices.csv')
marks<-read.csv('assignments/Cleaned Data/cleanKS4Final.csv')
towns<-read.csv('assignments/Cleaned Data/Towns.csv')
colnames(marks)
# [1] "SchoolName"    "Postcode"      "shortPostcode" "County"        "District"      "Attainment8"   "Progress8"     "Year"  
colnames(house_prices)
# [1] "X"                 "Postcode"          "shortPostcode"     "Price"             "Year"              "PPD_Category_Type"


# Join and aggregate to prevent many-to-many issues
house_pricesXmarks <- house_prices %>%
  inner_join(marks, by = c("shortPostcode","Postcode", "Year")) %>% inner_join(towns,by="shortPostcode")
colnames(house_pricesXmarks)

#[1] "X"               "Postcode"        "shortPostcode.x" "Price"           "Year"            "SchoolName"      "shortPostcode.y"
# [8] "Attainment8"     "Progress8"       



ggplot(house_pricesXmarks, aes(x = Attainment8, y = Price, color = County)) +
  geom_point(alpha = 0.7) +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Attainment 8 Score vs House Price (Both Counties)",
    x = "Average Attainment 8 Score",
    y = "Average House Price (GBP)"
  ) +
  theme_minimal()
cor(house_pricesXmarks$Attainment8, house_pricesXmarks$Price, use = "complete.obs")
You sent
