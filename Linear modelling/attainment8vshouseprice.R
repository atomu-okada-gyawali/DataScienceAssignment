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

house_prices_avg = house_prices %>% group_by(shortPostcode,Year) %>% summarise(avg_price=mean(Price))
marks_avg = marks %>% group_by(shortPostcode,Year) %>% summarise(avg_attainment=mean(Attainment8))
# Join and aggregate to prevent many-to-many issues
house_pricesXmarks <- house_prices_avg %>%
  inner_join(marks_avg, by = c("shortPostcode", "Year")) %>% inner_join(towns,by="shortPostcode")
colnames(house_pricesXmarks)
# [1] "shortPostcode"  "Year"           "avg_price"      "avg_attainment" "X"              "Town.City"
# [7] "District"       "County"         "Population2020" "Population2021" "Population2022" "Population2023"
# [13] "Population2024"

remove_outliers_iqr <- function(data, column) {
  # Ensure the column exists
  if (!(column %in% colnames(data))) stop("Column not found")

  # Pull the values
  vals <- data[[column]]
  Q1 <- quantile(vals, 0.25, na.rm = TRUE)
  Q3 <- quantile(vals, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val

  # Filter the data using the logical vector directly
  data[vals >= lower & vals <= upper, ]
}

house_pricesXmarks<-house_pricesXmarks %>% remove_outliers_iqr("avg_attainment")
house_pricesXmarks<-house_pricesXmarks %>% remove_outliers_iqr("avg_price")

ggplot(house_pricesXmarks, aes(x = avg_attainment, y = avg_price, color = County)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Attainment 8 Score vs House Price (Both Counties)",
    x = "Average Attainment 8 Score",
    y = "Average House Price (GBP)"
  ) +
  theme_minimal()

cor(house_pricesXmarks$avg_attainment, house_pricesXmarks$avg_price, use = "complete.obs")

# Fit a linear model
lm_model <- lm(avg_price ~ avg_attainment, data = house_pricesXmarks)

# Display model summary
summary(lm_model)

