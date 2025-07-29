library(tidyverse)
install.packages('ggrepel')
library(ggrepel)  # for nicer non-overlapping labels
towns <- read_csv("assignments/Cleaned Data/Towns.csv") 
housePrices <- read_csv("assignments/Cleaned Data/cleanHousePrices.csv") 
colnames(housePrices)
prepared = housePrices %>% inner_join(towns, by = "shortPostcode") %>% select(-'Postcode',-'shortPostcode',-'Town/City',-PPD_Category_Type ) %>% 
  group_by(District,County,Year) %>% summarise(avg=mean(Price,na.rm=TRUE))

# Step 1: filter the data to only keep the last year per District (to position the label)
label_data <- prepared %>%
  group_by(District) %>%
  filter(Year == max(Year))  # last year per District

# Step 2: plot with line, points, and labels at the end
prepared %>%
  ggplot(aes(x = Year, y = avg, group = District, color = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  geom_text_repel(
    data = label_data,
    aes(label = District),
    nudge_x = 0.2,       # move labels a bit to the right
    direction = "y",
    hjust = 0,
    segment.color = NA   # no connecting line
  ) +
  labs(
    title = "Average House Prices by District (2021–2024)",
    x = "Year",
    y = "Average Price"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("SOUTH YORKSHIRE" = "blue", "WEST YORKSHIRE" = "red")) +
  xlim(min(prepared$Year), max(prepared$Year) + 1)  # extra space for labels

# Step 1: Filter data for 2023 and calculate average price per District
avg_2023 <- prepared %>%
  filter(Year == 2023)
#Step 2: Bar chart
ggplot(avg_2023, aes(x = reorder(District, -avg), y = avg, fill = County)) +
  geom_col() +
  labs(
    title = "Average House Prices by District in 2023",
    x = "District",
    y = "Average Price"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("SOUTH YORKSHIRE" = "blue", "WEST YORKSHIRE" = "red")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ County, scales = "free_x")

nprepared %>% ggplot( aes(x = reorder(District, -avg), y = avg)) +
  geom_boxplot() +
  facet_wrap(~ County, scales = "free_x") +  # Separate plots for each County
  labs(
    title = "Boxplot of House Prices by District",
    x = "District",
    y = "Average Price"
  ) 


