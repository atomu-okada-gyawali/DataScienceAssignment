
crimes= read_csv("assignments/Cleaned Data/cleanCrimes.csv")
crimes%>%
  # Join with towns on full Postcode
  inner_join(towns %>% select(Postcode, County, District, Population2022, Population2023, Population2024), 
             by = c("Postcode" = "Postcode")) %>%
  # Group and count crimes, including shortPostcode
  group_by(Postcode, shortPostcode, `Crime type`, District, County, Month, Population2022, Population2023, Population2024) %>%
  count(name = "n") %>%
  ungroup() %>%
  # Calculate crime rate per 10,000 people
  mutate(Crime_rate = case_when(
    as.numeric(substr(Month, 1, 4)) == 2022 ~ (n / Population2022) * 10000,
    as.numeric(substr(Month, 1, 4)) == 2023 ~ (n / Population2023) * 10000,
    as.numeric(substr(Month, 1, 4)) == 2024 ~ (n / Population2024) * 10000
  )) %>%
  # Remove count and population columns
  select(-n, -Population2022, -Population2023, -Population2024)
# library(ggplot2)
library(tidyverse)
library(ggplot2)
# Filter for Drug offenses only
drug_data <- crimes %>%
  filter(`Crime type` == "Drugs") 

# Plot: Separate boxplots for each county
ggplot(drug_data, aes(x = District, y = Crime_rate)) +
  geom_boxplot() +
  facet_wrap(~County, scales = "free_x") +
  labs(
    title = "Drug Offense Rates by District",
    x = "District",
    y = "Drug Offense Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(fmsb)

# Step 1: Prepare data
radar_data <- crimes %>%
  filter(`Crime type` == "Vehicle crime",
         County == "WEST YORKSHIRE",
         Month == "2022-06") %>%
  group_by(District) %>%
  summarise(rate = round(sum(Crime_rate),2), .groups = "drop",na.rm=TRUE)

# Step 2: Transpose properly
# Convert to a single row named vector with Districts as columns
rates <- as.numeric(radar_data$rate)
names(rates) <- radar_data$District

# Step 3: Make radar matrix with max, min, actual
radar_matrix <- rbind(
  rep(max(rates), length(rates)),  # max values
  rep(0, length(rates)),           # min values
  rates                            # actual data
)

# Step 4: Set column names
colnames(radar_matrix) <- radar_data$District

print(rates)
summary(rates)
# Step 5: Plot
radarchart(as.data.frame(radar_matrix),
           axistype = 1,
           pcol = "darkgreen", pfcol = rgb(0.1, 0.6, 0.1, 0.3), plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "black",
           caxislabels = seq(0, max(rates), length.out = 5),
           vlcex = 0.8)
title("Radar Chart: Vehicle Crime Rates\nWest Yorkshire (June 2022)")

colSums(is.na(crimes))
# Filter for Robbery in one county/month
robbery_data <- crimes %>%
  filter(`Crime type` == 'Robbery',
         County == "SOUTH YORKSHIRE",
         Month == "2024-02") %>%
  group_by(District) %>%
  summarise(rate = sum(Crime_rate,na.rm=TRUE))
crimes %>%   filter(`Crime type` == 'Robbery',
                    County == "SOUTH YORKSHIRE",
                    Month == "2024-01",
                    District== "SHEFFIELD")

# Pie chart
ggplot(robbery_data, aes(x = "", y = rate, fill = District)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(title = "Robbery Rate Distribution (South Yorkshire, June 2023)") +
  theme_void()



library(tidyverse)
library(lubridate)
install.packages('lubridate')
# Step 1: Filter for drug offenses only
drug_data <- crimes %>%
  filter(`Crime type` == "Drugs") %>%
  mutate(Year = year(ymd(paste0(Month, "-01"))))  # Extract year

# Step 2: Summarize average monthly drug crime rate by County and Year-Month
drug_trend <- drug_data %>%
  group_by(County, Month) %>%
  summarise(avg_crime_rate = mean(Crime_rate, na.rm = TRUE), .groups = "drop")

# Step 3: Plot the line chart
ggplot(drug_trend, aes(x = as.Date(paste0(Month, "-01")), y = avg_crime_rate, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point() +
  labs(
    title = "Drug Offense Rate per 10,000 People (Monthly)",
    subtitle = "Comparison between South and West Yorkshire",
    x = "Month",
    y = "Drug Offense Rate per 10,000",
    color = "County"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
