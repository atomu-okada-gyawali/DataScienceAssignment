towns <- read_csv("assignments/Cleaned Data/Towns.csv") 
internet<-read_csv("assignments/Cleaned Data/cleanPerformance.csv")
colnames(internet)
prepared_internet = internet %>% group_by(shortPostcode) %>% summarise(avg_download=mean(average_download_speed_mbit_s)) %>% 
  inner_join(towns, by = "shortPostcode") %>% 
  select(District, County, avg_download) 


# Boxplot with facet_wrap by County
ggplot( prepared_internet,aes(x = District, y = avg_download)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  facet_wrap(~ County, scales = "free_x") +  # separate chart for each county
  labs(
    title = "Boxplots of Average Download Speed by District for Each County",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Bar chart for South Yorkshire
prepared_internet %>%
  filter(County == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = avg_download, fill = District)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Download Speed by District in South Yorkshire",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar chart for West Yorkshire
prepared_internet %>%
  filter(County == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = avg_download, fill = District)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Download Speed by District in West Yorkshire",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

