library(tidyverse)
library(ggplot2)
# Load datasets
marks <- read.csv('assignments/Cleaned Data/cleanKS4Final.csv')
internet <- read.csv('assignments/Cleaned Data/cleanPerformance.csv')
towns <- read.csv('assignments/Cleaned Data/Towns.csv')

# Step 1: Get average download speed by shortPostcode
internet_avg <- internet %>%
  group_by(shortPostcode) %>%
  summarise(avg_download = mean(average_download_speed_mbit_s, na.rm = TRUE), .groups = "drop")

# Step 2: Join with marks and get County info from towns
combined <- marks %>%
  left_join(internet_avg, by = "shortPostcode") %>%
  left_join(towns %>% select(shortPostcode, County), by = "shortPostcode") %>%
  drop_na(avg_download, Attainment8, County)

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

combined %>% remove_outliers_iqr(avg_download) %>% remove_outliers_iqr((Attainment8))
# Step 3: Plot
ggplot(combined, aes(x = avg_download, y = Attainment8, color = County)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(method = "lm", aes(group = County), se = TRUE, linewidth = 1.2) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score (by County)",
    x = "Average Download Speed (Mbps)",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

# Step 4: Linear Model (overall)
lm_model <- lm(Attainment8 ~ avg_download, data = combined)
summary(lm_model)

# Step 5: Correlation
cor_val <- cor(combined$avg_download, combined$Attainment8, use = "complete.obs")
cat("Correlation between average download speed and Attainment 8:", round(cor_val, 3), "\n")

