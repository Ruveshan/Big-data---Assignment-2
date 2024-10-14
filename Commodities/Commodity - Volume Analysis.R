# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format if needed

# Convert volumes to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Vol."), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# List of commodity volume columns
commodities_volume <- c("Natural_Gas_Vol.", "Crude_oil_Vol.", "Gold_Vol.", 
                        "Silver_Vol.", "Platinum_Vol.", "Copper_Vol.")

# Initialize data frame to store volume analysis results
volume_metrics <- data.frame(Commodity = commodities_volume, 
                             During_COVID_AvgVolume = NA, Post_COVID_AvgVolume = NA,
                             Volume_Change_Percentage = NA)

# Calculate average volumes and changes in volume for each commodity
for (commodity in commodities_volume) {
  # During COVID average volume
  during_avg_volume <- mean(during_covid_data[[commodity]], na.rm = TRUE)
  
  # Post COVID average volume
  post_avg_volume <- mean(post_covid_data[[commodity]], na.rm = TRUE)
  
  # Percentage change in volume from during-COVID to post-COVID
  volume_change <- ((post_avg_volume - during_avg_volume) / during_avg_volume) * 100
  
  # Store results in the data frame
  volume_metrics[volume_metrics$Commodity == commodity, ] <- 
    c(commodity, during_avg_volume, post_avg_volume, volume_change)
}

# Print the volume metrics for verification
print("Commodity Volume Metrics:")
print(volume_metrics)

# Reshape data for plotting
volume_metrics_long <- volume_metrics %>%
  pivot_longer(cols = c(During_COVID_AvgVolume, Post_COVID_AvgVolume), 
               names_to = "Period", values_to = "Volume") %>%
  mutate(Period = ifelse(Period == "During_COVID_AvgVolume", "During COVID", "Post COVID"))

# Plot average trading volumes during and post-COVID
ggplot(volume_metrics_long, aes(x = Commodity, y = Volume, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Commodity Trading Volume During and Post-COVID",
       x = "Commodity", y = "Average Trading Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
