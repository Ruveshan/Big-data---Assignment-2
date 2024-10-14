# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Convert volumes to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(c("Bitcoin_Vol.", "Ethereum_Vol."), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate average trading volume for each period
volume_metrics <- data.frame(
  Asset = c("Bitcoin", "Ethereum"),
  During_COVID_AvgVolume = c(mean(during_covid_data$Bitcoin_Vol., na.rm = TRUE),
                             mean(during_covid_data$Ethereum_Vol., na.rm = TRUE)),
  Post_COVID_AvgVolume = c(mean(post_covid_data$Bitcoin_Vol., na.rm = TRUE),
                           mean(post_covid_data$Ethereum_Vol., na.rm = TRUE))
)

# Print volume metrics for verification
print("Average Trading Volume During and Post-COVID:")
print(volume_metrics)

# Reshape data for plotting
volume_metrics_long <- volume_metrics %>%
  pivot_longer(cols = c(During_COVID_AvgVolume, Post_COVID_AvgVolume), 
               names_to = "Period", values_to = "Volume") %>%
  mutate(Period = ifelse(Period == "During_COVID_AvgVolume", "During COVID", "Post COVID"))

# Plot average trading volumes during and post-COVID
ggplot(volume_metrics_long, aes(x = Asset, y = Volume, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Trading Volume for Bitcoin and Ethereum During and Post-COVID",
       x = "Asset", y = "Average Trading Volume") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot daily trading volume trends over time
volume_trends <- stock_data %>%
  select(Date, Bitcoin_Vol., Ethereum_Vol.) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Volume")

ggplot(volume_trends, aes(x = Date, y = Volume, color = Asset)) +
  geom_line() +
  labs(title = "Daily Trading Volume Trends for Bitcoin and Ethereum",
       x = "Date", y = "Trading Volume") +
  theme_minimal() +
  theme(legend.position = "bottom")
