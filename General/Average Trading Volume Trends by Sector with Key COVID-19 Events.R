# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Identify all columns with "vol" in their names for volume data
volume_columns <- grep("_vol\\.", colnames(stock_data), value = TRUE)

# Convert identified volume columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(volume_columns), ~ as.numeric(gsub(",", "", .))))

# Define significant COVID-19 events with dates and descriptions
event_dates <- data.frame(
  Event = c("WHO declares COVID-19 a pandemic", "US Stimulus Bill", 
            "Pfizer Vaccine Announcement", "First US Lockdown", "First Vaccine Rollout"),
  Date = as.Date(c("2020-03-11", "2020-03-27", "2020-11-09", "2020-03-19", "2020-12-14"))
)

# Function to calculate sector-wise average volume
calculate_sector_volume <- function(stock_data, volume_assets) {
  sector_volume <- stock_data %>%
    select(Date, all_of(volume_assets)) %>%
    rowwise() %>%
    mutate(Average_Volume = mean(c_across(everything()), na.rm = TRUE)) %>%
    ungroup() %>%
    select(Date, Average_Volume)
  return(sector_volume)
}

# Define sectors based on volume column names
tech_stocks <- volume_columns[grepl("Apple|Microsoft|Nvidia|Google|Meta|Netflix|Amazon|Tesla", volume_columns)]
indices <- volume_columns[grepl("S.P_500|Nasdaq_100", volume_columns)]
commodities <- volume_columns[grepl("Natural_Gas|Crude_oil|Gold|Silver|Platinum|Copper", volume_columns)]
cryptocurrencies <- volume_columns[grepl("Bitcoin|Ethereum", volume_columns)]

# Calculate average volume for each sector
tech_volume <- calculate_sector_volume(stock_data, tech_stocks) %>% mutate(Sector = "Technology")
indices_volume <- calculate_sector_volume(stock_data, indices) %>% mutate(Sector = "Indices")
commodities_volume <- calculate_sector_volume(stock_data, commodities) %>% mutate(Sector = "Commodities")
crypto_volume <- calculate_sector_volume(stock_data, cryptocurrencies) %>% mutate(Sector = "Cryptocurrencies")

# Combine sector volume data
sector_volume_data <- bind_rows(tech_volume, indices_volume, commodities_volume, crypto_volume)

# Plot volume trends across sectors, highlighting key COVID-19 event dates
ggplot(sector_volume_data, aes(x = Date, y = Average_Volume, color = Sector)) +
  geom_line() +
  geom_vline(data = event_dates, aes(xintercept = as.numeric(Date)), color = "red", linetype = "dashed") +
  geom_text(data = event_dates, aes(x = Date, y = Inf, label = Event), angle = 90, vjust = -0.5, hjust = 1, size = 3, inherit.aes = FALSE) +
  labs(
    title = "Average Trading Volume Trends by Sector with Key COVID-19 Events",
    x = "Date", y = "Average Trading Volume",
    color = "Sector"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for better readability
  theme_minimal() +
  theme(legend.position = "bottom")
