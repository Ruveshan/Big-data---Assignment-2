# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)  # For moving averages

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the list of assets by sector
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")
indices <- c("S.P_500_Price", "Nasdaq_100_Price")
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")
cryptocurrencies <- c("Bitcoin_Price", "Ethereum_Price")

# Combine all asset columns for analysis
all_assets <- c(tech_stocks, indices, commodities, cryptocurrencies)

# Ensure all price columns are numeric, removing commas if present
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Function to calculate 50-day and 200-day moving averages for an asset
calculate_moving_averages <- function(data, asset_column) {
  data %>%
    mutate(
      `50_MA` = rollapply(get(asset_column), width = 50, FUN = mean, fill = NA, align = "right"),
      `200_MA` = rollapply(get(asset_column), width = 200, FUN = mean, fill = NA, align = "right")
    ) %>%
    select(Date, `50_MA`, `200_MA`)
}

# Calculate moving averages for each sector and aggregate at the sector level
calculate_sector_moving_averages <- function(stock_data, sector_assets, sector_name) {
  sector_ma_data <- lapply(sector_assets, function(asset) {
    data <- stock_data %>% select(Date, all_of(asset))
    ma_data <- calculate_moving_averages(data, asset)
    ma_data <- ma_data %>% mutate(Asset = asset)
    return(ma_data)
  })
  
  sector_ma_combined <- bind_rows(sector_ma_data) %>%
    group_by(Date) %>%
    summarise(
      `Sector_50_MA` = mean(`50_MA`, na.rm = TRUE),
      `Sector_200_MA` = mean(`200_MA`, na.rm = TRUE)
    ) %>%
    mutate(
      Sector = sector_name,
      Golden_Cross = if_else(lag(`Sector_50_MA`) < lag(`Sector_200_MA`) & `Sector_50_MA` > `Sector_200_MA`, 1, 0),
      Death_Cross = if_else(lag(`Sector_50_MA`) > lag(`Sector_200_MA`) & `Sector_50_MA` < `Sector_200_MA`, 1, 0)
    )
  return(sector_ma_combined)
}

# Calculate sector-wise moving averages
tech_ma <- calculate_sector_moving_averages(stock_data, tech_stocks, "Technology")
indices_ma <- calculate_sector_moving_averages(stock_data, indices, "Indices")
commodities_ma <- calculate_sector_moving_averages(stock_data, commodities, "Commodities")
crypto_ma <- calculate_sector_moving_averages(stock_data, cryptocurrencies, "Cryptocurrencies")

# Combine sector data
sector_ma_data <- bind_rows(tech_ma, indices_ma, commodities_ma, crypto_ma)

# Plot sector moving averages with golden and death crosses
ggplot(sector_ma_data, aes(x = Date)) +
  geom_line(aes(y = `Sector_50_MA`, color = "50-Day MA")) +
  geom_line(aes(y = `Sector_200_MA`, color = "200-Day MA")) +
  geom_point(data = subset(sector_ma_data, Golden_Cross == 1), aes(y = `Sector_50_MA`, color = "Golden Cross"), shape = 17, size = 3) +
  geom_point(data = subset(sector_ma_data, Death_Cross == 1), aes(y = `Sector_50_MA`, color = "Death Cross"), shape = 17, size = 3) +
  facet_wrap(~ Sector, scales = "free_y") +  # Separate facets for each sector
  labs(
    title = "50-Day and 200-Day Moving Averages with Golden and Death Crosses by Sector",
    x = "Date", y = "Price",
    color = "Legend"
  ) +
  scale_color_manual(values = c("50-Day MA" = "blue", "200-Day MA" = "purple", "Golden Cross" = "green", "Death Cross" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")
