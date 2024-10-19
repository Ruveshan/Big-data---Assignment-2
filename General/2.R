# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Print column names to check for any discrepancies
print(colnames(stock_data))

# Define sectors and their respective assets (adjust this if column names are different)
sectors <- list(
  Technology = c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price"),
  Commodities = c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                  "Silver_Price", "Platinum_Price", "Copper_Price"),
  Cryptocurrencies = c("Bitcoin_Price", "Ethereum_Price"),
  Indices = c("S.P_500_Price", "Nasdaq_100_Price")
)

# Combine all sector columns into a single list of assets
all_assets <- unlist(sectors)

# Convert all price columns to numeric, handling commas and non-numeric values
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .)), .names = "{.col}"))

# Print stock_data to confirm the changes
print(head(stock_data))
