# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format as needed

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))))

# Select only Date and Price columns for technology stocks
tech_stock_prices <- stock_data %>%
  select(Date, Apple_Price, Microsoft_Price, Nvidia_Price, Google_Price, 
         Meta_Price, Netflix_Price, Amazon_Price, Tesla_Price)

# Reshape data to long format for plotting
tech_stock_prices_long <- tech_stock_prices %>%
  pivot_longer(cols = -Date, names_to = "Stock", values_to = "Price") %>%
  mutate(Stock = gsub("_Price", "", Stock)) # Remove '_Price' suffix for cleaner labels

# Plot daily closing prices for each stock
ggplot(tech_stock_prices_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  labs(title = "Daily Closing Prices of Technology Sector Stocks",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")
