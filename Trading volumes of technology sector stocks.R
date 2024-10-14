# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format as needed

# Convert prices and volumes to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))),
         across(ends_with("_Vol."), ~ as.numeric(gsub(",", "", .))))

# Filter for the technology sector stocks
tech_stocks <- stock_data %>%
  select(Date, Apple_Price, Apple_Vol., Microsoft_Price, Microsoft_Vol., 
         Nvidia_Price, Nvidia_Vol., Google_Price, Google_Vol., Meta_Price, Meta_Vol.,
         Netflix_Price, Netflix_Vol., Amazon_Price, Amazon_Vol., Tesla_Price, Tesla_Vol.) 

# Reshape data to long format for plotting
tech_stocks_long <- tech_stocks %>%
  pivot_longer(cols = -Date, names_to = c("Stock", "Metric"), names_sep = "_") %>%
  pivot_wider(names_from = Metric, values_from = value, values_fn = list)

# Remove rows where Price or Vol. has multiple values
tech_stocks_long <- tech_stocks_long %>%
  rowwise() %>%
  filter(length(Price) == 1, length(Vol.) == 1) %>%
  unnest(c(Price, Vol.))

# Plot daily closing prices for each stock
ggplot(tech_stocks_long, aes(x = Date, y = Price, color = Stock)) +
  geom_line() +
  labs(title = "Daily Closing Prices of Technology Sector Stocks",
       x = "Date", y = "Closing Price (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot daily volumes for each stock
ggplot(tech_stocks_long, aes(x = Date, y = Vol., color = Stock)) +
  geom_line() +
  labs(title = "Daily Trading Volumes of Technology Sector Stocks",
       x = "Date", y = "Volume") +
  theme_minimal() +
  theme(legend.position = "bottom")
