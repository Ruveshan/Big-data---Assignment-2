# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Filter data to include pre-, during-, and post-COVID periods (2018-2024)
filtered_data <- stock_data %>%
  filter(Date >= as.Date("2018-01-01") & Date <= as.Date("2024-12-31"))

# Convert prices to numeric if needed and handle NA values
filtered_data <- filtered_data %>%
  mutate(SP500_Price = as.numeric(gsub(",", "", S.P_500_Price)),
         NASDAQ100_Price = as.numeric(gsub(",", "", Nasdaq_100_Price))) %>%
  filter(!is.na(SP500_Price) & !is.na(NASDAQ100_Price))  # Remove rows with NA prices

# Calculate 50-day and 200-day moving averages for both indices
filtered_data <- filtered_data %>%
  mutate(SP500_50MA = rollapply(SP500_Price, width = 50, FUN = mean, fill = NA, align = "right"),
         SP500_200MA = rollapply(SP500_Price, width = 200, FUN = mean, fill = NA, align = "right"),
         NASDAQ100_50MA = rollapply(NASDAQ100_Price, width = 50, FUN = mean, fill = NA, align = "right"),
         NASDAQ100_200MA = rollapply(NASDAQ100_Price, width = 200, FUN = mean, fill = NA, align = "right"))



# Plot the moving averages for NASDAQ 100
ggplot(filtered_data, aes(x = Date)) +
  geom_line(aes(y = NASDAQ100_Price, color = "NASDAQ 100 Price")) +
  geom_line(aes(y = NASDAQ100_50MA, color = "50-Day MA")) +
  geom_line(aes(y = NASDAQ100_200MA, color = "200-Day MA")) +
  labs(title = "NASDAQ 100 Price with 50-Day and 200-Day Moving Averages (2018-2024)",
       x = "Date", y = "Price") +
  theme_minimal() +
  scale_color_manual(values = c("NASDAQ 100 Price" = "black", "50-Day MA" = "blue", "200-Day MA" = "red")) +
  theme(legend.position = "bottom")

# Plot the moving averages for S&P 500
ggplot(filtered_data, aes(x = Date)) +
  geom_line(aes(y = SP500_Price, color = "S&P 500 Price")) +
  geom_line(aes(y = SP500_50MA, color = "50-Day MA")) +
  geom_line(aes(y = SP500_200MA, color = "200-Day MA")) +
  labs(title = "S&P 500 Price with 50-Day and 200-Day Moving Averages (2018-2024)",
       x = "Date", y = "Price") +
  theme_minimal() +
  scale_color_manual(values = c("S&P 500 Price" = "black", "50-Day MA" = "blue", "200-Day MA" = "red")) +
  theme(legend.position = "bottom")
