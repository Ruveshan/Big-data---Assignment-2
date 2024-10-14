# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format if needed

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(c("Bitcoin_Price", "Ethereum_Price"), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Plot daily price trends for Bitcoin and Ethereum
price_trends <- stock_data %>%
  select(Date, Bitcoin_Price, Ethereum_Price) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Price")

ggplot(price_trends, aes(x = Date, y = Price, color = Asset)) +
  geom_line() +
  labs(title = "Daily Price Trends for Bitcoin and Ethereum",
       x = "Date", y = "Price (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate daily returns for volatility analysis
stock_data <- stock_data %>%
  mutate(Bitcoin_Return = c(NA, diff(log(Bitcoin_Price))),
         Ethereum_Return = c(NA, diff(log(Ethereum_Price))))

# Filter data for volatility analysis by period
during_covid_returns <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31")) %>%
  select(Date, Bitcoin_Return, Ethereum_Return)

post_covid_returns <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31")) %>%
  select(Date, Bitcoin_Return, Ethereum_Return)

# Calculate and compare volatility (standard deviation of returns)
volatility_metrics <- data.frame(
  Asset = c("Bitcoin", "Ethereum"),
  During_COVID_SD = c(sd(during_covid_returns$Bitcoin_Return, na.rm = TRUE),
                      sd(during_covid_returns$Ethereum_Return, na.rm = TRUE)),
  Post_COVID_SD = c(sd(post_covid_returns$Bitcoin_Return, na.rm = TRUE),
                    sd(post_covid_returns$Ethereum_Return, na.rm = TRUE))
)

# Print volatility metrics for verification
print("Volatility (Standard Deviation of Returns) During and Post-COVID:")
print(volatility_metrics)

# Reshape data for plotting volatility
volatility_long <- volatility_metrics %>%
  pivot_longer(cols = -Asset, names_to = "Period", values_to = "Volatility") %>%
  mutate(Period = ifelse(Period == "During_COVID_SD", "During COVID", "Post COVID"))

# Plot volatility comparison
ggplot(volatility_long, aes(x = Asset, y = Volatility, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Volatility (Standard Deviation of Returns) for Bitcoin and Ethereum",
       x = "Asset", y = "Volatility (Standard Deviation of Returns)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
