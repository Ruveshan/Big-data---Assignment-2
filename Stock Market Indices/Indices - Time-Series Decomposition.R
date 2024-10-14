# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Convert prices to numeric and calculate daily returns
stock_data <- stock_data %>%
  mutate(SP500_Price = as.numeric(gsub(",", "", S.P_500_Price)),
         NASDAQ100_Price = as.numeric(gsub(",", "", Nasdaq_100_Price))) %>%
  filter(!is.na(SP500_Price) & !is.na(NASDAQ100_Price)) %>%
  mutate(SP500_Return = c(NA, diff(log(SP500_Price))),
         NASDAQ100_Return = c(NA, diff(log(NASDAQ100_Price))))

# Filter data from 2020 onwards to focus on COVID-19 and post-COVID periods and remove NA returns
filtered_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & !is.na(SP500_Return) & !is.na(NASDAQ100_Return))

# Convert the returns to time series objects with a weekly frequency of 52
sp500_ts <- ts(filtered_data$SP500_Return, frequency = 52)
nasdaq100_ts <- ts(filtered_data$NASDAQ100_Return, frequency = 52)

# Decompose the time series using STL (Seasonal-Trend decomposition)
sp500_decomp <- stl(sp500_ts, s.window = "periodic")
nasdaq100_decomp <- stl(nasdaq100_ts, s.window = "periodic")


# Decomposition for NASDAQ 100
autoplot(nasdaq100_decomp) +
  ggtitle("Time-Series Decomposition of NASDAQ 100 Weekly Returns (2020-2024)") +
  theme_minimal()

# Plot the decompositions
# Decomposition for S&P 500
autoplot(sp500_decomp) +
  ggtitle("Time-Series Decomposition of S&P 500 Weekly Returns (2020-2024)") +
  theme_minimal()
