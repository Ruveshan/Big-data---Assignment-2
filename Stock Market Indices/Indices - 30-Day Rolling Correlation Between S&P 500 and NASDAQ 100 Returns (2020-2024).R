# Load necessary libraries
library(dplyr)
library(ggplot2)
library(zoo)

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

# Calculate the 30-day rolling correlation between S&P 500 and NASDAQ 100 returns
stock_data <- stock_data %>%
  mutate(Rolling_Correlation = rollapplyr(
    data = seq_along(SP500_Return),  # Use a sequence to access each window
    width = 30,
    FUN = function(i) cor(stock_data$SP500_Return[i], stock_data$NASDAQ100_Return[i], use = "complete.obs"),
    fill = NA
  ))

# Filter data from 2020 onwards to focus on COVID-19 and post-COVID periods
filtered_data <- stock_data %>% filter(Date >= as.Date("2020-01-01"))

# Plot the rolling correlation over time
ggplot(filtered_data, aes(x = Date, y = Rolling_Correlation)) +
  geom_line(color = "blue") +
  labs(title = "30-Day Rolling Correlation Between S&P 500 and NASDAQ 100 Returns (2020-2024)",
       x = "Date", y = "Rolling Correlation") +
  theme_minimal()
