# Load necessary libraries
library(dplyr)
library(tidyr)
library(corrplot)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))))

# Select key assets based on your list
# Cryptocurrencies: Bitcoin and Ethereum
# Commodities: Natural Gas, Crude Oil, Gold, Silver, Platinum, Copper
# Stock Market Indices: S&P 500, NASDAQ 100
# Technology Companies: Apple, Microsoft, NVIDIA, Alphabet, Meta, Netflix, Amazon, Tesla
selected_assets <- stock_data %>%
  select(Date, Bitcoin_Price, Ethereum_Price, Natural_Gas_Price, Crude_oil_Price, Gold_Price,
         Silver_Price, Platinum_Price, Copper_Price, SP500_Price = S.P_500_Price, 
         NASDAQ100_Price = Nasdaq_100_Price, Apple_Price, Microsoft_Price, 
         Nvidia_Price, Google_Price = Google_Price, Meta_Price, Netflix_Price, Amazon_Price, Tesla_Price)

# Calculate daily returns for each selected asset
selected_assets <- selected_assets %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}")) %>%
  select(Date, starts_with("Return_"))

# Split data into during-COVID and post-COVID periods
during_covid_returns <- selected_assets %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31")) %>%
  select(-Date)

post_covid_returns <- selected_assets %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31")) %>%
  select(-Date)

# Calculate correlation matrices for each period
during_covid_corr <- cor(during_covid_returns, use = "complete.obs")
post_covid_corr <- cor(post_covid_returns, use = "complete.obs")

# Display correlation matrices
print("During COVID Correlation Matrix:")
print(during_covid_corr)

print("Post COVID Correlation Matrix:")
print(post_covid_corr)

# Visualize correlations with corrplot
# During COVID correlation plot
corrplot(during_covid_corr, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Correlations During COVID-19 (2020-2021)")

# Post COVID correlation plot
corrplot(post_covid_corr, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Correlations Post COVID-19 (2022-2024)")
