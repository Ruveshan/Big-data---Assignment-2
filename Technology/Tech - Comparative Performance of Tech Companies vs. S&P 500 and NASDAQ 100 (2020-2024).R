# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the list of tech companies and their corresponding price columns
price_columns <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                   "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")

# Include S&P 500 and NASDAQ 100 indices for comparison
index_columns <- c("S.P_500_Price", "Nasdaq_100_Price")

# Ensure all price columns are numeric, removing commas if present
stock_data <- stock_data %>%
  mutate(across(all_of(c(price_columns, index_columns)), ~ as.numeric(gsub(",", "", .))))

# Filter data from 2020 onwards to focus on COVID-19 and post-COVID periods
filtered_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01"))

# Calculate daily returns for each tech company and indices
returns_data <- filtered_data %>%
  select(Date, all_of(price_columns), all_of(index_columns)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}"))

# Calculate cumulative returns
cumulative_returns <- returns_data %>%
  mutate(across(starts_with("Return"), ~ cumsum(replace_na(., 0)), .names = "Cum_{col}"))

# Reshape cumulative returns data for plotting
cumulative_long <- cumulative_returns %>%
  select(Date, starts_with("Cum_Return")) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Cumulative_Return") %>%
  mutate(Asset = sub("Cum_Return_", "", Asset),
         Asset = sub("_Price", "", Asset))  # Clean up asset names

# Plot cumulative returns for tech companies and indices
ggplot(cumulative_long, aes(x = Date, y = Cumulative_Return, color = Asset)) +
  geom_line() +
  labs(title = "Comparative Performance of Tech Companies vs. S&P 500 and NASDAQ 100 (2020-2024)",
       x = "Date", y = "Cumulative Return") +
  theme_minimal() +
  theme(legend.position = "bottom")
