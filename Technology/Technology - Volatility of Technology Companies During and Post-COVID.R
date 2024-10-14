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

# Define COVID-19 and post-COVID periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01"))

# Calculate daily returns for each tech company during COVID-19
during_covid_returns <- during_covid_data %>%
  select(Date, all_of(price_columns)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}"))

# Calculate daily returns for each tech company post-COVID-19
post_covid_returns <- post_covid_data %>%
  select(Date, all_of(price_columns)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}"))

# Calculate volatility (standard deviation of daily returns) for each tech company in each period
volatility_during <- during_covid_returns %>%
  summarise(across(starts_with("Return"), \(x) sd(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Company", values_to = "Volatility") %>%
  mutate(Company = sub("Return_", "", Company),
         Company = sub("_Price", "", Company),
         Period = "During COVID-19")

volatility_post <- post_covid_returns %>%
  summarise(across(starts_with("Return"), \(x) sd(x, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "Company", values_to = "Volatility") %>%
  mutate(Company = sub("Return_", "", Company),
         Company = sub("_Price", "", Company),
         Period = "Post COVID-19")

# Combine the two volatility data frames
volatility_data <- bind_rows(volatility_during, volatility_post)

# Plot volatility for each tech company during and post-COVID
ggplot(volatility_data, aes(x = Company, y = Volatility, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Volatility of Technology Companies During and Post-COVID",
       x = "Company", y = "Volatility (Standard Deviation of Daily Returns)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
