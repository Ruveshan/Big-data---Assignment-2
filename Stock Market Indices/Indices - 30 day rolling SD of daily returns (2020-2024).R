# Load necessary libraries
library(dplyr)
library(zoo)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Filter data from 2020 to 2024
filtered_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2024-12-31"))

# Convert prices to numeric and remove rows with NA values
filtered_data <- filtered_data %>%
  mutate(SP500_Price = as.numeric(gsub(",", "", S.P_500_Price)),  # Ensure numeric format without commas
         NASDAQ100_Price = as.numeric(gsub(",", "", Nasdaq_100_Price))) %>%
  filter(!is.na(SP500_Price) & !is.na(NASDAQ100_Price))  # Remove rows with NA prices

# Calculate daily returns for both indices
filtered_data <- filtered_data %>%
  mutate(SP500_Return = c(NA, diff(log(SP500_Price))),
         NASDAQ100_Return = c(NA, diff(log(NASDAQ100_Price))))

# Calculate the rolling standard deviation (30-day window) for both indices
filtered_data <- filtered_data %>%
  mutate(SP500_Rolling_SD = rollapply(SP500_Return, width = 30, FUN = sd, fill = NA, align = "right"),
         NASDAQ100_Rolling_SD = rollapply(NASDAQ100_Return, width = 30, FUN = sd, fill = NA, align = "right"))

# Plot the rolling standard deviation for both indices
ggplot(filtered_data, aes(x = Date)) +
  geom_line(aes(y = SP500_Rolling_SD, color = "S&P 500")) +
  geom_line(aes(y = NASDAQ100_Rolling_SD, color = "NASDAQ 100")) +
  labs(title = "30-Day Rolling Standard Deviation of Daily Returns (2020-2024)",
       x = "Date", y = "Rolling Standard Deviation of Returns") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("S&P 500" = "blue", "NASDAQ 100" = "red"))
