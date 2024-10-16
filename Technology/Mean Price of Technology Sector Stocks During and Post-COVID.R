# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the technology stocks
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")

# Convert technology stock price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(tech_stocks), ~ as.numeric(gsub(",", "", .))))

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the mean price for the technology sector during COVID and post-COVID
mean_during_covid <- during_covid_data %>%
  summarise(across(all_of(tech_stocks), mean, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

mean_post_covid <- post_covid_data %>%
  summarise(across(all_of(tech_stocks), mean, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

# Combine the results into a data frame for visualization
mean_prices <- data.frame(
  Period = c("During COVID (2020-2021)", "Post-COVID (2022-2024)"),
  Mean_Price = c(mean_during_covid, mean_post_covid)
)

# Print the mean prices to the console
print("Mean Prices of Technology Sector Stocks During and Post-COVID:")
print(mean_prices)

# Plot the mean prices
ggplot(mean_prices, aes(x = Period, y = Mean_Price, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Mean Price of Technology Sector Stocks During and Post-COVID",
    x = "Period",
    y = "Mean Price (USD)"
  ) +
  scale_fill_manual(values = c("During COVID (2020-2021)" = "lightblue", "Post-COVID (2022-2024)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")
