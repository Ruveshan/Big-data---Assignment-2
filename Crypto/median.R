# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the cryptocurrency prices
crypto_prices <- c("Bitcoin_Price", "Ethereum_Price")

# Convert cryptocurrency price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(crypto_prices), ~ as.numeric(gsub(",", "", .))))

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the median price for the cryptocurrency sector during COVID and post-COVID
median_during_covid <- during_covid_data %>%
  summarise(across(all_of(crypto_prices), median, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

median_post_covid <- post_covid_data %>%
  summarise(across(all_of(crypto_prices), median, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

# Combine the results into a data frame for visualization
median_crypto <- data.frame(
  Period = c("During COVID (2020-2021)", "Post-COVID (2022-2024)"),
  Median_Price = c(median_during_covid, median_post_covid)
)

# Print the median values to the console
print("Median Prices of Cryptocurrency Sector During and Post-COVID:")
print(median_crypto)

# Plot the median prices comparison
ggplot(median_crypto, aes(x = Period, y = Median_Price, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Median Price of Cryptocurrency Sector During and Post-COVID",
    x = "Period",
    y = "Median Price (USD)"
  ) +
  scale_fill_manual(values = c("During COVID (2020-2021)" = "lightblue", "Post-COVID (2022-2024)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")
