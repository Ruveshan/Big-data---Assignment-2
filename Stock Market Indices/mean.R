# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the stock indices prices
indices_prices <- c("S.P_500_Price", "Nasdaq_100_Price")

# Convert stock indices price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(indices_prices), ~ as.numeric(gsub(",", "", .))))

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the mean price for the stock indices sector during COVID and post-COVID
mean_during_covid <- during_covid_data %>%
  summarise(across(all_of(indices_prices), mean, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

mean_post_covid <- post_covid_data %>%
  summarise(across(all_of(indices_prices), mean, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

# Combine the results into a data frame for visualization
mean_indices <- data.frame(
  Period = c("During COVID (2020-2021)", "Post-COVID (2022-2024)"),
  Mean_Price = c(mean_during_covid, mean_post_covid)
)

# Print the mean values to the console
print("Mean Prices of Stock Indices Sector During and Post-COVID:")
print(mean_indices)

# Plot the mean prices comparison
ggplot(mean_indices, aes(x = Period, y = Mean_Price, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Mean Price of Stock Indices Sector During and Post-COVID",
    x = "Period",
    y = "Mean Price (USD)"
  ) +
  scale_fill_manual(values = c("During COVID (2020-2021)" = "lightblue", "Post-COVID (2022-2024)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")
