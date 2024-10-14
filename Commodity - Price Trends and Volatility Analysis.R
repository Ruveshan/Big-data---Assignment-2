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
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# List of commodity price columns
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")

# Initialize data frame to store results
commodity_metrics <- data.frame(Commodity = commodities, 
                                During_COVID_AvgPrice = NA, During_COVID_SDPrice = NA,
                                Post_COVID_AvgPrice = NA, Post_COVID_SDPrice = NA)

# Calculate metrics for each commodity
for (commodity in commodities) {
  # During COVID metrics
  during_avg_price <- mean(during_covid_data[[commodity]], na.rm = TRUE)
  during_sd_price <- sd(during_covid_data[[commodity]], na.rm = TRUE)
  
  # Post COVID metrics
  post_avg_price <- mean(post_covid_data[[commodity]], na.rm = TRUE)
  post_sd_price <- sd(post_covid_data[[commodity]], na.rm = TRUE)
  
  # Store results in the data frame
  commodity_metrics[commodity_metrics$Commodity == commodity, ] <- 
    c(commodity, during_avg_price, during_sd_price, post_avg_price, post_sd_price)
}

# Print the metrics for verification
print("Commodity Metrics:")
print(commodity_metrics)

# Reshape commodity_metrics for volatility (standard deviation) plotting
commodity_metrics_long <- commodity_metrics %>%
  pivot_longer(cols = -Commodity, names_to = c("Period", "Metric"), names_sep = "_")

# Check the reshaped data for any missing values
print("Reshaped Commodity Metrics (for plotting):")
print(commodity_metrics_long)

# Remove any rows with missing values in commodity_metrics_long for plotting
commodity_metrics_long <- commodity_metrics_long %>% filter(!is.na(value))

# Plot volatility (standard deviation) comparison during and post-COVID
ggplot(commodity_metrics_long %>% filter(Metric == "SDPrice"), 
       aes(x = Commodity, y = value, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Commodity Volatility (Standard Deviation) During and Post-COVID",
       x = "Commodity", y = "Standard Deviation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting price trends for each commodity over time
price_trends <- stock_data %>%
  select(Date, all_of(commodities)) %>%
  pivot_longer(cols = -Date, names_to = "Commodity", values_to = "Price") %>%
  filter(!is.na(Price))  # Remove rows with missing prices

# Plot price trends
ggplot(price_trends, aes(x = Date, y = Price, color = Commodity)) +
  geom_line() +
  labs(title = "Commodity Price Trends Over Time",
       x = "Date", y = "Price (USD)") +
  theme_minimal() +
  theme(legend.position = "bottom")
