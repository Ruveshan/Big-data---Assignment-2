# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the list of assets by sector
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")
indices <- c("S.P_500_Price", "Nasdaq_100_Price")
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")
cryptocurrencies <- c("Bitcoin_Price", "Ethereum_Price")

# Combine all asset columns for analysis
all_assets <- c(tech_stocks, indices, commodities, cryptocurrencies)

# Ensure all price columns are numeric, removing commas if present
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Define the pandemic period
pandemic_period <- as.Date(c("2020-01-01", "2021-12-31"))

# Filter data to the pandemic period
pandemic_data <- stock_data %>%
  filter(Date >= pandemic_period[1] & Date <= pandemic_period[2])

# Function to calculate maximum drawdown
calculate_max_drawdown <- function(prices) {
  drawdown <- (prices / cummax(prices)) - 1  # Calculate drawdown as percent decline from peak
  max_drawdown <- min(drawdown, na.rm = TRUE)  # Find the largest drawdown (most negative value)
  return(max_drawdown)
}

# Calculate maximum drawdown for each asset
drawdown_results <- pandemic_data %>%
  summarise(across(all_of(all_assets), calculate_max_drawdown, .names = "Drawdown_{col}"))

# Reshape data for easier analysis and viewing
drawdown_results_long <- drawdown_results %>%
  pivot_longer(cols = everything(), names_to = "Asset", values_to = "Max_Drawdown") %>%
  mutate(Sector = case_when(
    grepl("Apple|Microsoft|Nvidia|Google|Meta|Netflix|Amazon|Tesla", Asset) ~ "Technology",
    grepl("S.P_500|Nasdaq_100", Asset) ~ "Indices",
    grepl("Natural_Gas|Crude_oil|Gold|Silver|Platinum|Copper", Asset) ~ "Commodities",
    grepl("Bitcoin|Ethereum", Asset) ~ "Cryptocurrencies",
    TRUE ~ "Other"
  ))

# Replace "Drawdown_" prefix for cleaner labeling
drawdown_results_long$Asset <- gsub("Drawdown_", "", drawdown_results_long$Asset)

# Print the results
print(drawdown_results_long)

# Optional: Plotting the maximum drawdown by sector
ggplot(drawdown_results_long, aes(x = Asset, y = Max_Drawdown, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sector, scales = "free_x") +
  labs(title = "Maximum Drawdown of Assets by Sector During COVID-19 Pandemic",
       x = "Asset", y = "Maximum Drawdown") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
