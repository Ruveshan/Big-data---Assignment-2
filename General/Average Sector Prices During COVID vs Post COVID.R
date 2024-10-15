# Load necessary libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Load and preprocess the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define sectors and assets
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")
indices <- c("S.P_500_Price", "Nasdaq_100_Price")
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")
cryptocurrencies <- c("Bitcoin_Price", "Ethereum_Price")

# Convert all price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(c(tech_stocks, indices, commodities, cryptocurrencies)), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Define helper function to calculate sector average
calculate_sector_average <- function(data, assets) {
  data %>%
    select(all_of(assets)) %>%
    summarise(across(everything(), mean, na.rm = TRUE)) %>%
    rowMeans(na.rm = TRUE)
}

# Calculate average prices for each sector during COVID and post-COVID
sector_averages <- bind_rows(
  tibble(
    Sector = c("Technology", "Indices", "Commodities", "Cryptocurrencies"),
    Avg_Price = c(
      calculate_sector_average(during_covid_data, tech_stocks),
      calculate_sector_average(during_covid_data, indices),
      calculate_sector_average(during_covid_data, commodities),
      calculate_sector_average(during_covid_data, cryptocurrencies)
    ),
    Period = "During COVID"
  ),
  tibble(
    Sector = c("Technology", "Indices", "Commodities", "Cryptocurrencies"),
    Avg_Price = c(
      calculate_sector_average(post_covid_data, tech_stocks),
      calculate_sector_average(post_covid_data, indices),
      calculate_sector_average(post_covid_data, commodities),
      calculate_sector_average(post_covid_data, cryptocurrencies)
    ),
    Period = "Post COVID"
  )
)

# Print the sector averages to the console
print("Sector Average Prices During and Post-COVID:")
print(sector_averages)

# Plot comparison of sector averages during and post-COVID
ggplot(sector_averages, aes(x = Sector, y = Avg_Price, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(
    title = "Average Sector Prices During COVID vs Post COVID",
    x = "Sector",
    y = "Average Price"
  ) +
  scale_fill_manual(values = c("During COVID" = "lightblue", "Post COVID" = "orange")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
