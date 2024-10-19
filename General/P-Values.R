# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define sectors and their respective assets
sectors <- list(
  Technology = c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price"),
  Commodities = c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                  "Silver_Price", "Platinum_Price", "Copper_Price"),
  Cryptocurrencies = c("Bitcoin_Price", "Ethereum_Price"),
  Indices = c("S.P_500_Price", "Nasdaq_100_Price")
)

# Check and convert all relevant price columns to numeric, removing non-numeric characters
for (column in unlist(sectors)) {
  if (is.character(stock_data[[column]])) {
    stock_data[[column]] <- as.numeric(gsub("[^0-9.-]", "", stock_data[[column]]))
  }
}

# Split data for COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))
post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Function to perform hypothesis testing for each sector
perform_sector_test <- function(during_data, post_data, sector_assets) {
  during_values <- during_data %>%
    select(all_of(sector_assets)) %>%
    pivot_longer(cols = everything(), values_to = "Price") %>%
    pull(Price)
  
  post_values <- post_data %>%
    select(all_of(sector_assets)) %>%
    pivot_longer(cols = everything(), values_to = "Price") %>%
    pull(Price)
  
  # Test for normality
  shapiro_during <- shapiro.test(during_values)$p.value
  shapiro_post <- shapiro.test(post_values)$p.value
  
  # Select test based on normality results
  if (shapiro_during > 0.05 && shapiro_post > 0.05) {
    test_result <- t.test(during_values, post_values, alternative = "two.sided")
  } else {
    test_result <- wilcox.test(during_values, post_values, alternative = "two.sided")
  }
  
  return(test_result$p.value)
}

# Apply hypothesis testing to each sector
sector_p_values <- data.frame(
  Sector = names(sectors),
  P_Value = sapply(sectors, function(assets) perform_sector_test(during_covid_data, post_covid_data, assets))
)

# Add a significance indicator
sector_p_values$Significance <- ifelse(sector_p_values$P_Value < 0.05, "Significant", "Not Significant")

# Print the results to the console
print("Hypothesis Testing Results for Sector Performance Changes:")
print(sector_p_values)

# Visualize the p-values by sector
ggplot(sector_p_values, aes(x = Sector, y = P_Value, fill = Significance)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "P-Values for Sector Performance Changes During and Post-COVID",
    x = "Sector",
    y = "P-Value"
  ) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("Significant" = "orange", "Not Significant" = "lightblue")) +
  theme_minimal() +
  theme(legend.position = "bottom")
