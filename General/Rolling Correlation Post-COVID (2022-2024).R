# Load necessary libraries
library(dplyr)
library(zoo)
library(ggplot2)
library(corrplot)

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

# Combine all sector columns into a single list and convert to numeric
all_assets <- unlist(sectors)
existing_assets <- intersect(all_assets, colnames(stock_data))
stock_data <- stock_data %>%
  mutate(across(all_of(existing_assets), ~ as.numeric(gsub(",", "", .))))

# Calculate daily returns for each asset
returns_data <- stock_data %>%
  mutate(across(all_of(existing_assets), ~ c(NA, diff(log(.))), .names = "Return_{col}")) %>%
  select(Date, starts_with("Return"))

# Filter the data for COVID (2020-2021) and post-COVID (2022-2024) periods
covid_returns <- returns_data %>% filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))
post_covid_returns <- returns_data %>% filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Define representative assets to simplify the correlation analysis
representative_assets <- c("Return_Apple_Price", "Return_Crude_oil_Price", "Return_Bitcoin_Price", "Return_S.P_500_Price")

# Function to calculate rolling correlations for each asset pair within a given window
calculate_rolling_correlation <- function(data, assets, window = 30) {
  rollapply(
    data %>% select(all_of(assets)),
    width = window,
    FUN = function(x) {
      cor_matrix <- cor(x, use = "pairwise.complete.obs")
      return(cor_matrix)
    },
    by.column = FALSE,
    align = "right"
  )
}

# Calculate rolling correlations for COVID and post-COVID periods
covid_rolling_corr <- calculate_rolling_correlation(covid_returns, representative_assets)
post_covid_rolling_corr <- calculate_rolling_correlation(post_covid_returns, representative_assets)

# Ensure that the last rolling correlation is a matrix
covid_last_corr_matrix <- as.matrix(covid_rolling_corr[[length(covid_rolling_corr)]])
post_covid_last_corr_matrix <- as.matrix(post_covid_rolling_corr[[length(post_covid_rolling_corr)]])

# Plot rolling correlations for COVID and post-COVID
par(mfrow = c(1, 2))  # Set up a 1x2 plot layout

# COVID period correlation plot
if (is.matrix(covid_last_corr_matrix)) {
  corrplot(covid_last_corr_matrix, method = "color", title = "Rolling Correlation During COVID (2020-2021)",
           mar = c(0, 0, 1, 0))
} else {
  print("COVID period correlation matrix is not available.")
}

# Post-COVID period correlation plot
if (is.matrix(post_covid_last_corr_matrix)) {
  corrplot(post_covid_last_corr_matrix, method = "color", title = "Rolling Correlation Post-COVID (2022-2024)",
           mar = c(0, 0, 1, 0))
} else {
  print("Post-COVID period correlation matrix is not available.")
}
