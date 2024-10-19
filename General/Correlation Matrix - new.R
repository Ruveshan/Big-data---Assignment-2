# Load necessary libraries
library(dplyr)
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
  select(Date, starts_with("Return")) %>%
  drop_na()

# Define COVID and post-COVID periods
covid_returns <- returns_data %>% filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))
post_covid_returns <- returns_data %>% filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate average daily returns for each sector in both periods
calculate_sector_avg_returns <- function(data) {
  data %>%
    summarise(
      Technology = rowMeans(select(., Return_Apple_Price, Return_Microsoft_Price, Return_Nvidia_Price, 
                                   Return_Google_Price, Return_Meta_Price, Return_Netflix_Price, 
                                   Return_Amazon_Price, Return_Tesla_Price), na.rm = TRUE),
      Commodities = rowMeans(select(., Return_Natural_Gas_Price, Return_Crude_oil_Price, Return_Gold_Price, 
                                    Return_Silver_Price, Return_Platinum_Price, Return_Copper_Price), na.rm = TRUE),
      Cryptocurrencies = rowMeans(select(., Return_Bitcoin_Price, Return_Ethereum_Price), na.rm = TRUE),
      Indices = rowMeans(select(., Return_S.P_500_Price, Return_Nasdaq_100_Price), na.rm = TRUE)
    )
}

# Calculate sector average returns for COVID and post-COVID periods
covid_sector_returns <- calculate_sector_avg_returns(covid_returns)
post_covid_sector_returns <- calculate_sector_avg_returns(post_covid_returns)

# Calculate correlation matrices for COVID and post-COVID periods
covid_corr_matrix <- cor(covid_sector_returns, method = "spearman", use = "pairwise.complete.obs")
post_covid_corr_matrix <- cor(post_covid_sector_returns, method = "spearman", use = "pairwise.complete.obs")

# Visualize correlation matrices for both periods
par(mfrow = c(1, 2))  # Set up a 1x2 plot layout

# COVID period correlation matrix plot
corrplot(covid_corr_matrix, method = "color", title = "Sector Correlation During COVID (2020-2021)",
         mar = c(0, 0, 1, 0))

# Post-COVID period correlation matrix plot
corrplot(post_covid_corr_matrix, method = "color", title = "Sector Correlation Post-COVID (2022-2024)",
         mar = c(0, 0, 1, 0))

# Reset par
par(mfrow = c(1, 1))

# Print correlation matrices to console
print("Correlation Matrix During COVID (2020-2021):")
print(covid_corr_matrix)
print("Correlation Matrix Post-COVID (2022-2024):")
print(post_covid_corr_matrix)
