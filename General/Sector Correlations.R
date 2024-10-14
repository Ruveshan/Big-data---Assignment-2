# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define asset categories by sector
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")
indices <- c("S.P_500_Price", "Nasdaq_100_Price")
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")
cryptocurrencies <- c("Bitcoin_Price", "Ethereum_Price")

# Combine all asset columns for analysis
all_assets <- c(tech_stocks, indices, commodities, cryptocurrencies)

# Ensure all price columns are numeric
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Filter data from 2020 onwards
filtered_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01"))

# Calculate daily returns for each asset
returns_data <- filtered_data %>%
  select(Date, all_of(all_assets)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}"))

# Define return columns for correlation analysis
return_columns <- grep("^Return_", colnames(returns_data), value = TRUE)

# During COVID-19 period (2020-2021)
during_covid_returns <- returns_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))
during_covid_corr <- cor(during_covid_returns[return_columns], use = "complete.obs")

# Post COVID-19 period (2022 onwards)
post_covid_returns <- returns_data %>%
  filter(Date >= as.Date("2022-01-01"))
post_covid_corr <- cor(post_covid_returns[return_columns], use = "complete.obs")



# Plot Post COVID-19 Correlation Matrix
corrplot(post_covid_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Sector Correlations Post COVID-19 (2022-2024)",
         mar = c(0, 0, 2, 0), # Adjust margin around the plot
         tl.cex = 0.8,        # Increase label font size
         cl.cex = 0.8,        # Increase color legend font size
         number.cex = 0.7)    # Adjust coefficient font size for clarity

# Plot During COVID-19 Correlation Matrix
corrplot(during_covid_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Sector Correlations During COVID-19 (2020-2021)",
         mar = c(0, 0, 2, 0), # Adjust margin around the plot
         tl.cex = 0.8,        # Increase label font size
         cl.cex = 0.8,        # Increase color legend font size
         number.cex = 0.7)    # Adjust coefficient font size for clarity
