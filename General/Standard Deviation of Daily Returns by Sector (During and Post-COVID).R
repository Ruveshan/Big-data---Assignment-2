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

# Define timeframes
during_covid_period <- as.Date(c("2020-01-01", "2021-12-31"))
post_covid_period <- as.Date(c("2022-01-01", "2024-12-31"))

# Calculate daily returns for each asset
returns_data <- stock_data %>%
  select(Date, all_of(all_assets)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}"))

# Function to calculate standard deviation of returns for a given period
calc_sd_for_period <- function(data, period) {
  data %>%
    filter(Date >= period[1] & Date <= period[2]) %>%
    summarise(across(starts_with("Return"), ~ sd(.x, na.rm = TRUE), .names = "SD_{col}"))
}

# Calculate standard deviations for during and post-COVID periods
during_covid_sd <- calc_sd_for_period(returns_data, during_covid_period)
post_covid_sd <- calc_sd_for_period(returns_data, post_covid_period)

# Combine results with period labels
sd_results <- bind_rows(
  during_covid_sd %>% mutate(Period = "During COVID"),
  post_covid_sd %>% mutate(Period = "Post-COVID")
)

# Reshape data for easier analysis and viewing
sd_results_long <- sd_results %>%
  pivot_longer(cols = -Period, names_to = "Asset", values_to = "SD") %>%
  mutate(Sector = case_when(
    grepl("Apple|Microsoft|Nvidia|Google|Meta|Netflix|Amazon|Tesla", Asset) ~ "Technology",
    grepl("S.P_500|Nasdaq_100", Asset) ~ "Indices",
    grepl("Natural_Gas|Crude_oil|Gold|Silver|Platinum|Copper", Asset) ~ "Commodities",
    grepl("Bitcoin|Ethereum", Asset) ~ "Cryptocurrencies",
    TRUE ~ "Other"
  ))

# Replace "Return_" prefix for cleaner labeling
sd_results_long$Asset <- gsub("Return_", "", sd_results_long$Asset)

# Print the results
print(sd_results_long)

# Optional: Plotting the standard deviation of returns by sector and period
ggplot(sd_results_long, aes(x = Period, y = SD, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Sector, scales = "free_y") +
  labs(title = "Standard Deviation of Daily Returns by Sector (During and Post-COVID)",
       x = "Period", y = "Standard Deviation of Daily Returns") +
  theme_minimal()
