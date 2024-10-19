# Load necessary libraries
library(dplyr)
library(ggplot2)
library(rugarch)  # Ensure this package is installed for GARCH modeling

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
  drop_na()  # Remove any rows with NA returns

# Reshape returns data to long format for analysis
long_returns <- returns_data %>%
  pivot_longer(cols = starts_with("Return"), names_to = "Asset", values_to = "Return") %>%
  mutate(Sector = case_when(
    Asset %in% paste0("Return_", sectors$Technology) ~ "Technology",
    Asset %in% paste0("Return_", sectors$Commodities) ~ "Commodities",
    Asset %in% paste0("Return_", sectors$Cryptocurrencies) ~ "Cryptocurrencies",
    Asset %in% paste0("Return_", sectors$Indices) ~ "Indices",
    TRUE ~ "Other"
  ))

# GARCH Model Setup for Volatility Clustering
garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(1, 0), include.mean = TRUE),
                         distribution.model = "std")

# Fit GARCH models for each sector's representative asset and extract volatility
volatility_data <- long_returns %>%
  filter(Asset %in% c("Return_Apple_Price", "Return_Crude_oil_Price", 
                      "Return_Bitcoin_Price", "Return_S.P_500_Price")) %>%
  group_by(Sector) %>%
  do({
    asset_returns <- .$Return
    garch_fit <- ugarchfit(spec = garch_spec, data = asset_returns)
    data.frame(Date = .$Date, Sector = .$Sector, Volatility = sigma(garch_fit))
  }) %>%
  ungroup()

# Print the volatility data to the console
print("Volatility Data for Each Sector's Representative Asset:")
print(volatility_data)

# Visualization of Volatility Clustering Over Time by Sector
ggplot(volatility_data, aes(x = Date, y = Volatility, color = Sector, group = Sector)) +
  geom_line() +
  labs(title = "Volatility Clustering Over Time by Sector", x = "Date", y = "Volatility") +
  theme_minimal() +
  theme(legend.position = "bottom")
