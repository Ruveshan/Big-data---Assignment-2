# Load necessary libraries
library(dplyr)
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
all_assets <- c(tech_stocks, indices, commodities, cryptocurrencies)

# Convert all price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Split data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01"))

# Calculate peak values during COVID for each asset
covid_peaks <- during_covid_data %>%
  summarise(across(all_of(all_assets), max, na.rm = TRUE)) %>%
  pivot_longer(cols = everything(), names_to = "Asset", values_to = "Covid_Peak")

# Find recovery date in the post-COVID period
recovery_data <- post_covid_data %>%
  select(Date, all_of(all_assets)) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Price") %>%
  left_join(covid_peaks, by = "Asset") %>%
  group_by(Asset) %>%
  filter(!is.na(Covid_Peak) & Price >= Covid_Peak) %>%
  summarise(Recovery_Date = min(Date, na.rm = TRUE)) %>%
  left_join(covid_peaks, by = "Asset") %>%
  mutate(Recovery_Time = as.numeric(difftime(Recovery_Date, as.Date("2022-01-01"), units = "days")),
         Sector = case_when(
           Asset %in% tech_stocks ~ "Technology",
           Asset %in% indices ~ "Indices",
           Asset %in% commodities ~ "Commodities",
           Asset %in% cryptocurrencies ~ "Cryptocurrencies",
           TRUE ~ "Other"
         ))

# Replace NA with message indicating non-recovery in the dataset timeframe
recovery_data$Recovery_Time[is.na(recovery_data$Recovery_Time)] <- Inf

# Print the recovery data
print("Sector Recovery Time Comparison:")
print(recovery_data)

# Plot the recovery times for visual comparison
ggplot(recovery_data, aes(x = reorder(Asset, Recovery_Time), y = Recovery_Time, fill = Sector)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Recovery Time of Assets by Sector to COVID Peak Levels",
    x = "Asset",
    y = "Recovery Time (Days)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3")
