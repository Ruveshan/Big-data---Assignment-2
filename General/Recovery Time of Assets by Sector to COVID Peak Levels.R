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

# Calculate individual recovery times for each asset in the post-COVID period
recovery_data <- post_covid_data %>%
  select(Date, all_of(all_assets)) %>%
  pivot_longer(cols = -Date, names_to = "Asset", values_to = "Price") %>%
  left_join(covid_peaks, by = "Asset") %>%
  group_by(Asset) %>%
  mutate(
    Recovered = Price >= Covid_Peak,
    Recovery_Date = if_else(Recovered, Date, as.Date(NA))
  ) %>%
  filter(!is.na(Recovery_Date)) %>%
  summarise(Recovery_Date = min(Recovery_Date, na.rm = TRUE)) %>%
  left_join(covid_peaks, by = "Asset") %>%
  mutate(
    Recovery_Time = as.numeric(difftime(Recovery_Date, as.Date("2022-01-01"), units = "days")),
    Sector = case_when(
      Asset %in% tech_stocks ~ "Technology",
      Asset %in% indices ~ "Indices",
      Asset %in% commodities ~ "Commodities",
      Asset %in% cryptocurrencies ~ "Cryptocurrencies",
      TRUE ~ "Unassigned"
    )
  ) %>%
  # Ensure Inf values are replaced with NA for assets that did not recover
  mutate(Recovery_Time = ifelse(is.infinite(Recovery_Time), NA, Recovery_Time))

# Add rows for assets that never recovered
missing_recoveries <- setdiff(all_assets, recovery_data$Asset)
if (length(missing_recoveries) > 0) {
  missing_data <- data.frame(
    Asset = missing_recoveries,
    Recovery_Date = NA,
    Covid_Peak = NA,
    Recovery_Time = NA,
    Sector = case_when(
      missing_recoveries %in% tech_stocks ~ "Technology",
      missing_recoveries %in% indices ~ "Indices",
      missing_recoveries %in% commodities ~ "Commodities",
      missing_recoveries %in% cryptocurrencies ~ "Cryptocurrencies",
      TRUE ~ "Unassigned"
    )
  )
  recovery_data <- bind_rows(recovery_data, missing_data)
}

# Calculate the average recovery time for each sector
sector_recovery <- recovery_data %>%
  group_by(Sector) %>%
  summarise(Average_Recovery_Time = mean(Recovery_Time, na.rm = TRUE)) %>%
  mutate(
    Recovery_Status = ifelse(is.na(Average_Recovery_Time), "No Recovery", "Recovered"),
    Average_Recovery_Time = ifelse(is.na(Average_Recovery_Time), 0, Average_Recovery_Time)  # Replace NA with 0 for plotting
  )

# Print the sector-level recovery data
print("Sector Recovery Time Comparison:")
print(sector_recovery)

# Plot the average recovery times for each sector
ggplot(sector_recovery, aes(x = Sector, y = Average_Recovery_Time, fill = Sector)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = ifelse(Recovery_Status == "No Recovery", "No Recovery", round(Average_Recovery_Time, 1))), 
            vjust = -0.5, color = "black", size = 3) +
  labs(
    title = "Average Recovery Time of Sectors to COVID Peak Levels",
    x = "Sector",
    y = "Average Recovery Time (Days)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))  # Adjust y-axis for labels
