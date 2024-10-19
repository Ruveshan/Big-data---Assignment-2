# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)  # For ANOVA and Tukey's HSD

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

# Standardize column names to prevent issues with pivot_longer
colnames(stock_data) <- make.names(colnames(stock_data), unique = TRUE)

# Combine all sector columns into a single list
all_assets <- unlist(sectors)

# Convert all price columns to numeric, handling commas and non-numeric values
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Rename Date column to prevent conflicts
stock_data <- stock_data %>%
  rename(Trade_Date = Date)

# Reshape data to long format for easier filtering and analysis
long_data <- stock_data %>%
  pivot_longer(cols = all_of(all_assets), names_to = "Asset", values_to = "Price") %>%
  drop_na(Price)  # Remove rows with NA prices

# Add sector information based on Asset names
long_data <- long_data %>%
  mutate(Sector = case_when(
    Asset %in% sectors$Technology ~ "Technology",
    Asset %in% sectors$Commodities ~ "Commodities",
    Asset %in% sectors$Cryptocurrencies ~ "Cryptocurrencies",
    Asset %in% sectors$Indices ~ "Indices",
    TRUE ~ "Other"
  ))

# Filter the data for COVID (2020-2021) and post-COVID (2022-2024) periods and add a period column
during_covid_data <- long_data %>%
  filter(Trade_Date >= as.Date("2020-01-01") & Trade_Date <= as.Date("2021-12-31")) %>%
  mutate(Period = "During COVID")

post_covid_data <- long_data %>%
  filter(Trade_Date >= as.Date("2022-01-01") & Trade_Date <= as.Date("2024-12-31")) %>%
  mutate(Period = "Post COVID")

# Combine the COVID and post-COVID datasets
combined_data <- bind_rows(during_covid_data, post_covid_data)

# Calculate mean price for each sector during COVID and post-COVID
sector_means <- combined_data %>%
  group_by(Sector, Period) %>%
  summarise(Mean_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Perform ANOVA to compare sector performance differences between sectors
anova_result <- aov(Mean_Price ~ Sector * Period, data = sector_means)
summary(anova_result)

# Perform Tukey's HSD post hoc test to pinpoint specific sector differences
tukey_result <- TukeyHSD(anova_result, which = "Sector")
print("Tukey's HSD Test Results:")
print(tukey_result)

# Visualization of sector means by period
ggplot(sector_means, aes(x = Sector, y = Mean_Price, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Average Sector Performance During and Post COVID",
    x = "Sector",
    y = "Mean Price"
  ) +
  scale_fill_manual(values = c("During COVID" = "lightblue", "Post COVID" = "orange")) +
  theme_minimal() +
  theme(legend.position = "bottom")
