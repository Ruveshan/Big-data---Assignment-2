# Load necessary libraries
library(dplyr)

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

# Define COVID and post-COVID periods
covid_period <- stock_data %>% filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))
post_covid_period <- stock_data %>% filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Function to calculate volatility (standard deviation) for each sector
calculate_volatility <- function(data) {
  sapply(sectors, function(stocks) {
    sector_data <- data %>% select(all_of(stocks))
    sd(unlist(sector_data), na.rm = TRUE)
  })
}

# Calculate volatility during COVID and post-COVID
volatility_during_covid <- calculate_volatility(covid_period)
volatility_post_covid <- calculate_volatility(post_covid_period)

# Determine high and low volatility based on median standard deviation threshold
volatility_threshold <- median(c(volatility_during_covid, volatility_post_covid))

volatility_categories <- data.frame(
  Sector = rep(names(sectors), 2),
  Period = c(rep("During COVID", length(sectors)), rep("Post COVID", length(sectors))),
  Volatility = c(volatility_during_covid, volatility_post_covid),
  Volatility_Category = ifelse(
    c(volatility_during_covid, volatility_post_covid) > volatility_threshold, "High Volatility", "Low Volatility"
  )
)

# Perform Chi-Square Test
chi_square_table <- table(volatility_categories$Sector, volatility_categories$Volatility_Category)
chi_square_result <- chisq.test(chi_square_table)

# Print results
print("Volatility Categories Across Sectors During and Post COVID:")
print(volatility_categories)
print("Chi-Square Test Results for Sector Volatility:")
print(chi_square_result)

# Visualization of volatility categories by sector and period
library(ggplot2)

ggplot(volatility_categories, aes(x = Sector, fill = Volatility_Category)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Period) +
  labs(title = "Volatility Categories by Sector During and Post COVID",
       x = "Sector", y = "Count", fill = "Volatility Category") +
  theme_minimal()
