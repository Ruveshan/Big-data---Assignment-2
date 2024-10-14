# Load necessary libraries
library(dplyr)
library(ggplot2)
library(corrplot)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format if needed

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))))

# Filter data into during-COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# List of commodity price columns
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")

# Extract only commodity prices for each period
during_covid_commodities <- during_covid_data %>% select(all_of(commodities))
post_covid_commodities <- post_covid_data %>% select(all_of(commodities))

# Calculate correlation matrices
during_covid_corr <- cor(during_covid_commodities, use = "complete.obs")
post_covid_corr <- cor(post_covid_commodities, use = "complete.obs")

# Display correlation matrices
print("During COVID Correlation Matrix:")
print(during_covid_corr)

print("Post COVID Correlation Matrix:")
print(post_covid_corr)

# Plot correlation matrices for visual comparison
# During COVID correlation plot
corrplot(during_covid_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Commodity Price Correlations During COVID-19 (2020-2021)")

# Post COVID correlation plot
corrplot(post_covid_corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         title = "Commodity Price Correlations Post COVID-19 (2022-2024)")
