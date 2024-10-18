# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the commodities prices
commodities_prices <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                        "Silver_Price", "Platinum_Price", "Copper_Price")

# Convert commodity price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(commodities_prices), ~ as.numeric(gsub(",", "", .))))

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the standard deviation for the commodities sector during COVID and post-COVID
std_dev_during_covid <- during_covid_data %>%
  summarise(across(all_of(commodities_prices), sd, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

std_dev_post_covid <- post_covid_data %>%
  summarise(across(all_of(commodities_prices), sd, na.rm = TRUE)) %>%
  rowMeans(na.rm = TRUE)

# Combine the results into a data frame for visualization
std_dev_commodities <- data.frame(
  Period = c("During COVID (2020-2021)", "Post-COVID (2022-2024)"),
  Standard_Deviation = c(std_dev_during_covid, std_dev_post_covid)
)

# Print the standard deviation values to the console
print("Standard Deviation of Commodities Sector Prices During and Post-COVID:")
print(std_dev_commodities)

# Plot the standard deviation comparison
ggplot(std_dev_commodities, aes(x = Period, y = Standard_Deviation, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Std Deviation of Commodities Sector Prices During and Post-COVID",
    x = "Period",
    y = "Standard Deviation"
  ) +
  scale_fill_manual(values = c("During COVID (2020-2021)" = "lightblue", "Post-COVID (2022-2024)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")
