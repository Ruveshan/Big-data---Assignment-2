# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the indices
indices <- c("S.P_500_Price", "Nasdaq_100_Price")

# Convert index price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(indices), ~ as.numeric(gsub(",", "", .))))

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the median price for each index during COVID and post-COVID
median_during_covid <- during_covid_data %>%
  summarise(across(all_of(indices), median, na.rm = TRUE))
median_post_covid <- post_covid_data %>%
  summarise(across(all_of(indices), median, na.rm = TRUE))

# Combine the medians into a single data frame for comparison
median_comparison <- data.frame(
  Index = c("S&P 500", "NASDAQ 100"),
  Median_During_COVID = as.numeric(median_during_covid),
  Median_Post_COVID = as.numeric(median_post_covid)
)

# Print the median comparison to the console
print("Median Daily Closing Prices for S&P 500 and NASDAQ 100 During and Post-COVID:")
print(median_comparison)

# Reshape data for visualization
median_comparison_long <- median_comparison %>%
  pivot_longer(cols = c(Median_During_COVID, Median_Post_COVID), 
               names_to = "Period", values_to = "Median_Price")

# Plot the comparison
ggplot(median_comparison_long, aes(x = Index, y = Median_Price, fill = Period)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  labs(
    title = "Median Daily Closing Prices for S&P 500 and NASDAQ 100 During and Post-COVID",
    x = "Stock Market Index",
    y = "Median Price (USD)"
  ) +
  scale_fill_manual(values = c("Median_During_COVID" = "lightblue", "Median_Post_COVID" = "orange"),
                    labels = c("During COVID (2020-2021)", "Post-COVID (2022-2024)")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
