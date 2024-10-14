# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(c("S.P_500_Price", "Nasdaq_100_Price"), ~ as.numeric(gsub(",", "", .))))

# Calculate daily returns for S&P 500 and NASDAQ 100
stock_data <- stock_data %>%
  mutate(SP500_Return = c(NA, diff(log(`S.P_500_Price`))),
         NASDAQ100_Return = c(NA, diff(log(Nasdaq_100_Price))))

# Filter data into during-COVID and post-COVID periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate volatility (standard deviation of returns) for each period
volatility_metrics <- data.frame(
  Index = c("S&P 500", "NASDAQ 100"),
  During_COVID_SD = c(sd(during_covid_data$SP500_Return, na.rm = TRUE),
                      sd(during_covid_data$NASDAQ100_Return, na.rm = TRUE)),
  Post_COVID_SD = c(sd(post_covid_data$SP500_Return, na.rm = TRUE),
                    sd(post_covid_data$NASDAQ100_Return, na.rm = TRUE))
)

# Print volatility metrics for comparison
print("Volatility (Standard Deviation of Returns) During and Post-COVID:")
print(volatility_metrics)

# Calculate cumulative returns for visualization
stock_data <- stock_data %>%
  mutate(SP500_Cumulative_Return = cumsum(coalesce(SP500_Return, 0)),
         NASDAQ100_Cumulative_Return = cumsum(coalesce(NASDAQ100_Return, 0)))

# Plot cumulative returns
cumulative_returns <- stock_data %>%
  select(Date, SP500_Cumulative_Return, NASDAQ100_Cumulative_Return) %>%
  pivot_longer(cols = -Date, names_to = "Index", values_to = "Cumulative_Return")

ggplot(cumulative_returns, aes(x = Date, y = Cumulative_Return, color = Index)) +
  geom_line() +
  labs(title = "Cumulative Returns of S&P 500 and NASDAQ 100",
       x = "Date", y = "Cumulative Return") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Plot volatility comparison between during-COVID and post-COVID periods
volatility_long <- volatility_metrics %>%
  pivot_longer(cols = -Index, names_to = "Period", values_to = "Volatility") %>%
  mutate(Period = ifelse(Period == "During_COVID_SD", "During COVID", "Post COVID"))

ggplot(volatility_long, aes(x = Index, y = Volatility, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Volatility of S&P 500 and NASDAQ 100 During and Post-COVID",
       x = "Index", y = "Volatility (Standard Deviation of Returns)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
