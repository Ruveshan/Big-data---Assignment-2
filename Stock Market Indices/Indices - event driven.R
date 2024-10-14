# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define key event dates with descriptions
event_dates_df <- data.frame(
  Event_Date = as.Date(c("2020-03-11", "2020-03-23", "2020-04-03", "2020-11-09",
                         "2020-12-14", "2021-07-19", "2021-11-26", "2022-01-03")),
  Description = c("WHO declares COVID-19 a pandemic",
                  "U.S. announces major lockdowns",
                  "CDC recommends mask-wearing in public",
                  "Pfizer announces vaccine efficacy",
                  "First COVID-19 vaccine administered in U.S.",
                  "Delta variant causes renewed restrictions",
                  "WHO classifies Omicron as variant of concern",
                  "Omicron surge peaks in the U.S.")
)

# Convert prices to numeric and calculate daily returns
stock_data <- stock_data %>%
  mutate(SP500_Price = as.numeric(gsub(",", "", S.P_500_Price)),
         NASDAQ100_Price = as.numeric(gsub(",", "", Nasdaq_100_Price))) %>%
  filter(!is.na(SP500_Price) & !is.na(NASDAQ100_Price)) %>%
  mutate(SP500_Return = c(NA, diff(log(SP500_Price))),
         NASDAQ100_Return = c(NA, diff(log(NASDAQ100_Price))))

# Calculate volatility (standard deviation) of daily returns around each event date
volatility_analysis <- lapply(event_dates_df$Event_Date, function(date) {
  # Define a 15-day window around each event (7 days before and 7 days after)
  window_data <- stock_data %>%
    filter(Date >= (date - 7) & Date <= (date + 7))
  
  # Check if we have enough data; if not, assign NA
  if (nrow(window_data) >= 5) {
    data.frame(
      Event_Date = date,
      Index = c("S&P 500", "NASDAQ 100"),
      Volatility = c(sd(window_data$SP500_Return, na.rm = TRUE),
                     sd(window_data$NASDAQ100_Return, na.rm = TRUE))
    )
  } else {
    data.frame(
      Event_Date = date,
      Index = c("S&P 500", "NASDAQ 100"),
      Volatility = c(NA, NA)
    )
  }
})

# Combine results into a single data frame and filter out NA values
volatility_analysis_df <- bind_rows(volatility_analysis) %>%
  filter(!is.na(Volatility))

# Merge with event descriptions
volatility_analysis_df <- merge(volatility_analysis_df, event_dates_df, by = "Event_Date")

# Create custom x-axis labels with date and description
volatility_analysis_df$Event_Label <- paste0(volatility_analysis_df$Event_Date, "\n", 
                                             volatility_analysis_df$Description)

# Plot the volatility around each event for S&P 500 and NASDAQ 100 with descriptions below the date
ggplot(volatility_analysis_df, aes(x = Event_Label, y = Volatility, fill = Index)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Volatility of S&P 500 and NASDAQ 100 Around Key COVID-19 Events",
       x = "Event Date", y = "Standard Deviation of Returns (Volatility)") +
  theme_minimal() +
  scale_fill_manual(values = c("S&P 500" = "blue", "NASDAQ 100" = "red")) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
