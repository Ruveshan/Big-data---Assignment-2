# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(zoo)  # for interpolation

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the list of assets by sector
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")
indices <- c("S.P_500_Price", "Nasdaq_100_Price")
commodities <- c("Natural_Gas_Price", "Crude_oil_Price", "Gold_Price", 
                 "Silver_Price", "Platinum_Price", "Copper_Price")
cryptocurrencies <- c("Bitcoin_Price", "Ethereum_Price")

# Combine all asset columns for analysis
all_assets <- c(tech_stocks, indices, commodities, cryptocurrencies)

# Ensure all price columns are numeric, removing commas if present
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ as.numeric(gsub(",", "", .))))

# Interpolate missing values for each asset column
stock_data <- stock_data %>%
  mutate(across(all_of(all_assets), ~ na.approx(., rule = 2)))

# Define significant COVID-19 events with dates and descriptions
event_dates <- data.frame(
  Event = c("WHO declares COVID-19 a pandemic", "US Stimulus Bill", "Pfizer Vaccine Announcement", 
            "First US Lockdown", "First Vaccine Rollout"),
  Date = as.Date(c("2020-03-11", "2020-03-27", "2020-11-09", "2020-03-19", "2020-12-14"))
)

# Calculate daily returns for each asset and replace NAs with 0
returns_data <- stock_data %>%
  select(Date, all_of(all_assets)) %>%
  mutate(across(-Date, ~ c(NA, diff(log(.))), .names = "Return_{col}")) %>%
  mutate(across(starts_with("Return"), ~ replace_na(., 0)))  # Replace NA with 0 across all returns

# Function to calculate average returns around each event with an extended window
event_impact <- function(event_date, window = 10) {
  returns_data %>%
    filter(Date >= event_date - window & Date <= event_date + window) %>%
    summarise(across(starts_with("Return"), mean)) %>%
    mutate(Event_Date = event_date)
}

# Apply event impact calculation for each event
event_results <- bind_rows(lapply(event_dates$Date, event_impact, window = 10))

# Reshape data for plotting
event_results_long <- event_results %>%
  pivot_longer(cols = -Event_Date, names_to = "Asset", values_to = "Average_Return") %>%
  mutate(Sector = case_when(
    grepl("Apple|Microsoft|Nvidia|Google|Meta|Netflix|Amazon|Tesla", Asset) ~ "Technology",
    grepl("S.P_500|Nasdaq_100", Asset) ~ "Indices",
    grepl("Natural_Gas|Crude_oil|Gold|Silver|Platinum|Copper", Asset) ~ "Commodities",
    grepl("Bitcoin|Ethereum", Asset) ~ "Cryptocurrencies",
    TRUE ~ "Other"
  ))

# Replace "Return_" prefix for cleaner labeling
event_results_long$Asset <- gsub("Return_", "", event_results_long$Asset)

# Plot the impact of each event on sector performance
ggplot(event_results_long, aes(x = factor(Event_Date), y = Average_Return, fill = Sector)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Impact of COVID-19 Events on Sector Performance",
       x = "Event Date", y = "Average Return",
       fill = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = event_dates$Event)  # Add event descriptions as labels
