# Load necessary libraries
library(dplyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format as needed

# Filter data into during-COVID and after-COVID periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

after_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# List of technology stock columns
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", 
                 "Google_Price", "Meta_Price", "Netflix_Price", 
                 "Amazon_Price", "Tesla_Price")

# Calculate statistics for each stock in both periods
summary_stats <- data.frame(Stock = tech_stocks, 
                            During_COVID_Mean = NA, During_COVID_SD = NA, 
                            After_COVID_Mean = NA, After_COVID_SD = NA,
                            Change_in_Price = NA)

for (stock in tech_stocks) {
  # During-COVID statistics
  during_mean <- mean(during_covid_data[[stock]], na.rm = TRUE)
  during_sd <- sd(during_covid_data[[stock]], na.rm = TRUE)
  
  # After-COVID statistics
  after_mean <- mean(after_covid_data[[stock]], na.rm = TRUE)
  after_sd <- sd(after_covid_data[[stock]], na.rm = TRUE)
  
  # Price change (percentage) from the end of during-COVID to end of after-COVID period
  during_covid_end <- tail(during_covid_data[[stock]], 1)
  after_covid_end <- tail(after_covid_data[[stock]], 1)
  price_change <- ((after_covid_end - during_covid_end) / during_covid_end) * 100
  
  # Store results in summary_stats
  summary_stats[summary_stats$Stock == stock, ] <- c(stock, during_mean, during_sd, after_mean, after_sd, price_change)
}

# Display summary statistics
print(summary_stats)
