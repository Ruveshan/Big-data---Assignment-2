# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Define the technology stocks
tech_stocks <- c("Apple_Price", "Microsoft_Price", "Nvidia_Price", "Google_Price",
                 "Meta_Price", "Netflix_Price", "Amazon_Price", "Tesla_Price")

# Convert technology stock price columns to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(tech_stocks), ~ as.numeric(gsub(",", "", .))))

# Calculate the daily mean price for the technology sector
stock_data <- stock_data %>%
  rowwise() %>%
  mutate(Tech_Sector_Mean = mean(c_across(all_of(tech_stocks)), na.rm = TRUE)) %>%
  ungroup()

# Filter the data for the COVID (2020-2021) and post-COVID (2022-2024) periods
during_covid_data <- stock_data %>%
  filter(Date >= as.Date("2020-01-01") & Date <= as.Date("2021-12-31"))

post_covid_data <- stock_data %>%
  filter(Date >= as.Date("2022-01-01") & Date <= as.Date("2024-12-31"))

# Calculate the standard deviation of the Tech_Sector_Mean during COVID and post-COVID
std_dev_tech_during_covid <- sd(during_covid_data$Tech_Sector_Mean, na.rm = TRUE)
std_dev_tech_post_covid <- sd(post_covid_data$Tech_Sector_Mean, na.rm = TRUE)

# Combine the results into a data frame for visualization
std_dev_tech_sector <- data.frame(
  Period = c("During COVID (2020-2021)", "Post-COVID (2022-2024)"),
  Standard_Deviation = c(std_dev_tech_during_covid, std_dev_tech_post_covid)
)

# Print the standard deviation values to the console
print("Standard Deviation of the Technology Sector During and Post-COVID:")
print(std_dev_tech_sector)

# Plot the standard deviation comparison
ggplot(std_dev_tech_sector, aes(x = Period, y = Standard_Deviation, fill = Period)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Standard Deviation of Technology Sector's Daily Mean Price During and Post-COVID",
    x = "Period",
    y = "Standard Deviation"
  ) +
  scale_fill_manual(values = c("During COVID (2020-2021)" = "lightblue", "Post-COVID (2022-2024)" = "orange")) +
  theme_minimal() +
  theme(legend.position = "none")
