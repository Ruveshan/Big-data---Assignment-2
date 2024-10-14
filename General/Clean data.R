# Load necessary libraries
library(dplyr)
library(tidyr)

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Preview the data to understand its structure
print(head(stock_data))
print(str(stock_data))

# Step 1: Remove any irrelevant columns
# For example, remove columns that contain all NAs or are unrelated to the analysis
stock_data <- stock_data %>% select(where(~ !all(is.na(.))))

# Step 2: Convert date column to Date format
# Assume the date column is named "Date" - adjust if necessary
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y")

# Step 3: Convert price columns to numeric
# Define the list of columns that should be numeric (price or volume columns)
price_columns <- grep("_Price$|_Vol\\.$", names(stock_data), value = TRUE)

# Remove commas and convert to numeric
stock_data <- stock_data %>%
  mutate(across(all_of(price_columns), ~ as.numeric(gsub(",", "", .))))

# Step 4: Handle missing values
# Options include removing rows, filling NAs with 0, or imputing with mean/median.
# Here, we replace NA with the previous value (forward fill), or with 0 for analysis
stock_data <- stock_data %>%
  fill(everything(), .direction = "down") %>%
  mutate(across(all_of(price_columns), ~ replace_na(., 0)))

# Step 5: Remove duplicate rows
stock_data <- stock_data %>% distinct()

# Step 6: Check for any remaining NA values and print summary
na_summary <- sapply(stock_data, function(x) sum(is.na(x)))
print(na_summary)

# Print cleaned data summary
print(summary(stock_data))
