# Load necessary libraries
library(dplyr)
library(ggplot2)
library(reshape2) # For reshaping data for plotting
library(corrplot) # For correlation plot visualization

# Load the dataset
stock_data <- read.csv("US_Stock_Data.csv")

# Convert 'Date' column to Date type
stock_data$Date <- as.Date(stock_data$Date, format = "%m/%d/%Y") # Adjust format as needed

# Convert prices to numeric, removing commas if needed
stock_data <- stock_data %>%
  mutate(across(ends_with("_Price"), ~ as.numeric(gsub(",", "", .))))

# Select only the Price columns for technology stocks
tech_stock_prices <- stock_data %>%
  select(Apple_Price, Microsoft_Price, Nvidia_Price, Google_Price, 
         Meta_Price, Netflix_Price, Amazon_Price, Tesla_Price)

# Calculate the correlation matrix
correlation_matrix <- cor(tech_stock_prices, use = "complete.obs")

# Plot the correlation matrix as a heatmap
corrplot::corrplot(correlation_matrix, method = "color", type = "upper",
                   tl.col = "black", tl.srt = 45, addCoef.col = "black",
                   title = "Correlation Matrix of Technology Sector Stocks",
                   mar = c(0, 0, 2, 0))
