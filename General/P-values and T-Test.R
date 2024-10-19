# Display the test results in the console
print("Paired T-Test and Mann-Whitney U Test Results for Sector Performance Changes:")
print(test_results_df)

# Load necessary library for visualization
library(ggplot2)
library(tidyr)

# Transform data for visualization in heatmap format
test_results_long <- test_results_df %>%
  pivot_longer(cols = c("T_Test_p_value", "Mann_Whitney_p_value"),
               names_to = "Test_Type", values_to = "P_Value")

# Add a significance level column for color mapping
test_results_long <- test_results_long %>%
  mutate(Significance = ifelse(P_Value < 0.05, "Significant", "Not Significant"))

# Plotting heatmap for p-values by Asset and Sector
ggplot(test_results_long, aes(x = Asset, y = Test_Type, fill = P_Value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "red", na.value = "grey50",
                      name = "P-Value", limits = c(0, 1)) +
  geom_text(aes(label = round(P_Value, 3)), color = "black", size = 3) +
  facet_wrap(~ Sector, scales = "free_x", nrow = 2) +
  labs(title = "P-Values for T-Test and Mann-Whitney U Test by Sector and Asset",
       x = "Asset", y = "Test Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
