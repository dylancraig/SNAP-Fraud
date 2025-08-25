# Load necessary libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)
library(lubridate)

# Define file paths using relative paths
new_jersey_dta_path <- "Data Outputs/New Jersey/County_Compiling/New_Jersey_County_Data.dta"
new_jersey_usda_path <- "Raw Data/New Jersey/USDA_Comparison/New_Jersey_USDA_State.xlsx"
plot_output_path <- "Plots/New Jersey/USDA_Comparison"
output_excel_path <- "Data Outputs/New Jersey/USDA_Comparison/New_Jersey_USDA_Comparison_State.xlsx"

# Read the .dta file and Excel file
New_Jersey <- read_dta(new_jersey_dta_path)
New_Jersey_USDA <- read_excel(new_jersey_usda_path)

# Aggregate county-level data to get state totals for each month and year
New_Jersey_aggregated <- New_Jersey %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    HH_TOTAL = sum(HH_TOTAL, na.rm = TRUE),
    IND_TOTAL = sum(IND_TOTAL, na.rm = TRUE)
  )

New_Jersey_USDA <- New_Jersey_USDA %>%
  mutate(
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Join the aggregated county-level data with the state-level data
combined_data <- New_Jersey_aggregated %>%
  inner_join(New_Jersey_USDA, by = c("YEAR", "MONTH"))

# Calculate the ratios for each relevant variable
combined_data <- combined_data %>%
  mutate(
    HH_RATIO = HH_TOTAL.x / HH_TOTAL.y,
    IND_RATIO = IND_TOTAL.x / IND_TOTAL.y
  )

# Create a new column for year-month
combined_data <- combined_data %>%
  mutate(YEAR_MONTH = ymd(paste(YEAR, MONTH, "01", sep = "-")))

# Reshape the data to long format for ggplot2
combined_data_long <- combined_data %>%
  pivot_longer(cols = c(HH_RATIO, IND_RATIO), names_to = "Variable", values_to = "Ratio")

# Create the plot with default y limit and save as .png and .pdf
plot1 <- ggplot(combined_data_long, aes(x = YEAR_MONTH, y = Ratio, color = Variable)) +
  geom_line() +
  labs(title = "Ratios Over Time",
       x = "Year-Month",
       y = "Ratio",
       color = "Variable") +
  theme_minimal()

# Save the plot as .png
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time.png"), plot = plot1, width = 8, height = 6)

# Save the plot as .pdf
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time.pdf"), plot = plot1, width = 8, height = 6)

# Create the plot with y limit set to 2 and save the plot as .png and .pdf
plot2 <- ggplot(combined_data_long, aes(x = YEAR_MONTH, y = Ratio, color = Variable)) +
  geom_line() +
  labs(title = "Ratios Over Time (Y limit = 2)",
       x = "Year-Month",
       y = "Ratio",
       color = "Variable") +
  ylim(0, 2) +
  theme_minimal()

# Save the plot as .png
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_YLimit_2.png"), plot = plot2, width = 8, height = 6)

# Save the plot as .pdf
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_YLimit_2.pdf"), plot = plot2, width = 8, height = 6)

# Save the combined data to an Excel file
write_xlsx(combined_data, output_excel_path)
