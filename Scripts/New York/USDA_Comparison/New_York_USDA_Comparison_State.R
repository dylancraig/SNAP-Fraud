# Load necessary libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)
library(lubridate)

# Define file paths using relative paths
new_york_dta_path <- "Data Outputs/New York/County_Compiling/New_York_County_Data.dta"
new_york_usda_path <- "Raw Data/New York/USDA_Comparison/New_York_USDA_State.xlsx"
plot_output_path <- "Plots/New York/USDA_Comparison"
output_excel_path <- "Data Outputs/New York/USDA_Comparison/New_York_USDA_Comparison_State.xlsx"

# Read the .dta file and Excel file
New_York <- read_dta(new_york_dta_path)
New_York_USDA <- read_excel(new_york_usda_path)

# Aggregate county-level data to get state totals for each month and year
New_York_aggregated <- New_York %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    HH_TOTAL = sum(HH_TOTAL, na.rm = TRUE),
    IND_TOTAL = sum(IND_TOTAL, na.rm = TRUE),
    ISS_TOTAL = sum(ISS_TOTAL, na.rm = TRUE)
  )

New_York_USDA <- New_York_USDA %>%
  mutate(
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Join the aggregated county-level data with the state-level data
combined_data <- New_York_aggregated %>%
  inner_join(New_York_USDA, by = c("YEAR", "MONTH"))

# Calculate the ratios for each relevant variable
combined_data <- combined_data %>%
  mutate(
    HH_RATIO = HH_TOTAL.x / HH_TOTAL.y,
    IND_RATIO = IND_TOTAL.x / IND_TOTAL.y,
    ISS_RATIO = ISS_TOTAL.x / ISS_TOTAL.y
  )

# Create a new column for year-month
combined_data <- combined_data %>%
  mutate(YEAR_MONTH = ymd(paste(YEAR, MONTH, "01", sep = "-")))

# Reshape the data to long format for ggplot2
combined_data_long <- combined_data %>%
  pivot_longer(cols = c(HH_RATIO, IND_RATIO, ISS_RATIO), names_to = "Variable", values_to = "Ratio")

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

# Save the combined data to an Excel file
write_xlsx(combined_data, output_excel_path)
