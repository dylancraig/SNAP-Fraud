# Load necessary libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
arizona_dta_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta"
arizona_usda_path <- "Raw Data/Arizona/USDA_Comparison/Arizona_USDA_State.xlsx"
plot_output_path <- "Plots/Arizona/USDA_Comparison"
output_excel_path <- "Data Outputs/Arizona/USDA_Comparison/Arizona_USDA_Comparison_State.xlsx"

# Read the .dta file and Excel file
Arizona <- read_dta(arizona_dta_path)
Arizona_USDA <- read_excel(arizona_usda_path)

# Remove commas and convert relevant columns to numeric
Arizona <- Arizona %>%
  mutate(
    HH_TOTAL = as.numeric(gsub(",", "", HH_TOTAL)),
    IND_TOTAL = as.numeric(gsub(",", "", IND_TOTAL)),
    ISS_TOTAL = as.numeric(gsub(",", "", ISS_TOTAL)),
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Aggregate county-level data to get state totals for each month and year
Arizona_aggregated <- Arizona %>%
  group_by(YEAR, MONTH) %>%
  summarise(
    HH_TOTAL = sum(HH_TOTAL, na.rm = TRUE),
    IND_TOTAL = sum(IND_TOTAL, na.rm = TRUE),
    ISS_TOTAL = sum(ISS_TOTAL, na.rm = TRUE)
  )

Arizona_USDA <- Arizona_USDA %>%
  mutate(
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Join the aggregated county-level data with the state-level data
combined_data <- Arizona_aggregated %>%
  inner_join(Arizona_USDA, by = c("YEAR", "MONTH"))

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

# Create the plot with y limit set to 2 and save the plot as .png and .pdf
plot2 <- ggplot(combined_data_long, aes(x = YEAR_MONTH, y = Ratio, color = Variable)) +
  geom_line() +
  labs(title = "Ratios Over Time (Y-Limit 2)",
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
