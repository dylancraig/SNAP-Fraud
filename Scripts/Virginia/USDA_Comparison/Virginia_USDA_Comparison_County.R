# Load necessary libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)
library(lubridate)

# Define file paths using relative paths
virginia_dta_path <- "Data Outputs/Virginia/County_Compiling/Virginia_County_Data.dta"
virginia_usda_path <- "Raw Data/Virginia/USDA_Comparison/Virginia_USDA_Locality.xlsx"
plot_output_path <- "Plots/Virginia/USDA_Comparison"
output_excel_path <- "Data Outputs/Virginia/USDA_Comparison/Virginia_USDA_Comparison_By_Locality.xlsx"

# Read the .dta file and Excel file
Virginia <- read_dta(virginia_dta_path)
Virginia_USDA <- read_excel(virginia_usda_path)

# Remove commas and convert relevant columns to numeric
Virginia <- Virginia %>%
  mutate(
    HH_TOTAL = as.numeric(gsub(",", "", HH_TOTAL)),
    IND_TOTAL = as.numeric(gsub(",", "", IND_TOTAL)),
    ISS_TOTAL = as.numeric(gsub(",", "", ISS_TOTAL)),
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Aggregate county-level data to get locality totals for each month and year
Virginia_aggregated <- Virginia %>%
  group_by(LOCALITY, YEAR, MONTH) %>%
  summarise(
    HH_TOTAL = sum(HH_TOTAL, na.rm = TRUE),
    IND_TOTAL = sum(IND_TOTAL, na.rm = TRUE),
    ISS_TOTAL = sum(ISS_TOTAL, na.rm = TRUE)
  )

Virginia_USDA <- Virginia_USDA %>%
  mutate(
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH)
  )

# Join the aggregated locality-level data with the locality-level USDA data
combined_data <- Virginia_aggregated %>%
  inner_join(Virginia_USDA, by = c("LOCALITY", "YEAR", "MONTH"))

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
  labs(title = "Ratios Over Time by Locality",
       x = "Year-Month",
       y = "Ratio",
       color = "Variable") +
  theme_minimal()

# Save the plot as .png
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_By_Locality.png"), plot = plot1, width = 8, height = 6)

# Save the plot as .pdf
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_By_Locality.pdf"), plot = plot1, width = 8, height = 6)

# Save the combined data to an Excel file
write_xlsx(combined_data, output_excel_path)
