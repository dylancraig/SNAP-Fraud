# Load necessary libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
arizona_dta_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta"
arizona_usda_path <- "Raw Data/Arizona/USDA_Comparison/Arizona_USDA_County.xlsx"
plot_output_path <- "Plots/Arizona/USDA_Comparison"
output_excel_path <- "Data Outputs/Arizona/USDA_Comparison/Arizona_USDA_Comparison_County.xlsx"

# Read the .dta file and Excel file
Arizona <- read_dta(arizona_dta_path)
Arizona_USDA <- read_excel(arizona_usda_path)

# Join the locality-level data with the USDA data by YEAR, MONTH, and LOCALITY
combined_data <- Arizona %>%
  inner_join(Arizona_USDA, by = c("YEAR", "MONTH", "LOCALITY"))

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

# Count the number of times each locality has a ratio less than 0.9 or greater than 1.1
ratio_counts_by_locality <- combined_data %>%
  group_by(LOCALITY) %>%
  summarise(
    HH_RATIO_COUNT = sum(HH_RATIO < 0.9 | HH_RATIO > 1.1, na.rm = TRUE),
    IND_RATIO_COUNT = sum(IND_RATIO < 0.9 | IND_RATIO > 1.1, na.rm = TRUE),
    ISS_RATIO_COUNT = sum(ISS_RATIO < 0.9 | ISS_RATIO > 1.1, na.rm = TRUE)
  )

# Save the ratio counts dataframe to an Excel file
write_xlsx(ratio_counts_by_locality, "Data Outputs/Arizona/USDA_Comparison/Arizona_USDA_Ratio_Counts.xlsx")

# Count the number of times each locality has a ratio less than 0.8 or greater than 1.2
ratio_counts_by_locality2 <- combined_data %>%
  group_by(LOCALITY) %>%
  summarise(
    HH_RATIO_COUNT = sum(HH_RATIO < 0.8 | HH_RATIO > 1.2, na.rm = TRUE),
    IND_RATIO_COUNT = sum(IND_RATIO < 0.8 | IND_RATIO > 1.2, na.rm = TRUE),
    ISS_RATIO_COUNT = sum(ISS_RATIO < 0.8 | ISS_RATIO > 1.2, na.rm = TRUE)
  )

# Reshape the data to long format for ggplot2
combined_data_long <- combined_data %>%
  pivot_longer(cols = c(HH_RATIO, IND_RATIO, ISS_RATIO), names_to = "Variable", values_to = "Ratio")

# Plot the ratios over time and save the plot as .png and .pdf
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

# Create the plot with y limit set to 2 and save the plot as .png and .pdf
plot2 <- ggplot(combined_data_long, aes(x = YEAR_MONTH, y = Ratio, color = Variable)) +
  geom_line() +
  labs(title = "Ratios Over Time by Locality (Y-Limit 2)",
       x = "Year-Month",
       y = "Ratio",
       color = "Variable") +
  ylim(0, 2) +
  theme_minimal()

# Save the plot as .png
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_By_Locality_YLimit_2.png"), plot = plot2, width = 8, height = 6)

# Save the plot as .pdf
ggsave(filename = file.path(plot_output_path, "Ratios_Over_Time_By_Locality_YLimit_2.pdf"), plot = plot2, width = 8, height = 6)

# Save the combined data to an Excel file
write_xlsx(combined_data, output_excel_path)
