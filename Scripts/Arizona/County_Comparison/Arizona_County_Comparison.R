# Load the libraries
library(haven)
library(tidyverse)
library(writexl)
library(lubridate)

# Define file paths using relative paths
arizona_dta_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta"
output_folder <- "Plots/Arizona/County_Comparison"
output_dta_path <- "Data Outputs/Arizona/County_Comparison/Arizona_SNAP_Summary.dta"
output_excel_path <- "Data Outputs/Arizona/County_Comparison/Arizona_SNAP_Summary.xlsx"

# Read the .dta file for Arizona
AZ <- read_dta(arizona_dta_path)

# Define the columns to track
columns_to_track <- c("HH_TOTAL", "IND_TOTAL", "IND_ADULT", "IND_CHILD", "ISS_TOTAL")

# Define treatment and control groups with the updated locality names for Arizona in title case
AZ <- AZ %>%
  mutate(
    TREATMENT_A = ifelse(LOCALITY == "Maricopa", 1, 0),
    CONTROL_A = ifelse(LOCALITY != "Maricopa", 1, 0)
  )

# Weight the data by population
AZ <- AZ %>%
  mutate(across(all_of(columns_to_track), ~ . / POPULATION, .names = "weighted_{col}"))

# Summarize the data by group, year, and month
summary_data <- AZ %>%
  pivot_longer(cols = starts_with("TREATMENT_") | starts_with("CONTROL_"), names_to = "GROUP", values_to = "flag") %>%
  filter(flag == 1) %>%
  group_by(GROUP, YEAR, MONTH) %>%
  summarize(across(starts_with("weighted_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Combine YEAR and MONTH into a single date column for better plotting
summary_data <- summary_data %>%
  mutate(Date = as.Date(paste(YEAR, MONTH, "01", sep = "-"), format = "%Y-%m-%d"))

# Function to create plots for each variable
create_plot <- function(data, variable) {
  y_label <- ifelse(grepl("ISS", variable), "Weighted Value (per person in localities)", "Weighted Value (percent of population for localities)")
  title <- case_when(
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in Arizona",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in Arizona",
    variable == "weighted_IND_ADULT" ~ "Trends for Adult Individuals (IND_ADULT) in Arizona",
    variable == "weighted_IND_CHILD" ~ "Trends for Child Individuals (IND_CHILD) in Arizona",
    variable == "weighted_ISS_TOTAL" ~ "Trends for Issuance Total (ISS_TOTAL) in Arizona",
    TRUE ~ paste("Trends for", variable, "in Arizona")
  )
  ggplot(data, aes(x = Date, y = !!sym(variable), color = GROUP)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2016-06-24"), linetype = "dashed", color = "red") +
    annotate("text", x = as.Date("2016-06-24"), y = Inf, label = "Event Date", vjust = 1.5, hjust = -0.1, color = "red") +
    labs(x = "Year-Month", y = y_label, title = title, color = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    annotate("text", x = as.Date("2000-01-01"), y = Inf, label = "TREATMENT_A: Maricopa\nCONTROL_A: All Other Localities", vjust = 1, hjust = 0, color = "black", size = 3)
}

# Plot for each variable
plots <- list()
variables <- c("weighted_HH_TOTAL", "weighted_IND_TOTAL", "weighted_IND_ADULT", "weighted_IND_CHILD", "weighted_ISS_TOTAL")

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

for (variable in variables) {
  plot <- create_plot(summary_data, variable)
  plots[[variable]] <- plot
  ggsave(filename = paste0(output_folder, "/county_comparison_", variable, "_Arizona_plot.png"), plot = plot, width = 10, height = 6)
}

# Save the summary data to .dta and .xlsx files
write_dta(summary_data, output_dta_path)
write_xlsx(summary_data, output_excel_path)

# Display plots (optional)
# plots$weighted_HH_TOTAL
# plots$weighted_IND_TOTAL
# plots$weighted_IND_ADULT
# plots$weighted_IND_CHILD
# plots$weighted_ISS_TOTAL
