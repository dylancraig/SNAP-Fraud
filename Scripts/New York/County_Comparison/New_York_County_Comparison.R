# Load the libraries
library(haven)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
new_york_dta_path <- "Data Outputs/New York/County_Compiling/New_York_County_Data.dta"
plot_output_dir <- "Plots/New York/County_Comparison"
output_summary_dta_path <- "Data Outputs/New York/County_Comparison/New_York_SNAP_Summary_Comparison.dta"
output_summary_excel_path <- "Data Outputs/New York/County_Comparison/New_York_SNAP_Summary_Comparison.xlsx"

# Read the new .dta file for New York
NY <- read_dta(new_york_dta_path)

# Define the columns to track
columns_to_track <- c("HH_TOTAL", "IND_TOTAL", "ISS_TOTAL", "HH_TA", "ISS_TA", "HH_NTA", "IND_NTA", "ISS_NTA")

# Define treatment and control groups with the updated locality names for New York in title case
NY <- NY %>%
  mutate(
    TREATMENT_A = ifelse(LOCALITY == "Orange", 1, 0),
    TREATMENT_B = ifelse(LOCALITY %in% c("Orange", "Ulster", "Dutchess", "Putnam", "Rockland"), 1, 0),
    CONTROL_A = ifelse(LOCALITY %in% c("Ulster", "Dutchess", "Putnam", "Rockland"), 1, 0),
    CONTROL_B = ifelse(!LOCALITY %in% c("Orange", "Ulster", "Dutchess", "Putnam", "Rockland"), 1, 0)
  )

# Weight the data by population
NY <- NY %>%
  mutate(across(all_of(columns_to_track), ~ . / POPULATION, .names = "weighted_{col}"))

# Summarize the data by group, year, and month
summary_data <- NY %>%
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
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in New York",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in New York",
    variable == "weighted_ISS_TOTAL" ~ "Trends for Issuance Total (ISS_TOTAL) in New York",
    variable == "weighted_HH_TA" ~ "Trends for Household Temporary Assistance (HH_TA) in New York",
    variable == "weighted_ISS_TA" ~ "Trends for Issuance Temporary Assistance (ISS_TA) in New York",
    variable == "weighted_HH_NTA" ~ "Trends for Household Non-Temporary Assistance (HH_NTA) in New York",
    variable == "weighted_IND_NTA" ~ "Trends for Individual Non-Temporary Assistance (IND_NTA) in New York",
    variable == "weighted_ISS_NTA" ~ "Trends for Issuance Non-Temporary Assistance (ISS_NTA) in New York",
    TRUE ~ paste("Trends for", variable, "in New York")
  )
  ggplot(data, aes(x = Date, y = !!sym(variable), color = GROUP)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2016-06-29"), linetype = "dashed", color = "red") +
    annotate("text", x = as.Date("2016-06-29"), y = Inf, label = "Event Date", vjust = 1.5, hjust = -0.1, color = "red") +
    labs(x = "Year-Month", y = y_label, title = title, color = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    annotate("text", x = as.Date("2000-01-01"), y = Inf, label = "TREATMENT_A: Orange\nTREATMENT_B: Orange, Ulster, Dutchess, Putnam, Rockland\nCONTROL_A: Ulster, Dutchess, Putnam, Rockland\nCONTROL_B: All Other Localities", vjust = 1, hjust = 0, color = "black", size = 3)
}

# Create output directory if it doesn't exist
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# Plot for each variable
plots <- list()
variables <- c("weighted_HH_TOTAL", "weighted_IND_TOTAL", "weighted_ISS_TOTAL", "weighted_HH_TA", "weighted_ISS_TA", "weighted_HH_NTA", "weighted_IND_NTA", "weighted_ISS_NTA")

for (variable in variables) {
  plot <- create_plot(summary_data, variable)
  plots[[variable]] <- plot
  ggsave(filename = file.path(plot_output_dir, paste0("county_comparison_", variable, "_New_York_plot.png")), plot = plot, width = 10, height = 6)
}

# Save the summary data as a .dta file
write_dta(summary_data, output_summary_dta_path)

# Save the summary data as an Excel file
write_xlsx(summary_data, output_summary_excel_path)
