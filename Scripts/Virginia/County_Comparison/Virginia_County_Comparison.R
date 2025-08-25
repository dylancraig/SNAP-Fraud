# Load the libraries
library(haven)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
virginia_dta_path <- "Data Outputs/Virginia/County_Compiling/Virginia_County_Data.dta"
plot_output_dir <- "Plots/Virginia/County_Comparison"
output_summary_dta_path <- "Data Outputs/Virginia/County_Comparison/Virginia_SNAP_Summary_Comparison.dta"
output_summary_excel_path <- "Data Outputs/Virginia/County_Comparison/Virginia_SNAP_Summary_Comparison.xlsx"

# Read the new .dta file for Virginia
VA <- read_dta(virginia_dta_path)

# Define the columns to track
columns_to_track <- c("HH_PA", "HH_NPA", "HH_TOTAL", "IND_PA", "IND_NPA", "IND_TOTAL", "ISS_PA", "ISS_NPA", "ISS_TOTAL")

# Define treatment and control groups
VA <- VA %>%
  mutate(
    TREATMENT_A = ifelse(LOCALITY == "LEE COUNTY", 1, 0),
    TREATMENT_B = ifelse(LOCALITY %in% c("LEE COUNTY", "SCOTT COUNTY", "WISE COUNTY", "NORTON CITY"), 1, 0),
    CONTROL_A = ifelse(LOCALITY %in% c("SCOTT COUNTY", "WISE COUNTY", "NORTON CITY"), 1, 0),
    CONTROL_B = ifelse(!LOCALITY %in% c("LEE COUNTY", "SCOTT COUNTY", "WISE COUNTY", "NORTON CITY"), 1, 0)
  )

# Weight the data by population
VA <- VA %>%
  mutate(across(all_of(columns_to_track), ~ . / POPULATION, .names = "weighted_{col}"))

# Summarize the data by group, year, and month
summary_data <- VA %>%
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
    variable == "weighted_HH_PA" ~ "Trends for Household Public Assistance (HH_PA) in Virginia",
    variable == "weighted_HH_NPA" ~ "Trends for Household Non-Public Assistance (HH_NPA) in Virginia",
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in Virginia",
    variable == "weighted_IND_PA" ~ "Trends for Individual Public Assistance (IND_PA) in Virginia",
    variable == "weighted_IND_NPA" ~ "Trends for Individual Non-Public Assistance (IND_NPA) in Virginia",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in Virginia",
    variable == "weighted_ISS_PA" ~ "Trends for Issuance Public Assistance (ISS_PA) in Virginia",
    variable == "weighted_ISS_NPA" ~ "Trends for Issuance Non-Public Assistance (ISS_NPA) in Virginia",
    variable == "weighted_ISS_TOTAL" ~ "Trends for Issuance Total (ISS_TOTAL) in Virginia",
    TRUE ~ paste("Trends for", variable, "in Virginia")
  )
  ggplot(data, aes(x = Date, y = !!sym(variable), color = GROUP)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2014-04-06"), linetype = "dashed", color = "red") +
    annotate("text", x = as.Date("2014-04-06"), y = Inf, label = "Event Date", vjust = 1.5, hjust = -0.1, color = "red") +
    labs(x = "Year-Month", y = y_label, title = title, color = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    annotate("text", x = as.Date("2000-01-01"), y = Inf, label = "TREATMENT_A: LEE COUNTY\nTREATMENT_B: LEE, SCOTT, WISE COUNTIES, NORTON CITY\nCONTROL_A: SCOTT, WISE COUNTIES, NORTON CITY\nCONTROL_B: All Other Localities", vjust = 1, hjust = 0, color = "black", size = 3)
}

# Create output directory if it doesn't exist
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# List of variables to plot
variables <- c("weighted_HH_PA", "weighted_HH_NPA", "weighted_HH_TOTAL", "weighted_IND_PA", 
               "weighted_IND_NPA", "weighted_IND_TOTAL", "weighted_ISS_PA", "weighted_ISS_NPA", "weighted_ISS_TOTAL")

# Generate and save plots
plots <- lapply(variables, function(variable) {
  plot <- create_plot(summary_data, variable)
  filename <- file.path(plot_output_dir, paste0("county_comparison_", variable, "_Virginia_plot.png"))
  ggsave(filename = filename, plot = plot, width = 10, height = 6)
  return(plot)
})

# Save the summary data as a .dta file
write_dta(summary_data, output_summary_dta_path)

# Save the summary data as an Excel file
write_xlsx(summary_data, output_summary_excel_path)
