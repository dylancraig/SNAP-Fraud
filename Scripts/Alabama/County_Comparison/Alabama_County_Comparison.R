# Load the libraries
library(haven)
library(tidyverse)
library(writexl)
library(lubridate)

# Define file paths using relative paths
alabama_dta_path <- "Data Outputs/Alabama/County_Compiling/Alabama_County_Data.dta"
output_folder <- "Plots/Alabama/County_Comparison"
output_dta_path <- "Data Outputs/Alabama/County_Comparison/Alabama_SNAP_Summary.dta"
output_excel_path <- "Data Outputs/Alabama/County_Comparison/Alabama_SNAP_Summary.xlsx"

# Read the .dta file for Alabama
AL <- read_dta(alabama_dta_path)

# Define the columns to track
columns_to_track <- c("HH_TOTAL", "IND_PA", "IND_NPA", "IND_TOTAL", "ISS_TOTAL")

# Define treatment and control groups with normal locality names
AL <- AL %>%
  mutate(
    TREATMENT_A = ifelse(LOCALITY == "Morgan", 1, 0),
    TREATMENT_B = ifelse(LOCALITY %in% c("Morgan", "Limestone", "Madison", "Marshall", "Cullman", "Lawrence"), 1, 0),
    CONTROL_A = ifelse(LOCALITY %in% c("Limestone", "Madison", "Marshall", "Cullman", "Lawrence"), 1, 0),
    CONTROL_B = ifelse(!LOCALITY %in% c("Morgan", "Limestone", "Madison", "Marshall", "Cullman", "Lawrence"), 1, 0)
  )

# Weight the data by population
AL <- AL %>%
  mutate(across(all_of(columns_to_track), ~ . / POPULATION, .names = "weighted_{col}"))

# Summarize the data by group, year, and month
summary_data <- AL %>%
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
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in Alabama",
    variable == "weighted_IND_PA" ~ "Trends for Public Assistance Individuals (IND_PA) in Alabama",
    variable == "weighted_IND_NPA" ~ "Trends for Non-Public Assistance Individuals (IND_NPA) in Alabama",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in Alabama",
    variable == "weighted_ISS_TOTAL" ~ "Trends for Issuance Total (ISS_TOTAL) in Alabama",
    TRUE ~ paste("Trends for", variable, "in Alabama")
  )
  ggplot(data, aes(x = Date, y = !!sym(variable), color = GROUP)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2016-09-21"), linetype = "dashed", color = "red") +
    annotate("text", x = as.Date("2016-09-21"), y = Inf, label = "Event Date", vjust = 1.5, hjust = -0.1, color = "red") +
    labs(x = "Year-Month", y = y_label, title = title, color = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    annotate("text", x = as.Date("2000-01-01"), y = Inf, label = "TREATMENT_A: Morgan\nTREATMENT_B: Morgan, Limestone, Madison, Marshall, Cullman, Lawrence\nCONTROL_A: Limestone, Madison, Marshall, Cullman, Lawrence\nCONTROL_B: All Other Localities", vjust = 1, hjust = 0, color = "black", size = 3)
}

# Plot for each variable
plots <- list()
variables <- c("weighted_HH_TOTAL", "weighted_IND_PA", "weighted_IND_NPA", "weighted_IND_TOTAL", "weighted_ISS_TOTAL")

# Create output directory if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

for (variable in variables) {
  plot <- create_plot(summary_data, variable)
  plots[[variable]] <- plot
  ggsave(filename = paste0(output_folder, "/county_comparison_", variable, "_Alabama_plot.png"), plot = plot, width = 10, height = 6)
}

# Save the summary data to .dta and .xlsx files
write_dta(summary_data, output_dta_path)
write_xlsx(summary_data, output_excel_path)

# Display plots (optional)
# plots$weighted_HH_TOTAL
# plots$weighted_IND_PA
# plots$weighted_IND_NPA
# plots$weighted_IND_TOTAL
# plots$weighted_ISS_TOTAL
