# Load the libraries
library(haven)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
nj_dta_path <- "Data Outputs/New Jersey/County_Compiling/New_Jersey_County_Data.dta"
plot_output_dir <- "Plots/New Jersey/County_Comparison"
output_excel_path <- "Data Outputs/New Jersey/County_Comparison/New_Jersey_SNAP_Summary.xlsx"
output_dta_path <- "Data Outputs/New Jersey/County_Comparison/New_Jersey_SNAP_Summary.dta"

# Read the new .dta file for New Jersey
NJ <- read_dta(nj_dta_path)

# Define the columns to track
columns_to_track <- c("HH_TANF", "HH_OTHER_POOR", "HH_TOTAL", "IND_ADULT", "IND_CHILD", "IND_TOTAL")

# Define treatment and control groups with the updated locality names for New Jersey in UPPERCASE
NJ <- NJ %>%
  mutate(
    TREATMENT_A = ifelse(LOCALITY == "OCEAN", 1, 0),
    TREATMENT_B = ifelse(LOCALITY %in% c("OCEAN", "MONMOUTH", "BURLINGTON", "ATLANTIC"), 1, 0),
    CONTROL_A = ifelse(LOCALITY %in% c("MONMOUTH", "BURLINGTON", "ATLANTIC"), 1, 0),
    CONTROL_B = ifelse(!LOCALITY %in% c("OCEAN", "MONMOUTH", "BURLINGTON", "ATLANTIC"), 1, 0)
  )

# Weight the data by population
NJ <- NJ %>%
  mutate(across(all_of(columns_to_track), ~ . / POPULATION, .names = "weighted_{col}"))

# Summarize the data by group, year, and month
summary_data <- NJ %>%
  pivot_longer(cols = starts_with("TREATMENT_") | starts_with("CONTROL_"), names_to = "GROUP", values_to = "flag") %>%
  filter(flag == 1) %>%
  group_by(GROUP, YEAR, MONTH) %>%
  summarize(across(starts_with("weighted_"), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

# Combine YEAR and MONTH into a single date column for better plotting
summary_data <- summary_data %>%
  mutate(Date = as.Date(paste(YEAR, MONTH, "01", sep = "-"), format = "%Y-%m-%d"))

# Function to create plots for each variable
create_plot <- function(data, variable) {
  y_label <- ifelse(grepl("IND", variable), "Weighted Value (per person in localities)", "Weighted Value (percent of population for localities)")
  title <- case_when(
    variable == "weighted_HH_TANF" ~ "Trends for Household TANF Recipient (HH_TANF) in New Jersey",
    variable == "weighted_HH_OTHER_POOR" ~ "Trends for Household Other Low Income (HH_OTHER_POOR) in New Jersey",
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in New Jersey",
    variable == "weighted_IND_ADULT" ~ "Trends for Adult Individuals (IND_ADULT) in New Jersey",
    variable == "weighted_IND_CHILD" ~ "Trends for Child Individuals (IND_CHILD) in New Jersey",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in New Jersey",
    TRUE ~ paste("Trends for", variable, "in New Jersey")
  )
  ggplot(data, aes(x = Date, y = !!sym(variable), color = GROUP)) +
    geom_line() +
    geom_vline(xintercept = as.Date("2015-12-04"), linetype = "dashed", color = "red") +
    annotate("text", x = as.Date("2015-12-04"), y = Inf, label = "Event Date", vjust = 1.5, hjust = -0.1, color = "red") +
    labs(x = "Year-Month", y = y_label, title = title, color = "Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    annotate("text", x = as.Date("2000-01-01"), y = Inf, label = "TREATMENT_A: OCEAN\nTREATMENT_B: OCEAN, MONMOUTH, BURLINGTON, ATLANTIC\nCONTROL_A: MONMOUTH, BURLINGTON, ATLANTIC\nCONTROL_B: All Other Localities", vjust = 1, hjust = 0, color = "black", size = 3)
}

# Plot for each variable
plots <- list()
variables <- c("weighted_HH_TANF", "weighted_HH_OTHER_POOR", "weighted_HH_TOTAL", "weighted_IND_ADULT", "weighted_IND_CHILD", "weighted_IND_TOTAL")

# Create output directory if it doesn't exist
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

for (variable in variables) {
  plot <- create_plot(summary_data, variable)
  plots[[variable]] <- plot
  ggsave(filename = file.path(plot_output_dir, paste0("county_comparison_", variable, "_New_Jersey_plot.png")), plot = plot, width = 10, height = 6)
}

# Save the summary data as a .dta file and Excel file
write_dta(summary_data, output_dta_path)
write_xlsx(summary_data, output_excel_path)
