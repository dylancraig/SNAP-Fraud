# Load the libraries
library(haven)
library(tidyverse)

# Define file paths using relative paths
nj_summary_dta_path <- "Data Outputs/New Jersey/County_Comparison/New_Jersey_SNAP_Summary.dta"
output_dir <- "Plots/New Jersey/Event_Study"

# Read the summary .dta file for New Jersey
NJ <- read_dta(nj_summary_dta_path)

# Ensure the Date column is of Date type
NJ$Date <- as.Date(NJ$Date)

# Define the base date
base_date <- as.Date("2015-12-01")

# Function to get the variable description
get_variable_description <- function(variable) {
  case_when(
    variable == "weighted_HH_TANF" ~ "Trends for Household TANF Recipient (HH_TANF) in New Jersey",
    variable == "weighted_HH_OTHER_POOR" ~ "Trends for Household Other Low Income (HH_OTHER_POOR) in New Jersey",
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in New Jersey",
    variable == "weighted_IND_ADULT" ~ "Trends for Adult Individuals (IND_ADULT) in New Jersey",
    variable == "weighted_IND_CHILD" ~ "Trends for Child Individuals (IND_CHILD) in New Jersey",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in New Jersey",
    TRUE ~ variable
  )
}

# Function to calculate differences and generate plots for a given group set
generate_plots <- function(df, treatment_group, control_group, variable, treatment_label, control_label) {
  # Filter for the specific treatment and control groups
  df_filtered <- df %>%
    filter(GROUP %in% c(treatment_group, control_group))
  
  # Calculate the difference at base_date
  base_diff <- df_filtered %>%
    filter(Date == base_date) %>%
    group_by(GROUP) %>%
    summarize(value = mean(get(variable), na.rm = TRUE)) %>%
    spread(GROUP, value) %>%
    mutate(diff = !!sym(treatment_group) - !!sym(control_group)) %>%
    pull(diff)
  
  # Calculate the difference for all dates and subtract the base difference
  df_diff <- df_filtered %>%
    group_by(Date) %>%
    summarize(
      !!treatment_group := mean(get(variable)[GROUP == treatment_group], na.rm = TRUE),
      !!control_group := mean(get(variable)[GROUP == control_group], na.rm = TRUE)
    ) %>%
    mutate(diff = (!!sym(treatment_group) - !!sym(control_group)) - base_diff)
  
  # Generate the plot
  p <- ggplot(df_diff, aes(x = Date, y = diff)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(base_date), linetype = "dashed", color = "red") +
    scale_y_continuous(limits = c(-0.05, 0.05)) +
    labs(title = get_variable_description(variable),
         x = "Date", y = "Difference") +
    annotate("text", x = as.Date("2014-01-01"), y = 0.045, label = paste("Treatment:", treatment_label), hjust = 0) +
    annotate("text", x = as.Date("2014-01-01"), y = 0.04, label = paste("Control:", control_label), hjust = 0) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA))
  
  return(p)
}

# List of weighted variables
weighted_vars <- c("weighted_HH_TANF", "weighted_HH_OTHER_POOR", "weighted_HH_TOTAL", 
                   "weighted_IND_ADULT", "weighted_IND_CHILD", "weighted_IND_TOTAL")

# Create output directory if it doesn't exist
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Generate and save plots for TREATMENT_A and CONTROL_A
plots_A <- lapply(weighted_vars, function(variable) {
  plot <- generate_plots(NJ, "TREATMENT_A", "CONTROL_A", variable, "OCEAN", "MONMOUTH, BURLINGTON, ATLANTIC")
  filename <- file.path(output_dir, paste0(variable, "_TREATMENT_A_vs_CONTROL_A_New_Jersey.png"))
  ggsave(filename = filename, plot = plot, width = 10, height = 6, dpi = 300, bg = "white")
  print(paste("Saved plot to:", filename))
  return(plot)
})

# Generate and save plots for TREATMENT_B and CONTROL_B
plots_B <- lapply(weighted_vars, function(variable) {
  plot <- generate_plots(NJ, "TREATMENT_B", "CONTROL_B", variable, "OCEAN, MONMOUTH, BURLINGTON, ATLANTIC", "all other localities besides OCEAN, MONMOUTH, BURLINGTON, and ATLANTIC")
  filename <- file.path(output_dir, paste0(variable, "_TREATMENT_B_vs_CONTROL_B_New_Jersey.png"))
  ggsave(filename = filename, plot = plot, width = 10, height = 6, dpi = 300, bg = "white")
  print(paste("Saved plot to:", filename))
  return(plot)
})
