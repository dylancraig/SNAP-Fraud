# Load the libraries
library(haven)
library(tidyverse)
library(lubridate)

# Define file paths using relative paths
alabama_dta_summary_path <- "Data Outputs/Alabama/County_Comparison/Alabama_SNAP_Summary.dta"
output_dir <- "Plots/Alabama/Event_Study"

# Read the summary .dta file for Alabama
AL <- read_dta(alabama_dta_summary_path)

# Ensure the Date column is of Date type
AL$Date <- as.Date(AL$Date)

# Define the base date
base_date <- as.Date("2016-09-01")

# Function to get the variable description
get_variable_description <- function(variable) {
  case_when(
    variable == "weighted_HH_TOTAL" ~ "Trends for Household Total (HH_TOTAL) in Alabama",
    variable == "weighted_IND_PA" ~ "Trends for Individual Public Assistance (IND_PA) in Alabama",
    variable == "weighted_IND_NPA" ~ "Trends for Individual Non-Public Assistance (IND_NPA) in Alabama",
    variable == "weighted_IND_TOTAL" ~ "Trends for Individual Total (IND_TOTAL) in Alabama",
    variable == "weighted_ISS_TOTAL" ~ "Trends for Issuance Total (ISS_TOTAL) in Alabama",
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
  
  # Determine y-axis limits and annotation positions
  if (grepl("ISS", variable)) {
    y_limits <- c(-10, 10)
    annotation_y_treatment <- 8
    annotation_y_control <- 6
  } else {
    y_limits <- c(-0.05, 0.05)
    annotation_y_treatment <- y_limits[2] - 0.01
    annotation_y_control <- y_limits[2] - 0.02
  }
  
  # Generate the plot
  p <- ggplot(df_diff, aes(x = Date, y = diff)) +
    geom_line() +
    geom_vline(xintercept = as.numeric(base_date), linetype = "dashed", color = "red") +
    scale_y_continuous(limits = y_limits) +
    labs(title = get_variable_description(variable),
         x = "Date", y = "Difference") +
    annotate("text", x = as.Date("2005-01-01"), y = annotation_y_treatment, label = paste("Treatment:", treatment_label), hjust = 0) +
    annotate("text", x = as.Date("2005-01-01"), y = annotation_y_control, label = paste("Control:", control_label), hjust = 0) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA))
  
  return(p)
}

# List of weighted variables for Alabama
weighted_vars <- c("weighted_HH_TOTAL", "weighted_IND_PA", "weighted_IND_NPA", 
                   "weighted_IND_TOTAL", "weighted_ISS_TOTAL")

# Generate and save plots for TREATMENT_A and CONTROL_A
dir.create(output_dir, showWarnings = FALSE)

plots_A <- lapply(weighted_vars, function(variable) {
  plot <- generate_plots(AL, "TREATMENT_A", "CONTROL_A", variable, "Morgan", "Limestone, Madison, Marshall, Cullman, Lawrence")
  filename <- file.path(output_dir, paste0(variable, "_TREATMENT_A_vs_CONTROL_A_Alabama.png"))
  ggsave(filename = filename, plot = plot, width = 10, height = 6, dpi = 300, bg = "white")
  print(paste("Saved plot to:", filename))
  return(plot)
})

# Generate and save plots for TREATMENT_B and CONTROL_B
plots_B <- lapply(weighted_vars, function(variable) {
  plot <- generate_plots(AL, "TREATMENT_B", "CONTROL_B", variable, "Morgan, Limestone, Madison, Marshall, Cullman, Lawrence", "all other localities besides Morgan, Limestone, Madison, Marshall, Cullman, Lawrence")
  filename <- file.path(output_dir, paste0(variable, "_TREATMENT_B_vs_CONTROL_B_Alabama.png"))
  ggsave(filename = filename, plot = plot, width = 10, height = 6, dpi = 300, bg = "white")
  print(paste("Saved plot to:", filename))
  return(plot)
})
