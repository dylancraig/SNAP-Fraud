# Load the libraries
library(haven)
library(readxl)
library(tidyverse)
library(writexl)

# Define file paths using relative paths
arizona_dta_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta"
population_path <- "Raw Data/Arizona/State_Population/Arizona_Population.xlsx"
output_dta_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta"
output_excel_path <- "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.xlsx"

# Read the .dta file for Arizona
AZ <- read_dta(arizona_dta_path)

# Read the Excel file for Arizona population data
population <- read_excel(population_path)

# Reshape the population data from wide to long format
population <- population %>%
  pivot_longer(cols = -c(STATE, LOCALITY), 
               names_to = "YEAR", 
               values_to = "POPULATION")

# Convert the YEAR column to numeric in the population data
population <- population %>%
  mutate(YEAR = as.numeric(YEAR))

# Merge the datasets based on STATE, LOCALITY, and YEAR
merged_data <- left_join(AZ, population, by = c("STATE", "LOCALITY", "YEAR"))

# Save the merged data back to a .dta file, overwriting the original file
write_dta(merged_data, output_dta_path)

# Save the merged data as an Excel file
write_xlsx(merged_data, output_excel_path)
