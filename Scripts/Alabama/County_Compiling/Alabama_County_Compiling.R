# Load necessary libraries
library(readxl)
library(dplyr)
library(stringr)
library(haven)  # For saving as .dta
library(ggplot2)
library(lubridate)
library(writexl)

# Define the folder paths using relative paths
folder_path <- "Raw Data/Alabama/County_Compiling"

# List all .xlsx files in the folder, excluding temporary files starting with '~$'
file_list <- list.files(path = folder_path, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Function to standardize MONTH column
standardize_month <- function(month) {
  month <- toupper(month)
  if (grepl("^[0-9]+$", month)) {
    return(as.integer(month))
  } else {
    month <- match(month, toupper(month.name))
    return(month)
  }
}

# Initialize an empty dataframe
main_df <- data.frame()

# Loop through each file and read data
for (file in file_list) {
  df <- read_excel(file)
  df <- df %>% mutate(across(everything(), as.character))
  df$MONTH <- sapply(df$MONTH, standardize_month)
  df$STATE[is.na(df$STATE)] <- "ALABAMA"
  main_df <- bind_rows(main_df, df)
}

# Remove rows where YEAR or MONTH is NA
main_df <- main_df %>%
  filter(!is.na(YEAR) & !is.na(MONTH))

# Remove rows where LOCALITY equals AESAP
main_df <- main_df %>%
  filter(LOCALITY != "AESAP")

# Convert specified columns to numeric after removing non-numeric characters
main_df <- main_df %>%
  mutate(
    YEAR = as.numeric(YEAR),
    MONTH = as.numeric(MONTH),
    HH_TOTAL = as.numeric(gsub("[^0-9.-]", "", HH_TOTAL)),
    IND_PA = as.numeric(gsub("[^0-9.-]", "", IND_PA)),
    IND_NPA = as.numeric(gsub("[^0-9.-]", "", IND_NPA)),
    IND_TOTAL = as.numeric(gsub("[^0-9.-]", "", IND_TOTAL)),
    ISS_TOTAL = as.numeric(gsub("[^0-9.-]", "", ISS_TOTAL))
  )

# Sort the dataframe by LOCALITY, YEAR, and MONTH
main_df <- main_df %>%
  arrange(LOCALITY, YEAR, MONTH)

# View the structure of the combined dataframe to see the data types of each column
str(main_df)
glimpse(main_df)

# Count occurrences of each unique combination of YEAR and MONTH
unique_combinations_count <- main_df %>%
  group_by(YEAR, MONTH) %>%
  summarise(count = n()) %>%
  arrange(YEAR, MONTH)

# Display all unique values of LOCALITY
unique_localities <- unique(main_df$LOCALITY)

# Find all rows with at least one missing value
rows_with_missing_values <- main_df %>%
  filter(if_any(everything(), is.na))

# Find rows where IND_PA + IND_NPA does not equal IND_TOTAL
rows_with_incorrect_total <- main_df %>%
  filter((IND_PA + IND_NPA) != IND_TOTAL)

# Count occurrences of each unique LOCALITY
locality_counts <- main_df %>%
  group_by(LOCALITY) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Find MONTH-YEAR combinations where a LOCALITY appears more than once
duplicate_localities <- main_df %>%
  group_by(YEAR, MONTH, LOCALITY) %>%
  filter(n() > 1) %>%
  arrange(YEAR, MONTH, LOCALITY)

# Find MONTH-YEAR combinations where the LOCALITY 'Tuscaloosa' doesn't appear
all_combinations <- main_df %>%
  select(YEAR, MONTH) %>%
  distinct() %>%
  arrange(YEAR, MONTH)

# Save the main dataframe as a .dta file
write_dta(main_df, "Data Outputs/Alabama/County_Compiling/Alabama_County_Data.dta")

# Save the main dataframe as an Excel file
write_xlsx(main_df, path = "Data Outputs/Alabama/County_Compiling/Alabama_County_Data.xlsx")
