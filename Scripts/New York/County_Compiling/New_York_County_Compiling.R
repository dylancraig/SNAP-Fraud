# Load necessary libraries
library(readxl)
library(dplyr)
library(haven)
library(writexl)

# Function to get the list of Excel files in a directory
get_excel_files <- function(directory) {
  list.files(directory, pattern = "*.xlsx", full.names = TRUE)
}

# Function to read and combine all Excel files into one dataframe
combine_excel_files <- function(directory) {
  files <- get_excel_files(directory)
  combined_data <- lapply(files, read_excel)
  combined_data <- bind_rows(combined_data)
  return(combined_data)
}

# Function to convert specified columns to numeric
convert_to_numeric <- function(df, columns) {
  df <- df %>%
    mutate(across(all_of(columns), ~ as.numeric(gsub(",", "", .))))
  return(df)
}

# Specify the directory containing the Excel files using relative paths
directory <- "Raw Data/New York/County_Compiling"

# Combine all Excel files into one dataframe and name it newyork
newyork <- combine_excel_files(directory)

# Modify LOCALITY values from "St.Lawrence" to "St. Lawrence"
newyork <- newyork %>%
  mutate(LOCALITY = ifelse(LOCALITY == "St.Lawrence", "St. Lawrence", LOCALITY))

# Specify the columns to be converted to numeric
columns_to_convert <- c("HH_TOTAL", "IND_TOTAL", "ISS_TOTAL", 
                        "HH_TA", "IND_TA", "ISS_TA", 
                        "HH_NTA", "IND_NTA", "ISS_NTA")

# Convert specified columns to numeric
newyork <- convert_to_numeric(newyork, columns_to_convert)

# Display all unique instances of the LOCALITY column along with their counts
unique_localities_counts <- newyork %>%
  group_by(LOCALITY) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print("Unique LOCALITY instances and their counts:")
print(unique_localities_counts)

# Identify rows with missing data
missing_data <- newyork %>%
  filter(is.na(HH_TOTAL) | is.na(IND_TOTAL) | is.na(ISS_TOTAL) | 
           is.na(HH_TA) | is.na(IND_TA) | is.na(ISS_TA) | 
           is.na(HH_NTA) | is.na(IND_NTA) | is.na(ISS_NTA) | 
           is.na(LOCALITY) | is.na(MONTH) | is.na(YEAR))

# Add a STATE column with every value as "New York"
newyork <- newyork %>%
  mutate(STATE = "New York")

# Organize the data by LOCALITY, then YEAR, then MONTH
newyork <- newyork %>%
  arrange(LOCALITY, YEAR, MONTH)

# Identify instances where HH_TOTAL does not equal HH_TA plus HH_NTA
incorrect_hh_totals <- newyork %>%
  filter(HH_TOTAL != HH_TA + HH_NTA)

# Identify instances where IND_TOTAL does not equal IND_TA plus IND_NTA
incorrect_ind_totals <- newyork %>%
  filter(IND_TOTAL != IND_TA + IND_NTA)

# Identify instances where ISS_TOTAL does not equal ISS_TA plus ISS_NTA
incorrect_iss_totals <- newyork %>%
  filter(ISS_TOTAL != ISS_TA + ISS_NTA)

# Identify all unique combinations of MONTH and YEAR and their counts
unique_month_year_counts <- newyork %>%
  group_by(MONTH, YEAR) %>%
  summarise(count = n()) %>%
  arrange(YEAR, MONTH)

# Save the newyork dataframe as a .dta file
write_dta(newyork, "Data Outputs/New York/County_Compiling/New_York_County_Data.dta")

# Save the newyork dataframe as an Excel file
write_xlsx(newyork, path = "Data Outputs/New York/County_Compiling/New_York_County_Data.xlsx")
