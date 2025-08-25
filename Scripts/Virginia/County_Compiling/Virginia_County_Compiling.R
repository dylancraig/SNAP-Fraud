# Load necessary libraries
library(tidyverse)
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(haven)  # For write_dta

# List all Excel files in the specified directory using relative paths
file_list <- list.files(path = "Raw Data/Virginia/County_Compiling", pattern = "*.xlsx", full.names = TRUE)

# Define a function to read and clean each Excel file
read_and_clean <- function(file_path) {
  tryCatch({
    # Read the Excel file
    df <- read_excel(file_path)
    
    # Ensure all columns are of consistent data types
    df <- df %>%
      mutate(across(everything(), as.character))
    
    return(df)
  }, error = function(e) {
    # Handle the error by returning NULL
    message(paste("Error in file:", file_path))
    return(NULL)
  })
}

# Read and combine all data from the Excel files
virginia_data <- file_list %>%
  map_dfr(read_and_clean)

# Remove rows where 'MONTH' is NA
virginia_data <- virginia_data[!is.na(virginia_data$MONTH), ]

# Remove rows where 'LOCALITY' is 'MULTI-FIPS'
virginia_data <- virginia_data %>%
  filter(LOCALITY != "MULTI-FIPS AGENCIES:")

# Named vector to map month names to their numerical values
month_mapping <- c(
  "JANUARY" = 1, "FEBRUARY" = 2, "MARCH" = 3, "APRIL" = 4,
  "MAY" = 5, "JUNE" = 6, "JULY" = 7, "AUGUST" = 8,
  "SEPTEMBER" = 9, "OCTOBER" = 10, "NOVEMBER" = 11, "DECEMBER" = 12
)

# Specify columns that should be numeric
numeric_columns <- c("HH_PA", "HH_NPA", "HH_TOTAL", "IND_PA", "IND_NPA", "IND_TOTAL", "ISS_PA", "ISS_NPA", "ISS_TOTAL", "FIPS")

# Standardize the 'LOCALITY' names by converting them to uppercase and ensure FIPS is 3 digits long
virginia_data <- virginia_data %>%
  mutate(
    LOCALITY = str_to_upper(LOCALITY),  # Convert to uppercase
    LOCALITY = str_replace_all(LOCALITY, "CITY|COUNTY", "\\0 "),  # Add space after CITY or COUNTY
    LOCALITY = str_replace_all(LOCALITY, "\\s+", " "),  # Replace multiple spaces with a single space
    LOCALITY = str_trim(LOCALITY),  # Trim leading and trailing spaces
    MONTH = recode(MONTH, !!!month_mapping),  # Convert month names to numbers
    FIPS = str_pad(FIPS, width = 3, side = "left", pad = "0")  # Ensure FIPS is 3 digits long
  )

# Convert specified columns to numeric after removing commas
virginia_data <- virginia_data %>%
  mutate(across(all_of(numeric_columns), ~ as.numeric(gsub(",", "", .))))

# Add a new column 'STATE' with the value 'VIRGINIA' for every row
virginia_data <- virginia_data %>%
  mutate(STATE = "VIRGINIA")

# Sort the dataframe by FIPS, YEAR, and MONTH
virginia_data <- virginia_data %>%
  arrange(FIPS, YEAR, MONTH)

# Check for all unique combinations of MONTH and YEAR
unique_combinations <- virginia_data %>%
  distinct(MONTH, YEAR) %>%
  arrange(YEAR, MONTH)

# Create a dataframe for each unique value of LOCALITY and count occurrences
locality_counts <- virginia_data %>%
  group_by(LOCALITY) %>%
  summarise(Count = n())

# Create a dataframe with any row that has at least one missing value
missing_values_data <- virginia_data %>%
  filter(if_any(everything(), is.na))

# Create a dataframe with rows where HH_PA plus HH_NPA does not equal HH_TOTAL
mismatch_hh_data <- virginia_data %>%
  filter((HH_PA + HH_NPA) != HH_TOTAL)

# Create a dataframe with rows where IND_PA plus IND_NPA does not equal IND_TOTAL
mismatch_ind_data <- virginia_data %>%
  filter((IND_PA + IND_NPA) != IND_TOTAL)

# Create a dataframe with rows where ISS_PA plus ISS_NPA does not equal ISS_TOTAL
mismatch_iss_data <- virginia_data %>%
  filter((ISS_PA + ISS_NPA) != ISS_TOTAL)

fips_counts <- virginia_data %>%
  group_by(FIPS) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Optional: Sort by count in descending order

month_year_counts <- virginia_data %>%
  group_by(MONTH, YEAR) %>%
  summarise(count = n()) %>%
  arrange(desc(count))  # Optional: Sort by count in descending order

locality_fips_counts <- virginia_data %>%
  group_by(LOCALITY, FIPS) %>%
  summarise(count = n())

## Bring in FIPS code databook
fips_data <- read_excel("Raw Data/Virginia/FIPS/Virginia_FIPS.xlsx")

virginia_merge <- merge(virginia_data, fips_data, by = "FIPS")

# Replace LOCALITY with LOCALITY_DATABOOK and rename it to LOCALITY
virginia_merge <- virginia_merge %>%
  select(-LOCALITY) %>%
  rename(LOCALITY = LOCALITY_DATABOOK)

# Count the number of times each unique instance of LOCALITY appears
locality_counts_merge <- virginia_merge %>%
  group_by(LOCALITY) %>%
  summarise(count = n())

unique_month_year <- virginia_merge %>%
  filter(LOCALITY == "WINCHESTER CITY") %>%
  select(MONTH, YEAR) %>%
  distinct()

# Create a dataframe with rows where HH_PA plus HH_NPA does not equal HH_TOTAL
mismatch_hh_data <- virginia_merge %>%
  filter((HH_PA + HH_NPA) != HH_TOTAL)

# Create a dataframe with rows where IND_PA plus IND_NPA does not equal IND_TOTAL
mismatch_ind_data <- virginia_merge %>%
  filter((IND_PA + IND_NPA) != IND_TOTAL)

# Create a dataframe with rows where ISS_PA plus ISS_NPA does not equal ISS_TOTAL
mismatch_iss_data <- virginia_merge %>%
  filter((ISS_PA + ISS_NPA) != ISS_TOTAL)

# View the structure of the virginia_merge dataframe to see the data types of each column
str(virginia_merge)
glimpse(virginia_merge)

# Save as a .dta file
write_dta(virginia_merge, "Data Outputs/Virginia/County_Compiling/Virginia_County_Data.dta")

# Save as an Excel file
write_xlsx(virginia_merge, path = "Data Outputs/Virginia/County_Compiling/Virginia_County_Data.xlsx")
