# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(haven)
library(tidyr)
library(writexl)

# Define the folder paths using relative paths
folder_path <- "Raw Data/New Jersey/County_Compiling"

# Get a list of all Excel files in the folder, excluding temporary files starting with ~$
file_list <- list.files(path = folder_path, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store dataframes
df_list <- list()

# Specify columns that should be numeric
numeric_columns <- c("HH_TANF", "HH_OTHER_POOR", "HH_TOTAL", "IND_ADULT", "IND_CHILD", "IND_TOTAL")

# Loop through each file and process it
for (file in file_list) {
  # Attempt to read the Excel file into a dataframe
  tryCatch({
    df <- read_excel(file)
    
    # Remove the IND_OLD column if it exists
    if ("IND_OLD" %in% colnames(df)) {
      df <- df %>% select(-IND_OLD)
    }
    
    # Convert specified columns to numeric after removing commas and handling coercion issues
    df <- df %>%
      mutate(across(all_of(numeric_columns), ~ as.numeric(gsub(",", "", as.character(.))), .names = "conv_{col}"))
    
    # Replace original columns with converted columns
    for (col in numeric_columns) {
      if (paste0("conv_", col) %in% colnames(df)) {
        df[[col]] <- df[[paste0("conv_", col)]]
        df[[paste0("conv_", col)]] <- NULL
      }
    }
    
    # Check if the "STATE" column is missing, and if so, add it with the value "NEW JERSEY"
    if (!"STATE" %in% colnames(df)) {
      df <- df %>% mutate(STATE = "NEW JERSEY")
    }
    
    # Add the dataframe to the list
    df_list[[file]] <- df
  }, error = function(e) {
    # Print a message if there's an error reading the file
    message(paste("Error reading file:", file, "\n", e))
  })
}

# Combine all dataframes into one
new_jersey <- bind_rows(df_list)

# Remove rows with any NA values
new_jersey <- new_jersey %>% drop_na()

# Sort the dataframe by LOCALITY, YEAR, and MONTH
new_jersey <- new_jersey %>%
  arrange(LOCALITY, YEAR, MONTH)

# Check for all unique instances of LOCALITY and how many times they appear
locality_counts <- new_jersey %>% 
  group_by(LOCALITY) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Check for all unique combinations of MONTH and YEAR and how many times they appear
month_year_counts <- new_jersey %>%
  group_by(MONTH, YEAR) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Find all instances where IND_ADULT plus IND_CHILD does not equal IND_TOTAL
discrepancies <- new_jersey %>%
  filter((IND_ADULT + IND_CHILD) != IND_TOTAL)

# Find all instances where HH_OTHER_POOR plus HH_TANF does not equal HH_TOTAL
discrepancies2 <- new_jersey %>%
  filter((HH_OTHER_POOR + HH_TANF) != HH_TOTAL)

# Save the combined dataframe to a .dta file
write_dta(new_jersey, "Data Outputs/New Jersey/County_Compiling/New_Jersey_County_Data.dta")

# Save the combined dataframe to an Excel file
write_xlsx(new_jersey, path = "Data Outputs/New Jersey/County_Compiling/New_Jersey_County_Data.xlsx")
