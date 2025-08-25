# Load necessary libraries
library(readxl)
library(dplyr)
library(openxlsx)
library(haven)
library(writexl)

# Define the folder paths using relative paths
folder_path <- "Raw Data/Arizona/County_Compiling"

# Get a list of all Excel files in the folder, excluding temporary files starting with ~$
file_list <- list.files(path = folder_path, pattern = "^[^~$].*\\.xlsx$", full.names = TRUE)

# Initialize an empty list to store dataframes
df_list <- list()

# Loop through each file and process it
for (file in file_list) {
  # Attempt to read the Excel file into a dataframe
  tryCatch({
    df <- read_excel(file)
    
    # Check if the "STATE" column is missing, and if so, add it with the value "ARIZONA"
    if (!"STATE" %in% colnames(df)) {
      df <- df %>% mutate(STATE = "ARIZONA")
    }
    
    # Add the dataframe to the list
    df_list[[file]] <- df
  }, error = function(e) {
    # Print a message if there's an error reading the file
    message(paste("Error reading file:", file, "\n", e))
  })
}

# Combine all dataframes into one
combined_df <- bind_rows(df_list)

# Specify columns that should be numeric
numeric_columns <- c("HH_TOTAL", "IND_TOTAL", "IND_ADULT", "IND_CHILD", "ISS_TOTAL")

# Ensure the specified columns exist in the dataframe
existing_numeric_columns <- intersect(numeric_columns, colnames(combined_df))

# Convert specified columns to numeric after removing commas
combined_df <- combined_df %>%
  mutate(across(all_of(existing_numeric_columns), ~ as.numeric(gsub(",", "", .))))

# Sort the dataframe by LOCALITY, YEAR, and MONTH
combined_df <- combined_df %>%
  arrange(LOCALITY, YEAR, MONTH)

# Check for all unique instances of LOCALITY and how many times they appear
locality_counts <- combined_df %>% 
  group_by(LOCALITY) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

# Check for all unique combinations of MONTH and YEAR and how many times they appear
month_year_counts <- combined_df %>%
  group_by(MONTH, YEAR) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Find all instances where IND_ADULT plus IND_CHILD does not equal IND_TOTAL
discrepancies <- combined_df %>%
  filter((IND_ADULT + IND_CHILD) != IND_TOTAL)

# View the structure of the combined dataframe to see the data types of each column
str(combined_df)
glimpse(combined_df)

# Save the combined dataframe to a .dta file
write_dta(combined_df, "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.dta")

# Save the combined dataframe to an Excel file
write_xlsx(combined_df, path = "Data Outputs/Arizona/County_Compiling/Arizona_County_Data.xlsx")
