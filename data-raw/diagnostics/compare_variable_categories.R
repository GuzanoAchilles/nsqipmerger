# data-raw/diagnostics/compare_variable_categories.R

library(tidyverse)
library(readr)
library(fs)

# Set your PUF directory
puf_dir <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf"

# List all NSQIP data files
files <- dir_ls(puf_dir, regexp = "\\.txt$|\\.csv$")

# Function to identify categorical columns and extract levels
extract_categorical_levels <- function(file_path) {
  year2 <- str_extract(file_path, "\\d{2}(?=\\.txt$|\\.csv$)")
  year <- paste0("20", year2)
  
  # Read first 1000 rows
  df <- read_delim(file_path, delim = "\t", n_max = 1000, show_col_types = FALSE)
  
  df <- df %>% rename_all(toupper)  # standardize var names
  
  out <- map_dfr(names(df), function(var) {
    values <- df[[var]]
    if (is.character(values) || is.factor(values)) {
      tibble(
        variable = var,
        year = year,
        level = unique(na.omit(values))
      )
    } else {
      NULL
    }
  })
  
  return(out)
}

# Extract all variable-year-level rows
all_levels <- map_dfr(files, extract_categorical_levels)

# Arrange neatly
all_levels <- all_levels %>%
  arrange(variable, year, level)

# Save output
dir_create("data-raw/variable_maps")
write_csv(all_levels, "data-raw/variable_maps/long_categorical_levels.csv")

message("✅ Finished extracting categorical variable levels.")
