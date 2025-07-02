# data-raw/diagnostics/compare_variable_names.R

library(tidyverse)
library(readr)
library(fs)

# ✅ Path to your raw NSQIP PUF files
puf_dir <- "F:/Research/UTMB/NSQIP/NSQIP_PUF/NSQIP_txt"  # <-- Update if needed

# 🔍 Function to extract headers and attach year
get_headers <- function(file_path) {
  ext <- path_ext(file_path)
  year2 <- str_extract(file_path, "\\d{2}(?=\\.txt$|\\.csv$)")
  year <- paste0("20", year2)
  
  if (ext == "txt") {
    df <- read_delim(file_path, delim = "\t", n_max = 0, show_col_types = FALSE)
  } else if (ext == "csv") {
    df <- read_csv(file_path, n_max = 0, show_col_types = FALSE)
  } else {
    stop("Unknown file format: ", file_path)
  }
  
  tibble(year = year, variable = toupper(names(df)))
}

# 🔃 Load headers from all files
files <- dir_ls(puf_dir, regexp = "\\.txt$|\\.csv$")
headers_long <- map_dfr(files, get_headers)

# 🧮 Add presence flag
headers_long <- headers_long %>%
  mutate(present = 1L)

# 📊 Pivot to wide matrix
headers_wide <- headers_long %>%
  distinct(variable, year, present) %>%
  pivot_wider(
    names_from = year,
    values_from = present,
    values_fill = list(present = 0)
  ) %>%
  arrange(variable)

# 📁 Ensure output folder exists
dir_create("data-raw/variable_maps")

# 💾 Write to CSV
write_csv(headers_wide, "data-raw/variable_maps/variable_presence_matrix.csv")
