library(tidyverse)
library(readr)

safe_merge_two_years <- function(path1, path2, year1, year2) {
  read_and_clean <- function(path, year) {
    df <- read_delim(path, delim = "\t", show_col_types = FALSE)
    names(df) <- toupper(trimws(names(df)))   # <- canonical names
    df <- df %>% mutate(year = year)
    df
  }
  
  d1 <- read_and_clean(path1, year1)
  d2 <- read_and_clean(path2, year2)
  
  # --- fix only the true type conflicts ---
  common <- intersect(names(d1), names(d2))
  bad    <- common[map_chr(d1[common], ~class(.x)[1]) !=
                     map_chr(d2[common], ~class(.x)[1])]
  
  if (length(bad)) {
    message("Converting conflicting vars to character: ", paste(bad, collapse = ", "))
    d1 <- mutate(d1, across(all_of(bad), as.character))
    d2 <- mutate(d2, across(all_of(bad), as.character))
  }
  
  bind_rows(d1, d2)
}

# usage --------------------------------------------------------
file_2013 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf13.txt"
file_2014 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf14.txt"

merged_13_14 <- safe_merge_two_years(file_2013, file_2014, 2013, 2014)
ncol(merged_13_14)   # should be 327 – the full set


library(tidyverse)
library(readr)
library(fs)

# -----------------------------------------------------------
# 1.  Safe two-year merge -----------------------------------
# -----------------------------------------------------------
safe_merge_two_years <- function(path1, path2, year1, year2) {
  read_and_clean <- function(path, year) {
    df <- read_delim(path, delim = "\t", show_col_types = FALSE)
    names(df) <- toupper(trimws(names(df)))      # canonical names
    df %>% mutate(year = year)
  }
  
  d1 <- read_and_clean(path1, year1)
  d2 <- read_and_clean(path2, year2)
  
  # ---- detect real type clashes ----
  common <- intersect(names(d1), names(d2))
  clashes <- common[
    map_chr(d1[common], ~class(.x)[1]) !=
      map_chr(d2[common], ~class(.x)[1])
  ]
  if (length(clashes)) {
    message("Converting to character: ", paste(clashes, collapse = ", "))
    d1 <- mutate(d1, across(all_of(clashes), as.character))
    d2 <- mutate(d2, across(all_of(clashes), as.character))
  }
  
  # ---- OPTIONAL: uncomment to mimic SPSS and drop vars not in both ----
  # keep_vars <- common
  # d1 <- select(d1, all_of(keep_vars))
  # d2 <- select(d2, all_of(keep_vars))
  
  bind_rows(d1, d2)
}

# -----------------------------------------------------------
# 2.  Run for 2013 + 2014 -----------------------------------
# -----------------------------------------------------------
file_2013 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf13.txt"
file_2014 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf14.txt"

merged_13_14 <- safe_merge_two_years(file_2013, file_2014, 2013, 2014)

# -----------------------------------------------------------
# 3.  Export results ----------------------------------------
# -----------------------------------------------------------
# create folder if it doesn't exist
dir_create("data-raw")

# CSV export
csv_path <- "data-raw/nsqip_merged_2013_2015.csv"
write_csv(merged_13_14, csv_path)
message("✅ CSV saved to: ", csv_path)


