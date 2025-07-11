# ── libraries ───────────────────────────────────────────────────────────────
library(tidyverse)   # dplyr + readr + purrr
library(fs)          # dir_create()

# ── 1)  paths you edit just once ────────────────────────────────────────────
old_csv <- "data-raw/nsqip_merged_2013_2014.csv"               # current merged
new_puf <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf15.txt"  # next year
new_year <- 2015
out_csv <- "data-raw/nsqip_merged_2013_2014_2015.csv"

# ── 2) read-&-clean helper (no pipes inside) ───────────────────────────────
read_and_clean <- function(path, yr) {
  df <- read_delim(path, delim = "\t", show_col_types = FALSE)
  names(df) <- toupper(trimws(names(df)))
  df$YEAR <- yr
  df
}

# ── 3) safe bind that fixes only real type clashes ─────────────────────────
safe_bind <- function(a, b) {
  common   <- intersect(names(a), names(b))
  clashes  <- common[ map_chr(a[common], ~ class(.x)[1]) !=
                        map_chr(b[common], ~ class(.x)[1]) ]
  if (length(clashes)) {
    message("Converting to character: ", paste(clashes, collapse = ", "))
    a <- mutate(a, across(all_of(clashes), as.character))
    b <- mutate(b, across(all_of(clashes), as.character))
  }
  bind_rows(a, b)
}

# ── 4) merge the new year in  ──────────────────────────────────────────────
message("▶ reading existing merged CSV …")
merged_so_far <- read_csv(old_csv, show_col_types = FALSE)

message("▶ reading ", basename(new_puf), " …")
year_df <- read_and_clean(new_puf, new_year)

message("▶ merging …")
merged_new <- safe_bind(merged_so_far, year_df)

# ── 5) write updated CSV  (+ RDS for R users) ──────────────────────────────
dir_create("data-raw")                 # harmless if already exists

write_csv(merged_new, out_csv)
write_rds(merged_new, sub("\\.csv$", ".rds", out_csv))

message("✅  wrote ", out_csv)
