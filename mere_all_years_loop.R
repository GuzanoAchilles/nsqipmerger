library(tidyverse)      # dplyr helpers
library(data.table)     # fread()
library(vroom)          # fast streaming write
library(fs)

# -------------------------------------------------------------------------
puf_dir <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf"
files   <- dir_ls(puf_dir, regexp = "\\.txt$|\\.csv$") %>% sort()
out_csv <- "data-raw/nsqip_merged_2013_2023.csv"

dir_create("data-raw")
if (file_exists(out_csv)) file_delete(out_csv)   # start fresh

# ---------- helper --------------------------------------------------------
read_year <- function(path) {
  yr <- as.integer(paste0("20", stringr::str_extract(path, "\\d{2}(?=\\.txt$|\\.csv$)")))
  dt <- fread(path, sep = "\t", data.table = FALSE, showProgress = FALSE)
  names(dt) <- toupper(trimws(names(dt)))
  dt$YEAR <- yr
  dt
}

safe_bind_two <- function(a, b) {
  common   <- intersect(names(a), names(b))
  clashes  <- common[
    map_chr(a[common], ~ class(.x)[1]) !=
      map_chr(b[common], ~ class(.x)[1])
  ]
  if (length(clashes)) {
    message("→ To character: ", paste(clashes, collapse = ", "))
    a <- mutate(a, across(all_of(clashes), as.character))
    b <- mutate(b, across(all_of(clashes), as.character))
  }
  bind_rows(a, b)
}

# ---------- streaming loop -----------------------------------------------
acc <- NULL  # small accumulator: never holds > 2 years

for (f in files) {
  message("Reading ", basename(f))
  df_y <- read_year(f)
  
  if (is.null(acc)) {
    acc <- df_y                          # first file
  } else {
    acc <- safe_bind_two(acc, df_y)      # merge with accumulator
  }
  
  # ----- append to CSV & free memory -----
  vroom_write(acc,
              path   = out_csv,
              delim  = ",",
              append = file_exists(out_csv))   # header only once
  
  acc <- NULL        # drop from RAM
  gc()               # encourage garbage collection
}

message("✅ Streaming merge finished: ", out_csv)
