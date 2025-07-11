# ──────────────────────────────────────────────────────
#  Merge NSQIP PUF 2013-2023  →  tab-delimited TXT
#  safe for SPSS (≤64 char names, no illegal chars)
#  → output:  data-raw/nsqip_merged_2013_2023.txt
# ──────────────────────────────────────────────────────
library(readr)     # read_delim(), write_delim()
library(dplyr)     # mutate(), across(), bind_rows()
library(stringr)   # str_replace_all(), str_sub()
library(fs)        # dir_create()

puf_dir <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf"
files   <- list.files(puf_dir, pattern = "\\d{2}\\.txt$", full.names = TRUE)
files   <- files[order(files)]          # 13 … 23

#──────────────── helper 1 – make SPSS-legal column names ────────────────
clean_names <- function(nms) {
  nms <- toupper(trimws(nms))                  # canonical
  nms <- str_replace_all(nms, "[^A-Z0-9_]", "_") # keep letters/digits/_
  nms <- ifelse(substr(nms, 1, 1) %in% 0:9, paste0("X", nms), nms) # start w/ letter
  str_sub(nms, 1, 64)                         # max 64 chars
}

#──────────────── helper 2 – read one year, add YEAR col ────────────────
read_one <- function(path) {
  yr  <- 2000 + as.integer(str_extract(path, "\\d{2}(?=\\.txt$)"))
  df  <- read_delim(path, delim = "\t", show_col_types = FALSE,
                    col_types = cols(.default = "c"))
  names(df) <- clean_names(names(df))
  mutate(df, YEAR = as.character(yr))
}

#──────────────── 1st file establishes final column order ───────────────
message("▶ reading seed file: ", basename(files[1]))
main <- read_one(files[1])
col_order <- names(main)

# create output dir + write header immediately
dir_create("data-raw")
out_txt <- "data-raw/nsqip_merged_2013_2023.txt"
write_delim(main, out_txt, delim = "\t", na = "",
            quote_escape = "double")   # header + first block

#──────────────── append each remaining year one-by-one ─────────────────
for (f in files[-1]) {
  message("▶ appending ", basename(f))
  df <- read_one(f)
  
  ## 1) align to master column order
  missing_in_df   <- setdiff(col_order, names(df))
  missing_in_main <- setdiff(names(df),  col_order)
  
  if (length(missing_in_df))
    df[missing_in_df] <- NA_character_
  
  if (length(missing_in_main)) {
    # new vars (appeared in later PUF); add to col_order & back-fill main
    col_order <- c(col_order, missing_in_main)
  }
  
  df <- df[, col_order, drop = FALSE]
  
  ## 2) detect type clashes between df and header prototype
  clashes <- intersect(names(main), names(df))
  bad <- clashes[
    map_chr(main[clashes], class) !=
      map_chr(df  [clashes], class)
  ]
  if (length(bad)) {
    df  <- mutate(df,  across(all_of(bad), as.character))
  }
  
  ## 3) append to disk (no header, quote strings)
  write_delim(df, out_txt, delim = "\t", na = "",
              quote_escape = "double",
              append = TRUE, col_names = FALSE)
  
  rm(df); gc()
}

message("✅  Finished.  SPSS-ready tab file at: ", out_txt)
