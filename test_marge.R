library(readr)
library(tidyverse)
library(glue)
library(fs)

# ── EDIT each run ───────────────────────────────────────────
old_merge <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/nsqip_merged_2013_2015.txt"
new_puf   <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf16.txt"
new_year  <- 2016
out_file  <- glue("data-raw/nsqip_merged_2013_{new_year}.txt")

file_copy(old_merge, out_file, overwrite = TRUE)

# template column order
template_names <- names(read_delim(out_file, n_max = 0, show_col_types = FALSE))

clean_names <- function(nms) {
  nms %>% toupper() %>% trimws() %>%
    str_replace_all("[^A-Z0-9_]", "_") %>% substr(1, 64)
}

# chunk callback --------------------------------------------------------
chunk_fun <- function(x, pos) {
  names(x) <- clean_names(names(x))
  names(x) <- template_names[match(names(x), template_names)]
  x$YEAR   <- new_year
  if (missing_cols <- setdiff(template_names, names(x)))
    x[missing_cols] <- NA_character_
  x <- x %>% select(all_of(template_names))
  write_delim(x, out_file, delim = "\t",
              append = TRUE, col_names = FALSE,
              quote_escape = "double")
  rm(x); invisible(gc())
}

# stream in ~5 MB pieces
read_delim_chunked(
  file      = new_puf,
  delim     = "\t",
  chunk_size = 5 * 1024 * 1024,   #  ≈5 MB
  callback  = SideEffectChunkCallback$new(chunk_fun),
  progress  = TRUE,
  show_col_types = FALSE
)

message("✅ appended ", new_year, " → ", out_file)
