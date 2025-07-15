# ─────────────────────────────────────────────────────────────
#  NSQIP 2013 + 2014   fresh merge  (TAB-delimited output)
# ─────────────────────────────────────────────────────────────

library(tidyverse)   # dplyr, readr, %>%
library(fs)

# ---- 1)  paths --------------------------------------------------------------
puf_2013 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf13.txt"
puf_2014 <- "/Users/addimoya/NSQIP_PUF/acs_nsqip_puf/acs_nsqip_puf14.txt"

# ---- 2)  helpers ------------------------------------------------------------
clean_names <- function(nms) {
  nms %>%
    toupper() %>%                         # upper-case
    trimws()  %>%                         # strip spaces
    str_replace_all("[^A-Z0-9_]", "_") %>%# keep safe chars
    substr(1, 64)                         # SPSS-safe length
}

read_puf <- function(path, year) {
  read_delim(path, delim = "\t", show_col_types = FALSE) %>%      # <- keep %>%
    set_names(clean_names(names(.))) %>%                          # names cleaned
    mutate(YEAR = year)
}

safe_bind <- function(a, b) {
  common  <- intersect(names(a), names(b))
  clashes <- common[
    map_chr(a[common], ~ class(.x)[1]) !=
      map_chr(b[common], ~ class(.x)[1])
  ]
  if (length(clashes)) {
    message("→ Converting to character: ", paste(clashes, collapse = ", "))
    a <- mutate(a, across(all_of(clashes), as.character))
    b <- mutate(b, across(all_of(clashes), as.character))
  }
  bind_rows(a, b)
}

# ---- 3)  read + merge -------------------------------------------------------
nsqip_2013 <- read_puf(puf_2013, 2013)
nsqip_2014 <- read_puf(puf_2014, 2014)

merged_13_14 <- safe_bind(nsqip_2013, nsqip_2014)
message("✔ merged rows: ", nrow(merged_13_14),
        " | columns: ", ncol(merged_13_14))

# ---- 4)  save ---------------------------------------------------------------
dir_create("data-raw")

txt_path <- "data-raw/nsqip_merged_2013_2014.txt"
write_delim(merged_13_14, txt_path, delim = "\t", quote_escape = "double")

rds_path <- "data-raw/nsqip_merged_2013_2014.rds"
saveRDS(merged_13_14, rds_path, compress = "xz")

message("✅  Wrote files:\n   • ", txt_path, "\n   • ", rds_path)
