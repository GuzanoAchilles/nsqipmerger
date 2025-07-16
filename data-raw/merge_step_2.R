# ─────────────────────────────────────────────────────────────
#  Append one PUF to an existing merge
# ─────────────────────────────────────────────────────────────
library(tidyverse)   # dplyr, readr, %>%
library(fs)

Sys.setenv("R_MAX_VSIZE" = "24GB")   # ① increase heap

# ... your functions read_puf(), safe_merge_two_years() ...

# ── 1)  paths you edit each time ─────────────────────────────
old_merge <- "E:/NSQIP_txt/nsqip_merged_2013_2014.txt"                # <-- previous file
new_puf   <- "E:/NSQIP_txt/acs_nsqip_puf15.txt"  # <-- new PUF
new_year  <- 2015                                                 # <-- its year

# ── 2)  helpers (same as before) ──────────────────────────────
clean_names <- function(nms) {
  nms %>%
    toupper() %>%
    trimws()  %>%
    str_replace_all("[^A-Z0-9_]", "_") %>%
    substr(1, 64)
}

read_puf <- function(path, year) {
  read_delim(path, delim = "\t", show_col_types = FALSE) %>% 
    set_names(clean_names(names(.))) %>% 
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

# ── 3)  read existing merge + new PUF ────────────────────────
merged_old <- read_delim(old_merge, delim = "\t", show_col_types = FALSE) %>% 
  set_names(clean_names(names(.)))                    # tidy names

puf_new    <- read_puf(new_puf, new_year)

# ── 4)  bind safely ───────────────────────────────────────────
merged_new <- safe_bind(merged_old, puf_new)

message("✔ new merged rows: ", nrow(merged_new),
        " | columns: ", ncol(merged_new))

# ── 5)  write out ─────────────────────────────────────────────
dir_create("data-raw")

out_stub <- glue::glue("data-raw/nsqip_merged_2013_{new_year}")

write_delim(merged_new, paste0(out_stub, ".txt"), delim = "\t",
            quote_escape = "double")


message("✅  Saved:\n   • ", out_stub, ".txt\n   • ")
