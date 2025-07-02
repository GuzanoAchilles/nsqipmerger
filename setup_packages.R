# setup_packages.R

required_packages <- c(
  "tidyverse",   # includes dplyr, tidyr, readr, purrr, tibble, etc.
  "fs",          # file path management
  "readr",       # reading .csv and .txt efficiently
  "stringr",     # string matching for year/file name extraction
  "usethis",     # GitHub and R package tools
  "devtools",    # optional for package dev
  "testthat",    # testing framework (later)
  "janitor"      # optional for cleaning names
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(required_packages, install_if_missing))

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

message("✅ All required packages are installed and loaded.")
