#########################################################################
# Script: 00_install_packages_minimal.R
# Purpose: Quick installation of all required packages
#########################################################################


# Install pacman if needed
if (!require("pacman")) install.packages("pacman")

# Install and load all packages
pacman::p_load(
  # Core packages
  tidyverse, here, fs, janitor, epitrix, gdata,
  # Web scraping & PDF
  xml2, rvest, pdftools, downloader,
  # Spatial analysis
  sf, rgeoda,
  # Visualization
  patchwork, ggpubr, ggtext, ggstatsplot, ggridges,
  # Tables
  gt, gtExtras,
  # Utilities
  tictoc
)

# Tabulizer (optional - requires Java)
tryCatch({
  pacman::p_load(tabulizer)
}, error = function(e) {
  message("Note: tabulizer requires Java JDK to be installed")
})

# This R code is for the set up of folders and files in the Project for better management
fs::dir_create(here::here("tables"))
fs::dir_create(here::here("plots"))
fs::dir_create(here::here("files"))

