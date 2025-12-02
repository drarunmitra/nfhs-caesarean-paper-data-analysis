################################################################################
# Script: 01_district_download_pdfs_and_scrape_data.R
# Purpose: Download district-level fact sheets from NFHS website and extract
#          caesarean section data from PDFs
# Author: Arun Mitra
################################################################################

# Load Required Packages -------------------------------------------------------
library(xml2)
library(rvest)
library(here)
library(tidyverse)
library(fs)
library(pdftools)
library(downloader)
library(tictoc)

# Start Timer
tic()


# View the folder structure
fs::dir_tree(path = here::here(), recurse = FALSE)

# Step 1: Get URLs for all state-level pages ----------------------------------
URL <- "http://rchiips.org/nfhs/districtfactsheet_NFHS-4.shtml"

state_URLs <- URL |>
  read_html() |>
  html_nodes("option") |>
  html_attr("value") |>
  paste("http://rchiips.org/nfhs/", ., sep = "") |>
  tail(-1)

# Extract state codes from URLs
state_codes <- str_split(state_URLs, "/") |>
  map_chr(pluck, 5) |>
  str_split("\\.") |>
  map_chr(pluck, 1) |>
  replace(., . == "MadhyaPradesh", "MP")

# Create state-wise folders
dir_create(here::here("data", "raw_data", "nfhs4", state_codes))

# Set names for the URLs
state_URLs <- state_URLs |> set_names(state_codes)

# Step 2: Download district fact sheets for each state ------------------------
download_district_factsheet <- function(state_url, state_folder) {
  # Read the state page
  pg <- state_url |> read_html()

  # Get destination file names (cleaned district names)
  dest_names <- pg |>
    html_nodes("option") |>
    html_text() |>
    tail(-1) |>
    epitrix::clean_labels() |>
    paste(., ".pdf", sep = "") |>
    here::here("data", "raw_data", "nfhs4", state_folder, .)

  # Get PDF download links
  file_link <- pg |>
    html_nodes("option") |>
    html_attr("value") |>
    paste("http://rchiips.org/nfhs/", ., sep = "") |>
    str_replace_all(., " ", "%20") |>
    tail(-1)

  # Download all PDFs for this state
  file_link |> map2(., dest_names, ~download.file(..1, destfile = ..2, mode = "wb"))
}

# Download PDFs for all states
map2(state_URLs, state_codes, ~(download_district_factsheet(..1, ..2)))

# Add Chandigarh manually (separate link structure)
dir_create(here::here("data", "raw_data", "nfhs4", "CH"))
URL_Chandigarh <- "http://rchiips.org/nfhs/pdf/NFHS4/CH_FactSheet.pdf"
download.file(URL_Chandigarh,
              destfile = here::here("data", "raw_data", "nfhs4", "CH", "chandigarh.pdf"),
              mode = "wb")

# View folder structure
fs::dir_tree(path = here::here("data", "raw_data", "nfhs4"), recurse = 1)

# Total number of districts (should be 705)
dir_ls(here::here("data", "raw_data", "nfhs4"), recurse = 1, type = "file") |> length()

# Step 3: Extract only page 4 from each PDF (contains C-section data) ---------
file_paths <- dir_info(here::here("data", "raw_data", "nfhs4"), recurse = 1, type = "file") |>
  mutate(across(.cols = everything(), as.character)) |>
  pull(path)

folder_names <- dir_ls(here::here("data", "raw_data", "nfhs4")) |> basename()

# Create folders for subset PDFs
dir_create(here::here("data", "subset_pdfs", "nfhs4", folder_names))

new_file_paths <- file_paths |> str_replace_all("raw_data", "subset_pdfs")

# Extract page 4 from each PDF
map2(file_paths, new_file_paths, ~pdftools::pdf_subset(..1, pages = 4, output = ..2))

# Exclude Chandigarh temporarily (different format)
chandigarh <- new_file_paths[98]
new_file_paths <- new_file_paths[!new_file_paths %in% chandigarh]

# Clean environment
gdata::keep("new_file_paths", sure = TRUE)
gc()

# Step 4: Extract tables from PDFs --------------------------------------------

# Function to clean and extract data from district fact sheets
clean_df_fn <- function(x) {
  # Extract district name
  district_name <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    slice(1) |>
    str_trim() |>
    str_split(",") |>
    unlist() |>
    first()

  # Extract state name
  state_name <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    slice(1) |>
    str_trim() |>
    str_split(",") |>
    unlist() |>
    last() |>
    str_split("- | – ") |>
    unlist() |>
    first() |>
    str_trim()

  # Parse the PDF table
  df <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    separate(., X1, into = c("a", "b", "c", "d", "e", "f", "g"),
             sep = "[^\\S\\r\\n]{2,}", fill = "right") |>
    add_column(district = district_name) |>
    add_column(state = state_name)

  # Find rows with relevant indicators
  from <- df |>
    pull(a) |>
    str_detect('Institutional births \\(\\%\\)') |>
    which()

  to <- df |>
    pull(a) |>
    str_detect('Births in a public health facility delivered by caesarean section \\(\\%\\)') |>
    which()

  # Extract and clean the relevant rows
  df <- df |>
    slice(from:to) |>
    mutate_all(list(~na_if(., ""))) |>
    janitor::remove_empty(which = "cols") |>
    mutate(a = stringr::str_sub(a, 5)) |>
    rename(indicator = a)

  # Select NFHS-4 column (varies by district format)
  if (ncol(df) == 5) {
    df <- df |> rename(nfhs4 = c)
  } else {
    df <- df |> rename(nfhs4 = d)
  }

  return(df)
}

# Extract data from all districts
df <- new_file_paths |>
  map(clean_df_fn) |>
  bind_rows() |>
  mutate(nfhs4 = parse_number(nfhs4)) |>
  dplyr::select(-c(b, c)) |>
  mutate(ID = group_indices(., state, district)) |>
  relocate(ID) |>
  relocate(state, .after = ID) |>
  relocate(district, .after = state)

# Step 5: Process Chandigarh separately (different format) --------------------
df_chandigarh <- chandigarh |>
  pdf_text() |>
  read_delim(delim = "\n", col_names = FALSE) |>
  separate(., X1, into = c("a", "b", "c", "d", "e", "f", "g"),
           sep = "[^\\S\\r\\n]{2,}", fill = "right") |>
  add_column(district = "Chandigarh") |>
  add_column(state = "Chandigarh")

from <- df_chandigarh |>
  pull(b) |>
  str_detect('Institutional births \\(\\%\\)') |>
  which()

to <- df_chandigarh |>
  pull(b) |>
  str_detect('Births in a public health facility delivered by caesarean section \\(\\%\\)') |>
  which()

df_chandigarh <- df_chandigarh |>
  slice(from:to) |>
  mutate_all(list(~na_if(., ""))) |>
  janitor::remove_empty(which = "cols") |>
  mutate(b = stringr::str_sub(b, 5)) |>
  rename(indicator = b) |>
  mutate(nfhs4 = parse_number(d)) |>
  dplyr::select(-c(c, d)) |>
  mutate(ID = ((df |> pull(ID) |> max()) + 1)) |>
  relocate(ID) |>
  relocate(state, .after = ID) |>
  relocate(district, .after = state)

# Combine main data with Chandigarh
df <- df |> bind_rows(df_chandigarh)

# Step 6: Create clean variable names and labels ------------------------------
cleaned_indicator <- c("insti_births_oveall",
                       "insti_births_public_facility",
                       "home_births_skilled",
                       "births_skilled",
                       "births_cs",
                       "births_cs_private_facility",
                       "births_cs_public_facility")

labels <- df |>
  pull(indicator) |>
  unique() |>
  as_tibble() |>
  add_column(var_name = cleaned_indicator) |>
  rename(var_label = value)

# Recode indicators to clean variable names
df <- df |>
  mutate(cleaned_indicator = case_when(
    indicator == "Institutional births (%)" ~ "insti_births_oveall",
    indicator == "Institutional births in public facility (%)" ~ "insti_births_public_facility",
    indicator == "Home delivery conducted by skilled health personnel (out of total deliveries) (%)" ~ "home_births_skilled",
    indicator == "Births assisted by a doctor/nurse/LHV/ANM/other health personnel (%)" ~ "births_skilled",
    indicator == "Births delivered by caesarean section (%)" ~ "births_cs",
    indicator == "Births in a private health facility delivered by caesarean section (%)" ~ "births_cs_private_facility",
    indicator == "Births in a public health facility delivered by caesarean section (%)" ~ "births_cs_public_facility",
    TRUE ~ indicator))

# Reshape to wide format
df <- df |>
  dplyr::select(-indicator) |>
  pivot_wider(names_from = cleaned_indicator, values_from = nfhs4)

# Add NFHS indicator
df <- df |> mutate(nfhs = "nfhs4")

# Step 7: Save cleaned NFHS-4 data --------------------------------------------
df |> write_rds(here::here("data", "clean_data", "nfhs4_cleaned_df_20211129.rds"))

# Note: This script only processes NFHS-4 data.
# NFHS-5 data should be processed separately using a similar approach.
# URL of the District Level Fact Sheets
URL <- "http://rchiips.org/NFHS/districtfactsheet_NFHS-5.shtml"


# URLs at the State Level 
state_URLs <- URL |>
  read_html() |>
  html_nodes("option") |>
  html_attr("value") |>
  paste("http://rchiips.org/NFHS/", ., sep = "") |>
  tail(-1) 

# Correct for Wrong link in Telangana (TL -> TG)
state_URLs <- state_URLs |>ifelse(. == "http://rchiips.org/NFHS/NFHS-5_TL.shtml", "http://rchiips.org/NFHS/NFHS-5_TG.shtml", .) 

# Remove Chandigarh as it is a redundant link
state_URLs <- state_URLs |>.[.!="http://rchiips.org/NFHS/NFHS-5_CH.shtml"] 

# Codes of the states
state_codes <- str_split(state_URLs, "_") |>
  map_chr(pluck,2) |>
  str_split("\\.") |>
  map_chr(pluck,1)

# Create Statewise folders 
dir_create(here::here("data", "raw_data", state_codes))

# Set Names of the URLs
state_URLs <- state_URLs |>set_names(state_codes)

# Create a function to download the PDF files (for each district)

download_district_factsheet <- function(state_url, state_folder){
  
  
  pg <- state_url |>read_html()
  
  dest_names <- pg |>
    html_nodes("option") |>
    html_text() |>
    tail(-1) |>
    epitrix::clean_labels() |>
    paste(., ".pdf", sep="") |>
    here::here("data", "raw_data", state_folder,.)
  
  file_link <- pg |>
    html_nodes("option") |>
    html_attr("value") |>
    paste("http://rchiips.org/NFHS/", ., sep = "") |>
    str_replace_all(., " ", "%20") |>
    tail(-1)
  
  file_link |>map2(., dest_names, ~download.file(..1, destfile = ..2, mode = "wb"))
}


# map2(tail(state_URLs,5), tail(state_codes,5), ~(download_district_factsheet(..1, ..2)))
map2(state_URLs, state_codes, ~(download_district_factsheet(..1, ..2)))

# Add Chandigarh Manually
dir_create(here::here("data", "raw_data", "CH"))
URL_Chandigarh <- "http://rchiips.org/NFHS/NFHS-5_FCTS/Chandigarh.pdf"
download.file(URL_Chandigarh, destfile = here::here("data", "raw_data", "CH", "chandigarh.pdf"))


# View the Folder Tree
fs::dir_tree(path = here::here("data", "raw_data"), recurse = 1)

# Total number of Districts (705)
dir_ls(here::here("data", "raw_data"), recurse = 1, type = "file") |>length()

file_paths <- dir_info(here::here("data", "raw_data"), recurse = 1, type = "file") |>
  mutate(across(.cols = everything(), as.character)) |>
  pull(path)

folder_names <- dir_ls(here::here("data", "raw_data")) |>basename()

# Create Statewise folders 
dir_create(here::here("data", "subset_pdfs", folder_names))

new_file_paths <- file_paths |>str_replace_all("raw_data", "subset_pdfs")

map2(file_paths, new_file_paths, ~pdftools::pdf_subset(..1, pages = 4, output = ..2))

# Remove everything from environment except new_file_paths
gdata::keep("new_file_paths", sure = T)
gc()

# Extract Tables
clean_df_fn <- function(x){
  
  district_name <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    slice(1) |>
    str_trim() |>
    str_split(",") |>
    unlist() |>
    first()
  
  state_name <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    slice(1) |>
    str_trim() |>
    str_split(",") |>
    unlist() |>
    last() %>%
    str_split("- | – ") |>
    unlist() %>%
    first() |>
    str_trim()
  
  df <- x |>
    pdf_text() |>
    read_delim(delim = "\n", col_names = FALSE) |>
    separate(., X1, into = c("a", "b", "c", "d", "e", "f", 'g'), sep = "[^\\S\\r\\n]{2,}", fill = "right") |>
    add_column(district = district_name) |>
    add_column(state = state_name)
  
  from <- df |>
    pull(b) %>%
    str_detect('Institutional births \\(\\%\\)') |>
    which()
  
  to <- df |>
    pull(b) %>%
    str_detect('Births in a public health facility that were delivered by caesarean section \\(\\%\\)') |>
    which()
  
  df <- df |>
    slice(from:to) |>
    mutate_all(list(~na_if(.,""))) |>
    janitor::remove_empty(which = "cols") |>
    mutate(b = stringr::str_sub(b, 5)) |>
    rename(indicator = b)
  
  if(ncol(df) == 4){
    df <- df |>rename(nfhs5 = c)
  }
  
  else{
    if(ncol(df) == 5){
      df <- df |>rename(nfhs5 = c, nfhs4 = d)
    }
    
    else{
      df <- df |>rename(nfhs5 = e, nfhs4 = f)
    }
  }
}

df <- new_file_paths |>
  map(clean_df_fn) |>
  bind_rows() |>
  dplyr::select(-c(c,d)) |>
  mutate(nfhs5 = parse_number(nfhs5)) |>
  mutate(nfhs4 = parse_number(nfhs4)) |>
  mutate(ID = group_indices(., state, district)) |>
  relocate(ID) |>
  relocate(state, .after = ID) |>
  relocate(district, .after = state)

df |>write_rds(here::here("data","clean_data", "nfhs5_cleaned_df_20211129.rds"))

toc()

# The final combined dataset is created by joining them both
nfhs4 <- readr::read_rds(here::here("data","clean_data", "nfhs4_cleaned_df_20211129.rds"))
nfhs5 <- readr::read_rds(here::here("data","clean_data", "nfhs5_cleaned_df_20211129.rds"))

dplyr::bind_rows(nfhs4, nfhs5) |>
  write_rds(here::here("data","clean_data", "cleaned_df_20211130.rds"))