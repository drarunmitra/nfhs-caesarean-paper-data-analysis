################################################################################
# Script: 01_state_download_pdfs_and_scrape_data.R
# Purpose: Download state-level fact sheets from NFHS website and extract
#          caesarean section data from PDFs
# Author: Arun Mitra
################################################################################


# Load Packages
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


# View the Folder Tree
fs::dir_tree(path = here::here(), recurse = FALSE)

# URL of the District Level Fact Sheets
URL <- "http://rchiips.org/nfhs/factsheet_NFHS-5.shtml"


# URLs at the State Level 
state_URLs <- URL |> 
  read_html() |> 
  html_nodes("option") |> 
  html_attr("value") |> 
  paste("http://rchiips.org/nfhs/", ., sep = "") |> 
  tail(-2) |> 
  head(-3)

state_folders <- str_split(state_URLs, "/") |> 
  map_chr(pluck,6) |> 
  str_split("\\.") |> 
  map_chr(pluck,1) 

# Create Statewise folders 
dir_create(here::here("data", "raw_data", "state_reports", "nfhs5", state_folders))

# Destination for Download
file_dest <- state_folders |> 
  epitrix::clean_labels() |> 
  paste(., ".pdf", sep="") |> 
  here::here("data", "raw_data",  "state_reports", "nfhs5", state_folders,.)

# Download Files
state_URLs  |> map2(., file_dest, ~download.file(..1, destfile = ..2, mode = "wb"))


file_paths <- dir_info(here::here("data", "raw_data", "state_reports", "nfhs5"), recurse = 1, type = "file") |> 
  mutate(across(.cols = everything(), as.character)) |> 
  pull(path)

folder_names <- dir_ls(here::here("data", "raw_data",  "state_reports", "nfhs5")) |> basename()

# Create Statewise folders 
dir_create(here::here("data", "subset_pdfs", "state_reports", "nfhs5",  folder_names))

new_file_paths <- file_paths |> str_replace_all("raw_data", "subset_pdfs")

map2(file_paths, new_file_paths, ~pdftools::pdf_subset(..1, pages = 4, output = ..2))

# Remove everything from environment except new_file_paths
gdata::keep("new_file_paths", sure = T)
gc()

# Extract Tables
clean_df_fn <- function(x){
  
  # x <- new_file_paths[[8]]
  
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
  
  df <- x |> 
    tabulizer::extract_tables() |>
    as.data.frame() |>
    as_tibble() |> 
    na_if("") |> 
    janitor::remove_empty(which = "cols") |>
    separate(., X2, into = c("a", "b", "c"), sep = "[^\\S\\r\\n]{2,}", fill = "right") |>
    janitor::remove_empty(which = "cols") |>
    add_column(state = state_name) |>
    setNames(c("indicator", "nfhs5_u", "nfhs5_r", "nfhs5", "nfhs4", "state"))
  
  from <- df |> 
    pull(indicator) |>
    str_detect('Institutional births \\(\\%\\)') |> 
    which()
  
  to <- df |> 
    pull(indicator) |>
    str_detect('Births in a public health facility that were delivered by caesarean section \\(\\%\\)') |> 
    which()
  
  
  df <- df |> 
    slice(from:to) |> 
    mutate_all(list(~na_if(.,""))) |> 
    janitor::remove_empty(which = "cols") |> 
    mutate(indicator = stringr::str_sub(indicator, 5))
}

df_delhi <- new_file_paths[25] |> 
  clean_df_fn() |> 
  mutate(state = "NCT Delhi") |> 
  mutate(nfhs5 = nfhs5_r) |> 
  separate(nfhs5_u, into = c("nfhs5_u", "nfhs5_r"), sep = " ") |> 
  mutate(nfhs5_u = parse_number(nfhs5_u)) |>
  mutate(nfhs5_r = parse_number(nfhs5_r)) |>
  mutate(nfhs5 = parse_number(nfhs5)) 

df <- new_file_paths |> 
  map(clean_df_fn) |> 
  bind_rows() |>
  filter(nfhs5 != "NCT Delhi") |> 
  mutate(nfhs5_u = parse_number(nfhs5_u)) |>
  mutate(nfhs5_r = parse_number(nfhs5_r)) |>
  mutate(nfhs5 = parse_number(nfhs5)) |>
  mutate(nfhs4 = parse_number(nfhs4)) |>
  relocate(state) |> 
  mutate(state = ifelse(is.na(state), "Chandigarh", state)) |> 
  mutate(nfhs4 = ifelse(state == "Chandigarh", nfhs5, nfhs4)) |> 
  mutate(nfhs5 = ifelse(state == "Chandigarh", nfhs5_r, nfhs5)) |>
  mutate(nfhs5_r = ifelse(state == "Chandigarh", NA, nfhs5_r)) |> 
  bind_rows(df_delhi)

  
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

df <- df |>
  mutate(cleaned_indicator = case_when(indicator == "Institutional births (%)" ~ "insti_births_oveall",
                                       indicator == "Institutional births in public facility (%)" ~ "insti_births_public_facility",
                                       indicator == "Home births that were conducted by skilled health personnel (%)" ~ "home_births_skilled",
                                       indicator == "Births attended by skilled health personnel (%)" ~ "births_skilled",
                                       indicator == "Births delivered by caesarean section (%)" ~ "births_cs", 
                                       indicator == "Births in a private health facility that were delivered by caesarean section (%)" ~ "births_cs_private_facility",
                                       indicator == "Births in a public health facility that were delivered by caesarean section (%)" ~ "births_cs_public_facility",
                                       TRUE ~ indicator))

df |> write_rds(here::here("data","clean_data", "state_level_nfhs5_cleaned_df_20211201.rds"))




################################################################################
# NFHS 4 Statelevel



# URL of the District Level Fact Sheets
URL <- "http://rchiips.org/nfhs/factsheet_NFHS-4.shtml"


# URLs at the State Level 
state_URLs <- URL |> 
  read_html() |> 
  html_nodes("option") |> 
  html_attr("value") |> 
  paste("http://rchiips.org/nfhs/", ., sep = "") |> 
  tail(-2)

state_folders <- str_split(state_URLs, "/") |> 
  map_chr(pluck,7) |> 
  str_split("\\.") |> 
  map_chr(pluck,1) |>
  str_split("_") |> 
  map_chr(pluck,1)
  

# Create Statewise folders 
dir_create(here::here("data", "raw_data", "state_reports", "nfhs4", state_folders))

# Destination for Download
file_dest <- state_folders |> 
  epitrix::clean_labels() |> 
  paste(., ".pdf", sep="") |> 
  here::here("data", "raw_data",  "state_reports", "nfhs4", state_folders,.)

# Download Files
state_URLs  |> map2(., file_dest, ~download.file(..1, destfile = ..2, mode = "wb"))


file_paths <- dir_info(here::here("data", "raw_data", "state_reports", "nfhs4"), recurse = 1, type = "file") |> 
  mutate(across(.cols = everything(), as.character)) |> 
  pull(path)

folder_names <- dir_ls(here::here("data", "raw_data",  "state_reports", "nfhs4")) |> basename()

# Create Statewise folders 
dir_create(here::here("data", "subset_pdfs", "state_reports", "nfhs4",  folder_names))

new_file_paths <- file_paths |> str_replace_all("raw_data", "subset_pdfs")

map2(file_paths, new_file_paths, ~pdftools::pdf_subset(..1, pages = 4, output = ..2))


# Remove Chandigarh for the time being 
chandigarh <- new_file_paths[6]
new_file_paths <- new_file_paths[! new_file_paths %in% chandigarh]

# Remove everything from environment except new_file_paths
gdata::keep("new_file_paths", sure = T)
gc()

# Extract Tables
clean_df_fn <- function(x){
  
  # x <- new_file_paths[[6]]
  
  state_name <- x |> 
    pdf_text() |> 
    read_delim(delim = "\n", col_names = FALSE) |> 
    slice(1) |> 
    str_trim() |> 
    str_split(",") |> 
    unlist() |> 
    last() |>
    str_split("- | – | -") |> 
    unlist() |>
    first() |> 
    str_trim()
  
  df <- x |> 
    tabulizer::extract_tables() |>
    as.data.frame() |>
    as_tibble() |> 
    na_if("") |> 
    janitor::remove_empty(which = "cols") |>
    separate(X2, into = c("a", "b", "c","d"), sep = "\\s", fill = "right") |>
    janitor::remove_empty(which = "cols") |>
    dplyr::select(1:4) |> 
    add_column(state = state_name) |>
    setNames(c("indicator", "nfhs4_u", "nfhs4_r", "nfhs4", "state"))
  
  from <- df |> 
    pull(indicator) |>
    str_detect('Institutional births \\(\\%\\)') |> 
    which()
  
  to <- df |> 
    pull(indicator) |>
    str_detect('Births in a public health facility delivered by caesarean section \\(\\%\\)') |> 
    which()
  
  df <- df |> 
    slice(from:to) |> 
    mutate_all(list(~na_if(.,""))) |> 
    janitor::remove_empty(which = "cols") |> 
    mutate(indicator = stringr::str_sub(indicator, 5)) 
}




df <- new_file_paths |> 
  map(clean_df_fn) |> 
  bind_rows() |> 
  mutate(nfhs4_u = parse_number(nfhs4_u)) |>
  mutate(nfhs4_r = parse_number(nfhs4_r)) |>
  mutate(nfhs4 = parse_number(nfhs4)) |>
  relocate(state) 


# Extract Chandigarh 
df_chandigarh <- chandigarh |> 
  tabulizer::extract_tables() |>
  as.data.frame() |>
  as_tibble() |> 
  na_if("") |> 
  janitor::remove_empty(which = "cols") |>
  separate(X3, into = c("a", "b", "c","d"), sep = "\\s", fill = "right") |>
  janitor::remove_empty(which = "cols") |>
  dplyr::select(1:4) |> 
  add_column(state = state_name) 

from <- df_chandigarh |> 
  pull(X1) |>
  str_detect('Institutional births \\(\\%\\)') |> 
  which()

to <- df_chandigarh |> 
  pull(X1) |>
  str_detect('Births in a public health facility delivered by caesarean section \\(\\%\\)') |> 
  which()

df_chandigarh <- df_chandigarh |> 
  slice(from:to) |> 
  mutate_all(list(~na_if(.,""))) |> 
  janitor::remove_empty(which = "cols") |> 
  mutate(X1 = stringr::str_sub(X1, 5)) |> 
  rename(indicator = X1) |>
  mutate(nfhs4_r = as.numeric(NA)) |>
  mutate(state = "Chandigarh") |> 
  mutate(nfhs4_u = parse_number(a)) |> 
  mutate(nfhs4 = parse_number(b)) |> 
  dplyr::select(-c(a,b)) |>
  relocate(state) |> 
  relocate(nfhs4_r, .after = nfhs4_u)

nrow(df)
nrow(df_chandigarh)

df <- df |> bind_rows(df_chandigarh)

nrow(df)


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

df <- df |>
  mutate(cleaned_indicator = case_when(indicator == "Institutional births (%)" ~ "insti_births_oveall",
                                       indicator == "Institutional births in public facility (%)" ~ "insti_births_public_facility",
                                       indicator == "Home births that were conducted by skilled health personnel (%)" ~ "home_births_skilled",
                                       indicator == "Births attended by skilled health personnel (%)" ~ "births_skilled",
                                       indicator == "Births delivered by caesarean section (%)" ~ "births_cs", 
                                       indicator == "Births in a private health facility that were delivered by caesarean section (%)" ~ "births_cs_private_facility",
                                       indicator == "Births in a public health facility that were delivered by caesarean section (%)" ~ "births_cs_public_facility",
                                       TRUE ~ indicator))

df |> write_rds(here::here("data","clean_data", "state_level_nfhs4_cleaned_df_20211201.rds"))




################################################################################
# NFHS 3 Statelevel



# URL of the State Level Fact Sheets
URL <- "http://rchiips.org/nfhs/report.shtml"

# URLs at the State Level 
state_URLs <- URL |> 
  read_html() |> 
  html_nodes("option") |> 
  html_attr("value") |> 
  paste("http://rchiips.org/nfhs/", ., sep = "") |> 
  tail(-1)

state_folders <- str_split(state_URLs, "/") |> 
  map_chr(pluck,5) |> 
  str_split("_") |> 
  map_chr(pluck,1)


# Create Statewise folders 
dir_create(here::here("data", "raw_data", "state_reports", "nfhs3", state_folders))

# Destination for Download
file_dest <- state_folders |> 
  epitrix::clean_labels() |> 
  paste(., ".pdf", sep="") |> 
  here::here("data", "raw_data",  "state_reports", "nfhs3", state_folders,.)

# Create a function to download the PDF files (for each district)
download_state_report <- function(state_url){
  
 # state_url <- state_URLs[[1]]
  # state_folder <- state_codes[[1]]
  
  pg <- state_url |> read_html()
  
  
  file_link <- pg |> 
    html_nodes(".red") |> 
    html_attr("href") |> 
    pluck(1) |> 
    paste("http://rchiips.org/nfhs/", ., sep = "") 
  
  file_dest <- state_url |> 
    str_split("/") |> 
    map_chr(pluck,5) |> 
    str_split("_") |> 
    map_chr(pluck,1) |> 
    epitrix::clean_labels() |> 
    paste(here::here("data", "raw_data",  "state_reports", "nfhs3", ., "/"), ., ".pdf", sep="")
  
  
  download.file(file_link, destfile = file_dest, mode = "wb")
}

state_URLs |> map(download_state_report)

file_paths <- dir_info(here::here("data", "raw_data", "state_reports", "nfhs3"), recurse = 1, type = "file") |> 
  mutate(across(.cols = everything(), as.character)) |> 
  pull(path)


# Remove Chandigarh for the time being 
up <- file_paths[27]
file_paths <- file_paths[! file_paths %in% up]


nfhs3_cs_ectract_fn <- function(file_dest){
  
  # file_dest <- file_paths[[1]]
  
  state_code <- file_dest |> 
    str_split("\\/") |> 
    map_chr(pluck, 11)
  
  x <- file_dest |> 
    pdf_text() |> 
    keep(~ str_detect(.x, "caesarean")) 
  
  str_start <- x |> 
    str_locate("Percentage delivered by caesarean section") |> 
    pluck(2)
  
  str_end <- x |> 
    str_locate("Number of births  ") |> 
    pluck(1)
  
  x1 <- x |> 
    str_sub(str_start + 1, str_end - 1) |> 
    str_split("\\s") |> 
    as.data.frame() |> 
    as_tibble() |> 
    na_if("") |> 
    janitor::remove_empty("rows") |> 
    slice(1:3) |>
    set_names("birth_cs") |>
    mutate(name = c("nfhs3_u",  "nfhs3_r",  "nfhs3")) |>
    add_column(state = state_code)
  
}

df <- file_paths |> map(nfhs3_cs_ectract_fn)

df <- df |> bind_rows()

df <- df |> mutate(birth_cs = as.numeric(birth_cs))

df_up <- tibble(birth_cs = c(12.1, 2.4, 4.4),
                name = c("nfhs3_u",  "nfhs3_r",  "nfhs3"),
                state = "Uttar Pradesh")

df <- df |> bind_rows(df_up)

df <- df |> pivot_wider(values_from = birth_cs, names_from = name)

df |> write_rds(here::here("data","clean_data", "state_level_nfhs3_cleaned_df_20211201.rds"))











































































df <- df |> dplyr::select(-indicator) |> pivot_wider(names_from = cleaned_indicator, values_from = nfhs5)

df <- df |> mutate(nfhs = "nfhs4")

df |> write_rds(here::here("data","clean_data", "nfhs4_cleaned_df_20211129.rds"))



cleaned_df <- bind_rows(nfhs4_cleaned_df_20211129, nfhs5_cleaned_df_20211129)

cleaned_df |> write_rds(here::here("data","clean_data", "cleaned_df_20211130.rds"))

toc()
# df |> View()


# NITI AAYOG DATA
x <- tabulizer::extract_tables(here("files", "District_Codes.pdf"), area = list(c(102.3006,  46.7534, 762.51991, 544.9508)))  

niti_district_codes <- x |> 
  map(as.data.frame) |> 
  map(set_names, c("district_code", "district", "district_hosp_code", "dist_hosp")) |> 
  map(as_tibble) |> 
  bind_rows() |> 
  tail(-1) |> 
  mutate(state = case_when(district_code == "" ~ district,
                           district == "" ~ district_code, 
                           TRUE ~ "")) |>
  mutate(state = na_if(state, "")) |>
  fill(state, .direction = "down") |>
  filter(district_hosp_code != "" & dist_hosp != "")


x <- here("files", "DH_KPI_1.pdf")

# staplr::rotate_pdf(page_rotation = 90, input_filepath = x,
#            output_filepath = x, overwrite = TRUE)
tabulizer::locate_areas(x)
x <- tabulizer::extract_tables(here("files", "DH_KPI_1.pdf"), area = list(c(69.26397, 50.28742, 772.5028, 543.87939)))  

x <- tabulizer::extract_text(here("files", "DH_KPI_1.pdf"), area = list(c(69.26397, 50.28742, 772.5028, 543.87939)))

cat(x)

pdftools::pdf_ocr_text(x)
