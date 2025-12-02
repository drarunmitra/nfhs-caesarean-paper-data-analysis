################################################################################
# Script: 02_cleaning_and_joining_with_spatial_data.R
# Purpose: Clean NFHS-4 and NFHS-5 data, merge datasets, and join with spatial
#          boundaries to create district-level shapefiles
# Author: Arun Mitra
################################################################################

# Load Required Packages -------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(ggpubr)
library(rgeoda)
library(patchwork)
library(ggtext)

# Load Data --------------------------------------------------------------------
# Dataset created in 01_district_download_pdfs_and_scrape_data.R 
df <- read_rds(here("data", "cleaned_df_20211130.rds"))

# Step 1: Fix data entry errors -----------------------------------------------
df <- df |>
  mutate(state = ifelse(state == "Sikar Rajasthan", "Rajasthan", state)) |>
  mutate(district = ifelse(district == "Chandigarh - Key Indicators", "Chandigarh", district)) |>
  mutate(district = ifelse(district == "Sikar Rajasthan - Key Indicators", "Sikar", district)) |>
  mutate(district = ifelse(district == "Haora", "Howrah", district)) |>
  mutate(district = ifelse(district == "Hugli", "Hooghly", district))

# Step 2: Add Lakshadweep data manually (missing from PDFs) -------------------
lakshadweep_df <- tibble::tribble(
  ~state, ~district, ~insti_births_oveall, ~insti_births_public_facility, ~home_births_skilled, ~births_skilled, ~births_cs, ~births_cs_private_facility, ~births_cs_public_facility, ~nfhs,
  "Lakshadweep", "Lakshadweep", 99.6, 65.3, 0.4, 100L, 31.3, 37.7, 28.2, "nfhs5",
  "Lakshadweep", "Lakshadweep", 99.3, 64.3, 0.7, 100L, 38.4, 59.9, 27.1, "nfhs4"
)

df <- df |> bind_rows(lakshadweep_df)

# Step 3: Reshape data to long format -----------------------------------------
df_nfhs5 <- df |>
  filter(nfhs == 'nfhs5') |>
  dplyr::select(-c(nfhs, ID)) |>
  pivot_longer(cols = insti_births_oveall:births_cs_public_facility,
               names_to = "cleaned_indicator",
               values_to = 'nfhs5')

df_nfhs4 <- df |>
  filter(nfhs == 'nfhs4') |>
  dplyr::select(-c(nfhs, ID)) |>
  pivot_longer(cols = insti_births_oveall:births_cs_public_facility,
               names_to = "cleaned_indicator",
               values_to = 'nfhs4')

# Merge NFHS-4 and NFHS-5 data
df <- df_nfhs5 |> left_join(df_nfhs4, by = c("state", "district", "cleaned_indicator"))

# Create unique district identifier
df <- df |>
  mutate(district_unique = paste(epitrix::clean_labels(district),
                                 epitrix::clean_labels(state), sep = "_"))

# Step 4: Load and process state-level data -----------------------------------
df_state <- read_rds(here("data", "state_level_nfhs5_cleaned_df_20211201.rds"))

# Fill missing values and handle special cases
df_state <- df_state |>
  mutate(nfhs4 = ifelse(state == "NCT Delhi", nfhs5, nfhs4)) |>
  tidyr::complete(state, cleaned_indicator) |>
  mutate(cleaned_indicator = case_when(
    cleaned_indicator == "Home births that were conducted by skilled health personnel10 (%)" ~ "home_births_skilled",
    cleaned_indicator == "Births attended by skilled health personnel10 (%)" ~ "births_skilled",
    TRUE ~ cleaned_indicator)) |>
  # Fill in missing values for specific states
  mutate(nfhs5 = case_when(
    state == "NCT Delhi" & cleaned_indicator == "insti_births_oveall" ~ 91.8,
    state == "NCT Delhi" & cleaned_indicator == "insti_births_public_facility" ~ 62.4,
    state == "NCT Delhi" & cleaned_indicator == "home_births_skilled" ~ 2.3,
    state == "NCT Delhi" & cleaned_indicator == "births_skilled" ~ 93.4,
    state == "NCT Delhi" & cleaned_indicator == "births_cs" ~ 23.6,
    state == "NCT Delhi" & cleaned_indicator == "births_cs_private_facility" ~ 42.8,
    state == "Ladakh" & cleaned_indicator == "births_cs_private_facility" ~ 39.3,
    state == "Sikkim" & cleaned_indicator == "births_cs_private_facility" ~ 55.4,
    state == "Tripura" & cleaned_indicator == "births_cs_private_facility" ~ 69.3,
    state == "NCT Delhi" & cleaned_indicator == "births_cs_public_facility" ~ 17.7,
    state == "Puducherry" & cleaned_indicator == "births_cs_private_facility" ~ 42.0,
    state == "Lakshadweep" & cleaned_indicator == "births_cs_private_facility" ~ 37.7,
    state == "Andaman & Nicobar Islands" & cleaned_indicator == "births_cs_private_facility" ~ 79.2,
    TRUE ~ nfhs5)) |>
  mutate(nfhs4 = case_when(
    state == "Ladakh" & cleaned_indicator == "births_cs_private_facility" ~ 22.4,
    state == "Sikkim" & cleaned_indicator == "births_cs_private_facility" ~ 49.3,
    state == "Tripura" & cleaned_indicator == "births_cs_private_facility" ~ 73.7,
    state == "Puducherry" & cleaned_indicator == "births_cs_private_facility" ~ 47.4,
    state == "Lakshadweep" & cleaned_indicator == "births_cs_private_facility" ~ 59.9,
    state == "Andaman & Nicobar Islands" & cleaned_indicator == "births_cs_private_facility" ~ 20.2,
    TRUE ~ nfhs4)) |>
  mutate(state_avg_nfhs5 = nfhs5, state_avg_nfhs4 = nfhs4) |>
  dplyr::select(state, cleaned_indicator, state_avg_nfhs5, state_avg_nfhs4) |>
  mutate(state_avg_nfhs5 = ifelse(state == "Chandigarh" & cleaned_indicator == "births_cs_private_facility",
                                  44.3, state_avg_nfhs5))

# Merge district and state data, filling missing districts with state averages
df <- df |>
  left_join(df_state) |>
  mutate(nfhs5 = ifelse(is.na(nfhs5), state_avg_nfhs5, nfhs5)) |>
  mutate(nfhs4 = ifelse(is.na(nfhs4), state_avg_nfhs4, nfhs4))

# Step 5: Load spatial data (shapefiles) ---------------------------------------
india_district_sf <- read_rds(here("files", "india_districts_simplified.rds"))
india_states <- read_rds(here("files", "india_states_simplified.rds"))
india_sf <- read_rds(here("files", "india_sf.rds"))

# Step 6: Harmonize district names between data and shapefiles ----------------
india_district_sf <- india_district_sf |>
  mutate(state = str_to_title(statename)) |>
  mutate(state = case_when(
    state == "Andaman & Nicobar" ~ "Andaman & Nicobar Islands",
    state == "Dadra & Nagar Have" ~ "Dadra & Nagar Haveli and Daman & Diu",
    state == "Daman & Diu" ~ "Dadra & Nagar Haveli and Daman & Diu",
    state == "Delhi" ~ "NCT Delhi",
    TRUE ~ state)) |>
  dplyr::select(-district) |>
  mutate(district = epitrix::clean_labels(distname)) |>
  # Harmonize district names to match NFHS data
  mutate(district_new = case_when(
    district == "nicobars" ~ "nicobar",
    district == "sri_potti_sriramulu_nell" ~ "sri_potti_sriramulu_nellore",
    district == "upper_dibang_valley" ~ "dibang_valley",
    district == "lower_dibang_valley" ~ "dibang_valley",
    district == "dakshin_bastar_dantewada" ~ "dantewada",
    district == "gariaband" ~ "gariyaband",
    district == "kondagaon" ~ "kodagaon",
    district == "ahmadabad" ~ "ahmedabad",
    district == "aravalli" ~ "aravali",
    district == "banas_kantha" ~ "banaskantha",
    district == "sabar_kantha" ~ "sabarkantha",
    district == "chota_udaipur" ~ "chhota_udaipur",
    district == "charki_dadri" ~ "charkhi_dadri",
    district == "gurugram" ~ "gurgaon",
    district == "nuh" ~ "mewat",
    district == "bagalkote" ~ "bagalkot",
    district == "ballari" ~ "bellary",
    district == "belagavi" ~ "belgaum",
    district == "bengaluru_rural" ~ "bangalore_rural",
    district == "vijayapura" ~ "bijapur",
    district == "chikkamagaluru" ~ "chikmagalur",
    district == "Chamarajanagara" ~ "chamarajanagar",
    district == "kalaburagi" ~ "gulbarga",
    district == "mysuru" ~ "mysore",
    district == "shivamogga" ~ "shimoga",
    district == "tumakuru" ~ "tumkur",
    district == "leh" ~ "leh_ladakh",
    district == "east_nimar" ~ "khandwa_east_nimar",
    district == "west_nimar" ~ "khargone_west_nimar",
    district == "niwari" ~ "tikamgarh",
    district == "ahmadnagar" ~ "ahmednagar",
    district == "sahibzada_ajit_singh_nag" ~ "sahibzada_ajit_singh_nagar",
    district == "sri_muktsar_sahib" ~ "muktsar",
    district == "jayashankar" ~ "jayashankar_bhupalapally",
    district == "kumuram_bheem_asifabad" ~ "komaram_bheem_asifabad",
    district == "sipahijala" ~ "sepahijala",
    district == "unokoti" ~ "unakoti",
    district == "bara_banki" ~ "barabanki",
    district == "amroha" ~ "jyotiba_phule_nagar",
    district == "kasganj" ~ "kanshiram_nagar",
    district == "mahrajganj" ~ "maharajganj",
    district == "hathras" ~ "mahamaya_nagar",
    district == "sant_kabir_nagar" ~ "sant_kabeer_nagar",
    district == "bhadohi" ~ "sant_ravidas_nagar_bhadohi",
    district == "shrawasti" ~ "shravasti",
    district == "hardwar" ~ "haridwar",
    district == "garhwal" ~ "pauri_garhwal",
    district == "udham_singh_nagar" ~ "udam_singh_nagar",
    district == "darjiling" ~ "darjeeling",
    district == "cooch_behar" ~ "koch_bihar",
    district == "north_twenty_four_pargan" ~ "north_twenty_four_parganas",
    district == "paschim_bardhaman" ~ "paschim_barddhaman",
    district == "purab_bardhaman" ~ "purab_barddhaman",
    district == "medinipur_west" ~ "paschim_medinipur",
    district == "south_twenty_four_pargan" ~ "south_twenty_four_parganas",
    TRUE ~ district)) |>
  mutate(district_unique = paste(epitrix::clean_labels(district_new),
                                 epitrix::clean_labels(state), sep = "_"))

# Ensure valid geometries
india_district_sf <- india_district_sf |> sf::st_make_valid()

# Step 7: Create spatial data for each indicator -------------------------------

# Function to join indicator data with spatial boundaries
# and fill missing districts with state averages
clean_districts_fn <- function(x) {
  df1 <- df |>
    tidyr::complete(state, cleaned_indicator) |>
    filter(cleaned_indicator == x) |>
    dplyr::select(nfhs5, nfhs4, district_unique, state)

  df_state1 <- df_state |>
    tidyr::complete(state, cleaned_indicator) |>
    filter(cleaned_indicator == x)

  india_district_sf1 <- india_district_sf |> left_join(df1)

  # Find districts with missing data and fill with state averages
  na_df <- india_district_sf1 |>
    st_drop_geometry() |>
    filter(is.na(nfhs5)) |>
    left_join(df_state1, by = "state") |>
    mutate(nfhs5 = state_avg_nfhs5) |>
    mutate(nfhs4 = state_avg_nfhs4) |>
    dplyr::select(c(nfhs5, nfhs4, district_unique, state))

  df1 <- df1 |> bind_rows(na_df)

  # Join back with spatial data
  india_district_sf1 <- india_district_sf |>
    left_join(df1)

  india_district_sf1 <- india_district_sf1 |> st_make_valid()

  return(india_district_sf1)
}

# Create spatial datasets for each indicator
sf_births_cs <- clean_districts_fn("births_cs")
sf_births_cs_private <- clean_districts_fn("births_cs_private_facility")
sf_births_cs_public <- clean_districts_fn("births_cs_public_facility")


# Write the spatial datasets
sf_births_cs  |> write_rds(here("files", "sf_births_cs.rds"))
sf_births_cs_private  |> write_rds(here("files", "sf_births_cs_private.rds"))
sf_births_cs_public  |> write_rds(here("files", "sf_births_cs_public.rds"))


