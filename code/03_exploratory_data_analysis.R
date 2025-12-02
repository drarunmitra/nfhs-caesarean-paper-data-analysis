################################################################################
# Script: 03_exploratory_data_analysis.R
# Purpose: Create manuscript tables and exploratory plots for caesarean
#          section data analysis
# Author: Arun Mitra
################################################################################

# Load Required Packages -------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(gtExtras)
library(ggridges)
library(gt)
library(ggstatsplot)
library(patchwork)

# Load Data --------------------------------------------------------------------
df <- read_rds(here("data", "cleaned_df_20211130.rds"))
zone_df <- read_csv(here("data", "zone_df.csv"))

# Step 1: Data Cleaning --------------------------------------------------------
df <- df |>
  mutate(state = ifelse(state == "Sikar Rajasthan", "Rajasthan", state)) |>
  mutate(district = ifelse(district == "Chandigarh - Key Indicators", "Chandigarh", district)) |>
  mutate(district = ifelse(district == "Sikar Rajasthan - Key Indicators", "Sikar", district)) |>
  mutate(district = ifelse(district == "Haora", "Howrah", district)) |>
  mutate(district = ifelse(district == "Hugli", "Hooghly", district))

# Add Lakshadweep data manually
lakshadweep_df <- tibble::tribble(
  ~state, ~district, ~insti_births_oveall, ~insti_births_public_facility, ~home_births_skilled, ~births_skilled, ~births_cs, ~births_cs_private_facility, ~births_cs_public_facility, ~nfhs,
  "Lakshadweep", "Lakshadweep", 99.6, 65.3, 0.4, 100L, 31.3, 37.7, 28.2, "nfhs5",
  "Lakshadweep", "Lakshadweep", 99.3, 64.3, 0.7, 100L, 38.4, 59.9, 27.1, "nfhs4"
)

df <- df |> bind_rows(lakshadweep_df)

# Standardize state names (merge DNH and DD)
df <- df |>
  mutate(state = case_when(
    state == "Dadra & Nagar Haveli and" ~ "DNH & DD",
    state == "Dadra & Nagar Haveli and Daman & Diu" ~ "DNH & DD",
    state == "Daman & Diu" ~ "DNH & DD",
    TRUE ~ state))

# Join with zone information
df <- df |> left_join(zone_df)

# Step 2: Create State-Level Summary Table -------------------------------------

# Calculate state-level averages by facility type
state_tbl <- df |>
  group_by(state, nfhs) |>
  summarise(
    prop_cs = mean(births_cs, na.rm = TRUE),
    prop_cs_public = mean(births_cs_public_facility, na.rm = TRUE),
    prop_cs_private = mean(births_cs_private_facility, na.rm = TRUE)
  ) |>
  ungroup() |>
  pivot_wider(id_cols = state, names_from = nfhs,
              values_from = c(prop_cs, prop_cs_public, prop_cs_private))

# Join with zone information
state_tbl <- state_tbl |> left_join(zone_df)

# Format table for manuscript
state_private_public_tbl <- state_tbl |>
  gt(
    groupname_col = 'zone',
    row_group_as_column = TRUE
  ) |>
  tab_spanner(
    label = "NFHS - 4",
    columns = contains('nfhs4')
  ) |>
  tab_spanner(
    label = "NFHS - 5",
    columns = contains('nfhs5')
  ) |>
  cols_label(
    state = 'State',
    prop_cs_nfhs4 = "Overall",
    prop_cs_nfhs5 = "Overall",
    prop_cs_public_nfhs4 = "Public",
    prop_cs_public_nfhs5 = "Public",
    prop_cs_private_nfhs4 = "Private",
    prop_cs_private_nfhs5 = "Private"
  ) |>
  fmt_number() |>
  sub_missing() |>
  data_color(
    columns = -state,
    palette = c("#f5f0f0", "#8a3036")
  )

# Save table
state_private_public_tbl |> write_rds('tables/state_private_public_tbl.rds')

# Step 3: Create Box Plots by Zone (NFHS-5) -----------------------------------

# Function to create standardized box plots
create_boxplot <- function(data, variable, plot_title, y_label) {
  data |>
    left_join(zone_df) |>
    ggbetweenstats(
      zone,
      {{ variable }},
      pairwise.display = "none",
      violin.args = list(width = 0, linewidth = 0),
      centrality.plotting = FALSE,
      ggplot.component = scale_y_continuous(limits = c(0, 100)),
      results.subtitle = FALSE,
      title = plot_title,
      xlab = "",
      ylab = y_label
    )
}

# Create plots for NFHS-5
df_nfhs5 <- df |> filter(nfhs == 'nfhs5')

p_overall <- create_boxplot(df_nfhs5, births_cs, "Overall", "Proportion (%)")
p_private <- create_boxplot(df_nfhs5, births_cs_private_facility, "Private Hospitals", "")
p_public <- create_boxplot(df_nfhs5, births_cs_public_facility, "Public Hospitals", "")

# Combine plots
p_cs_box_pub_pvt_nfhs5 <- (p_overall + p_public + p_private) +
  plot_annotation(title = "NFHS - 5",
                  theme = theme(plot.title = element_text(size = 15, face = "bold")))

# Save plot
p_cs_box_pub_pvt_nfhs5 |> write_rds("plots/p_cs_box_pub_pvt_nfhs5.rds")
ggsave("manuscript/plots/p_cs_box_pub_pvt_nfhs5.png", dpi = 300,
       height = 7, width = 15, scale = 0.6)

# Create plots for NFHS-4
df_nfhs4 <- df |> filter(nfhs == 'nfhs4')

p_overall_4 <- create_boxplot(df_nfhs4, births_cs, "Overall", "Proportion (%)")
p_private_4 <- create_boxplot(df_nfhs4, births_cs_private_facility, "Private Hospitals", "")
p_public_4 <- create_boxplot(df_nfhs4, births_cs_public_facility, "Public Hospitals", "")

# Combine plots
p_cs_box_pub_pvt_nfhs4 <- (p_overall_4 + p_public_4 + p_private_4) +
  plot_annotation(title = "NFHS - 4",
                  theme = theme(plot.title = element_text(size = 15, face = "bold")))

# Save plot
p_cs_box_pub_pvt_nfhs4 |> write_rds("plots/p_cs_box_pub_pvt_nfhs4.rds")
ggsave("plots/p_cs_box_pub_pvt_nfhs4.png", dpi = 300,
       height = 7, width = 15, scale = 0.6)

# Combined NFHS-4 and NFHS-5 plot
(wrap_elements(p_cs_box_pub_pvt_nfhs5) / wrap_elements(p_cs_box_pub_pvt_nfhs4)) +
  plot_annotation(title = "Proportion of Caesarean Sections across Administrative Regions of India",
                  caption = "Data from the National Family Health Survey - 4 & 5",
                  theme = theme(plot.title = element_text(size = 20, face = "bold")))

ggsave("plots/p_cs_box_pub_pvt.png", dpi = 300,
       height = 15, width = 20, scale = 0.6)

# Step 4: Create Urban-Rural Comparison Table ----------------------------------

# Load urban-rural data
# Created in 01_state_download_pdfs_and_scrape_data.R script
u_r_nfhs4_df <- read_rds(here("data", "clean_data", "state_level_nfhs4_cleaned_df_20211201.rds"))
u_r_nfhs5_df <- read_rds(here("data", "clean_data", "state_level_nfhs5_cleaned_df_20211201.rds"))

# Standardize state names
u_r_nfhs4_df <- u_r_nfhs4_df |>
  mutate(state = case_when(
    state == "Daman & Diu" ~ "DNH & DD",
    state == "Dadra & Nagar Haveli" ~ "DNH & DD",
    TRUE ~ state
  ))

u_r_nfhs5_df <- u_r_nfhs5_df |>
  mutate(state = ifelse(state == "Dadra & Nagar Haveli and Daman & Diu", "DNH & DD", state))

# Merge datasets
u_r_df <- u_r_nfhs5_df |>
  select(-nfhs4) |>
  left_join(u_r_nfhs4_df) |>
  filter(cleaned_indicator == 'births_cs') |>
  select(-indicator, -cleaned_indicator)

# Organize columns
u_r_df <- u_r_df |>
  relocate(nfhs4, .after = state) |>
  relocate(nfhs4_u, .after = nfhs4) |>
  relocate(nfhs4_r, .after = nfhs4_u) |>
  relocate(nfhs5, .after = nfhs4_r)

# Calculate state averages
u_r_df <- u_r_df |>
  group_by(state) |>
  summarize_all(mean, na.rm = TRUE) |>
  ungroup()

# Calculate differences between NFHS-5 and NFHS-4
u_r_df <- u_r_df |>
  mutate(diff = nfhs5 - nfhs4) |>
  mutate(diff_u = nfhs5_u - nfhs4_u) |>
  mutate(diff_r = nfhs5_r - nfhs4_r)

# Join with zone information
u_r_df <- u_r_df |> left_join(zone_df)

# Format table for manuscript
state_urban_rural_tbl <- u_r_df |>
  gt(
    groupname_col = 'zone',
    row_group_as_column = TRUE
  ) |>
  tab_spanner(
    label = "NFHS - 4",
    columns = contains('nfhs4')
  ) |>
  tab_spanner(
    label = "NFHS - 5",
    columns = contains('nfhs5')
  ) |>
  tab_spanner(
    label = "Increase in CS",
    columns = contains('diff')
  ) |>
  cols_label(
    state = 'State',
    nfhs4 = "Overall",
    nfhs5 = "Overall",
    diff = 'Overall',
    nfhs4_u = "Urban",
    nfhs5_u = "Urban",
    diff_u = "Urban",
    nfhs4_r = "Rural",
    nfhs5_r = "Rural",
    diff_r = "Rural"
  ) |>
  fmt_number() |>
  sub_missing() |>
  data_color(
    columns = -state,
    palette = c("#f5f0f0", "#8a3036")
  )

# Save table
state_urban_rural_tbl |> write_rds('tables/state_urban_rural_tbl.rds')

