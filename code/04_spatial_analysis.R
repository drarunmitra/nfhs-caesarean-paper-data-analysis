################################################################################
# Script: 04_spatial_analysis.R
# Purpose: Perform spatial clustering analysis using Local Moran's I to identify
#          clusters of high and low caesarean section rates
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

# Load Prepared Data --------------------------------------------------------
# Created in 02_cleaning_and_joining_with_spatial_data.R script
sf_births_cs <- read_rds(here('files', 'sf_births_cs.rds'))
sf_births_cs_private <- read_rds(here('files', 'sf_births_cs_private.rds'))
sf_births_cs_public <- read_rds(here('files', 'sf_births_cs_public.rds'))


# Load Spatial Data ------------------------------------------------------------
india_district_sf <- read_rds(here("files", "india_districts_simplified.rds"))
india_states <- read_rds(here("files", "india_states_simplified.rds"))
india_sf <- read_rds(here("files", "india_sf.rds"))


# Spatial Clustering Analysis --------------------------------------------------

# Function to perform Local Moran's I clustering analysis
cluster_plot_fn <- function(sf, nfhs = "nfhs5") {
  plot_title <- ifelse(nfhs == "nfhs5",
                       "National Family Health Survey - 5",
                       "National Family Health Survey - 4")

  plot_subtitle <- ifelse(nfhs == "nfhs5",
                          "(2019 - 2021)",
                          "(2015 - 2016)")

  # Create queen contiguity weights
  queen_w <- queen_weights(sf)

  # Calculate Local Moran's I
  lisa <- local_moran(queen_w, sf[nfhs])
  lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)

  lisa_labs <- factor(lisa_clusters,
                      levels = 0:(length(lisa$labels) - 1),
                      labels = lisa$labels)

  sf <- sf |>  bind_cols(lisa_clusters = lisa_clusters, lisa_labs = lisa_labs)

  # Create plot
  plot <- sf |> 
    ggplot() +
    geom_sf(aes(fill = lisa_labs), lwd = 0.2) +
    scale_fill_manual(name = "Clustering", values = lisa$colors) +
    ggtitle(plot_title, subtitle = plot_subtitle) +
    theme(legend.position = "right",
          plot.title = element_text(size = 40),
          legend.title = element_text(size = 40, face = "bold"),
          legend.key.size = unit(5, 'cm'),
          legend.text = element_text(size = 40),
          plot.subtitle = element_text(size = 32, face = "italic", color = "black"),
          axis.text.x = element_text(size = 30),
          axis.text.y = element_text(size = 30)) +
    theme_bw()

  return(plot)
}

# Overall Caesarean Births Clustering ------------------------------------------
sf <- sf_births_cs

p4_nfhs4 <- cluster_plot_fn(sf, 'nfhs4')
p4_nfhs5 <- cluster_plot_fn(sf, 'nfhs5')

patch <- p4_nfhs4 + p4_nfhs5 + plot_layout(guides = 'collect')

p4_plot <- patch + plot_annotation(
  title = 'Clustering of c-section births (%) across districts in India',
  caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml).',
  theme = theme(plot.title = element_text(size = 20))
)

p4_plot

ggsave("plots/cluster_dist_overall.png", dpi = 250,
       height = 30, width = 50, scale = 0.2)

# Private Hospitals Clustering ------------------------------------------------
sf <- sf_births_cs_private

p5_nfhs4 <- cluster_plot_fn(sf, 'nfhs4')
p5_nfhs5 <- cluster_plot_fn(sf, 'nfhs5')

patch <- p5_nfhs4 + p5_nfhs5 + plot_layout(guides = 'collect')

p5_plot <- patch + plot_annotation(
  title = 'Clustering of c-section births (%) among private health facilities',
  caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml).',
  theme = theme(plot.title = element_text(size = 20))
)

p5_plot

ggsave("plots/cluster_dist_pvt.png", dpi = 250,
       height = 30, width = 50, scale = 0.2)

# Public Hospitals Clustering -------------------------------------------------
sf <- sf_births_cs_public

p6_nfhs4 <- cluster_plot_fn(sf, 'nfhs4')
p6_nfhs5 <- cluster_plot_fn(sf, 'nfhs5')

patch <- p6_nfhs4 + p6_nfhs5 + plot_layout(guides = 'collect')

p6_plot <- patch + plot_annotation(
  title = 'Clustering of c-section births (%) among public health facilities',
  caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml).',
  theme = theme(plot.title = element_text(size = 20))
)

p6_plot

ggsave("plots/cluster_dist_pub.png", dpi = 250,
       height = 30, width = 50, scale = 0.2)

# Combined Public and Private Clustering Plot ----------------------------------
pp_cluster <- p6_nfhs4 + p6_nfhs5 + p5_nfhs4 + p5_nfhs5 +
  plot_annotation(
    title = 'Clustering of c-section births (%) among a) public and b) private health facilities \n across NFHS surveys',
    caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml). \n Districts with missing values have been represented using the state average.',
    theme = theme(plot.title = element_text(size = 20))
  )

ggsave("plots/pp_cluster.png", dpi = 250,
       height = 50, width = 50, scale = 0.2)

