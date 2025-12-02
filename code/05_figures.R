################################################################################
# Script: 05_figures.R
# Purpose: Generate final publication-ready figures for manuscript submission
# Author: [Your Name]
# Date: 2021-12-01
################################################################################

# Load Required Packages -------------------------------------------------------
library(tidyverse)
library(here)
library(sf)
library(ggpubr)
library(rgeoda)
library(patchwork)
library(ggtext)

# Load Spatial Data ------------------------------------------------------------
# Created in 02_cleaning_and_joining_with_spatial_data.R script
sf_births_cs <- read_rds(here('files', 'sf_births_cs.rds'))
sf_births_cs_private <- read_rds(here('files', 'sf_births_cs_private.rds'))
sf_births_cs_public <- read_rds(here('files', 'sf_births_cs_public.rds'))

india_states <- read_rds(here("files", "india_states_simplified.rds"))
india_sf <- read_rds(here("files", "india_sf.rds"))

# Define Clustering Function ---------------------------------------------------

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

  sf <- sf |> bind_cols(lisa_clusters = lisa_clusters, lisa_labs = lisa_labs)

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

cluster_plot_fn_new <- function(sf, nfhs = "nfhs5") {
  # Simplified version without titles for combined plots
  queen_w <- queen_weights(sf)
  lisa <- local_moran(queen_w, sf[nfhs])
  lisa_colors <- lisa_colors(lisa)
  lisa_labels <- lisa_labels(lisa)
  lisa_clusters <- lisa_clusters(lisa)

  lisa_labs <- factor(lisa_clusters,
                      levels = 0:(length(lisa$labels) - 1),
                      labels = lisa$labels)

  sf <- sf |> bind_cols(lisa_clusters = lisa_clusters, lisa_labs = lisa_labs)

  plot <- sf |>
    ggplot() +
    geom_sf(aes(fill = lisa_labs), lwd = 0.2) +
    scale_fill_manual(name = "Clustering", values = lisa$colors) +
    theme_bw()

  return(plot)
}

# Figure 2: Overall C-Section Rates (NFHS-4 vs NFHS-5) ------------------------

p1_nfhs4 <- sf_births_cs |>
  ggplot() +
  geom_sf(aes(fill = nfhs4), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-4", subtitle = "(2015 - 2016)") +
  theme(legend.position = "right",
        plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 40, face = "bold"),
        legend.key.size = unit(5, 'cm'),
        legend.text = element_text(size = 40),
        plot.subtitle = element_text(size = 32, face = "italic", color = "black"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30)) +
  labs(fill = "Proportion (%)") +
  theme_bw()

p1_nfhs5 <- sf_births_cs |>
  ggplot() +
  geom_sf(aes(fill = nfhs5), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-5", subtitle = "(2019 - 2021)") +
  theme(legend.position = "right",
        plot.title = element_text(size = 40, face = "bold"),
        legend.title = element_text(size = 40, face = "bold"),
        legend.key.size = unit(5, 'cm'),
        legend.text = element_text(size = 40),
        plot.subtitle = element_text(size = 32, face = "italic", color = "black"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30)) +
  labs(fill = "Proportion (%)") +
  theme_bw()

patch <- p1_nfhs4 + p1_nfhs5 + plot_layout(guides = 'collect')

p1_plot <- patch + plot_annotation(
  title = 'Proportion of c-section births (%) among districts of India',
  caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml). \n Districts with missing values have been represented using the state average.',
  theme = theme(plot.title = element_text(size = 20))
)

p1_plot

# Save Figure 2
p1_plot |>
  ggsave(filename = here('figure_2.eps'),
         dpi = 300, width = 10, height = 8)

# Figure 3: Public vs Private Health Facilities --------------------------------

# Public facilities
p2_nfhs4_public <- sf_births_cs_public |>
  ggplot() +
  geom_sf(aes(fill = nfhs4), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-4", subtitle = "(2015 - 2016)") +
  labs(fill = "Proportion (%)") +
  theme_bw() +
  theme(legend.position = "right")

p2_nfhs5_public <- sf_births_cs_public |>
  ggplot() +
  geom_sf(aes(fill = nfhs5), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-5", subtitle = "(2019 - 2021)") +
  theme_bw() +
  theme(legend.position = "right") +
  labs(fill = "Proportion (%)")

p2_public <- (wrap_elements(p2_nfhs4_public) + wrap_elements(p2_nfhs5_public))

p2_public <- p2_public + plot_annotation(
  title = '(A) Public Health Facilities',
  theme = theme(plot.title = element_text(size = 20))
)

# Private facilities
p2_nfhs4_private <- sf_births_cs_private |>
  ggplot() +
  geom_sf(aes(fill = nfhs4), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-4", subtitle = "(2015 - 2016)") +
  labs(fill = "Proportion (%)") +
  theme_bw() +
  theme(legend.position = "right")

p2_nfhs5_private <- sf_births_cs_private |>
  ggplot() +
  geom_sf(aes(fill = nfhs5), lwd = 0.2) +
  scale_fill_fermenter(palette = "PuBuGn", direction = 1, limits = c(0, 100)) +
  geom_sf(data = india_states, aes(), alpha = 0, lwd = 0.3) +
  geom_sf(data = india_sf, aes(), alpha = 0, lwd = 0.4) +
  ggtitle("NFHS-5", subtitle = "(2019 - 2021)") +
  theme_bw() +
  theme(legend.position = "right") +
  labs(fill = "Proportion (%)")

p2_private <- (wrap_elements(p2_nfhs4_private) + wrap_elements(p2_nfhs5_private))

p2_private <- p2_private + plot_annotation(
  title = '(B) Private Health Facilities',
  theme = theme(plot.title = element_text(size = 20))
)

# Combine public and private
p2_plot <- wrap_elements(p2_public) / wrap_elements(p2_private)

p2_plot <- p2_plot + plot_annotation(
  title = "Proportion of C-Section births across districts in India"
)

# Save Figure 3
p2_plot |>
  ggsave(filename = here('figure_3.eps'),
         dpi = 300, width = 10, height = 8)

# Figure 4: Overall Clustering -------------------------------------------------

sf <- sf_births_cs

p4_nfhs4 <- cluster_plot_fn(sf, 'nfhs4')
p4_nfhs5 <- cluster_plot_fn(sf, 'nfhs5')

patch <- p4_nfhs4 + p4_nfhs5 + plot_layout(guides = 'collect')

p4_plot <- patch + plot_annotation(
  title = 'Clustering of C-Section Births (%) across districts in India',
  caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml).',
  theme = theme(plot.title = element_text(size = 20))
)

p4_plot

# Save Figure 4
p4_plot |>
  ggsave(filename = here('figure_4.eps'),
         dpi = 300, width = 10, height = 8)

# Figure 5: Clustering by Facility Type ----------------------------------------

# Public facilities clustering
sf <- sf_births_cs_public

p5_nfhs4_public <- cluster_plot_fn_new(sf, 'nfhs4')
p5_nfhs5_public <- cluster_plot_fn_new(sf, 'nfhs5')

p5_public <- p5_nfhs4_public + p5_nfhs5_public + plot_layout(guides = 'collect')

p5_public <- p5_public + plot_annotation(
  title = '(A) Public Health Facilities',
  theme = theme(plot.title = element_text(size = 20))
)

# Private facilities clustering
sf <- sf_births_cs_private

p5_nfhs4_private <- cluster_plot_fn_new(sf, 'nfhs4')
p5_nfhs5_private <- cluster_plot_fn_new(sf, 'nfhs5')

p5_private <- p5_nfhs4_private + p5_nfhs5_private + plot_layout(guides = 'collect')

p5_private <- p5_private + plot_annotation(
  title = '(B) Private Health Facilities',
  theme = theme(plot.title = element_text(size = 20))
)

# Combine public and private clustering
p5 <- wrap_elements(p5_public) / wrap_elements(p5_private)

p5 <- p5 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = 'Clustering of C-Section Births (%) across districts in India',
    caption = 'Data source: \n NFHS website (http://rchiips.org/nfhs/index.shtml)\n Districts with missing values have been represented using the state average.',
    theme = theme(plot.title = element_text(size = 22, face = "bold"))
  )

# Save Figure 5
p5 |>
  ggsave(filename = here('figure_5.eps'),
         dpi = 300, width = 10, height = 8)
