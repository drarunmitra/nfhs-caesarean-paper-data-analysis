# Exploring Spatial Clusters of Caesarean Sections across India – Insights from NFHS Data

## Overview

This repository contains all data processing scripts, spatial analysis code, and data files for the manuscript "Exploring Spatial Clusters of Caesarean Sections across India – Insights from NFHS Data" submitted to PLOS Global Public Health.

## Study Description

This is the first national-level study to examine district-level spatial clustering of caesarean section (C-section) births in India. Using nationally representative aggregate data from the National Family Health Survey (NFHS-4 and NFHS-5), we analyze spatial patterns across 707 districts to understand:

- **Geographic disparities** in C-section rates across regions
- **Rural-urban differences** in caesarean delivery patterns
- **Public vs private sector variations** in C-section practices
- **Spatial clustering patterns** using Global and Local spatial autocorrelation analysis

This research addresses a critical gap in understanding the spatial epidemiology of the C-section epidemic in India and can inform policy decisions and resource allocation for maternal health services.

## Data Sources

Data were obtained from the National Family Health Survey (NFHS), conducted by the International Institute for Population Sciences (IIPS), Mumbai, under the stewardship of the Ministry of Health and Family Welfare (MoHFW), Government of India.

All data were obtained from the National Family Health Survey (NFHS) website:

- **NFHS-4 (2015-2016)**: http://rchiips.org/nfhs/factsheet_NFHS-4.shtml
- **NFHS-5 (2019-2021)**: http://rchiips.org/nfhs/factsheet_NFHS-5.shtml

Spatial boundary files were obtained from publicly available sources and simplified for computational efficiency.

## Repository Structure

```
.
├───code\
│   ├───00_project_setup_template.R
│   ├───01_district_download_pdfs_and_scrape_data.R
│   ├───01_state_download_pdfs_and_scrape_data.R
│   ├───02_cleaning_and_joining_with_spatial_data.R
│   ├───03_exploratory_data_analysis.R
│   ├───04_spatial_analysis.R
│   └───05_figures.R
├───data\
│   ├───cleaned_df_20211130.rds
│   ├───state_level_nfhs3_cleaned_df_20211201.rds
│   ├───state_level_nfhs4_cleaned_df_20211201.rds
│   └───state_level_nfhs5_cleaned_df_20211201.rds
├───files\
│   ├───india_districts_simplified.rds
│   ├───india_sf.rds
│   ├───india_states_simplified.rds
│   ├───sf_births_cs_private.rds
│   ├───sf_births_cs_public.rds
│   ├───sf_births_cs.rds
│   └───zone_df.csv
└───README.md
```

## Installation

### System Requirements

- R version 4.0 or higher
- Java Development Kit (JDK) for PDF table extraction (required by `tabulizer` package)
- Operating System: Windows, macOS, or Linux

### Installing R Packages

Run the package installation script to install all required packages:

```r
source("00_install_packages.R")
```

This will automatically install and load all required packages using `pacman`:

**Core packages:**
- `tidyverse` - Data manipulation and visualization
- `here` - Project-relative file paths
- `sf` - Spatial data handling
- `rgeoda` - Spatial analysis and Local Moran's I

**Data processing:**
- `janitor`, `epitrix`, `gdata` - Data cleaning utilities
- `xml2`, `rvest` - Web scraping
- `pdftools`, `tabulizer` - PDF data extraction
- `downloader` - File downloading

**Visualization:**
- `patchwork` - Combining plots
- `ggpubr`, `ggtext`, `ggstatsplot` - Enhanced plotting
- `gt`, `gtExtras` - Publication-quality tables

#### Package Versions used for Analysis

- downloader (version 0.4; Chang W, 2015)
- extrafont (version 0.20; Chang W, 2025)
- janitor (version 2.2.0; Firke S, 2023)
- lubridate (version 1.9.3; Grolemund G, Wickham H, 2011)
- fs (version 1.6.5; Hester J et al., 2024)
- gt (version 0.11.1; Iannone R et al., 2024)
- tictoc (version 1.2.1; Izrailev S, 2024)
- epitrix (version 0.4.0; Jombart T et al., 2023)
- ggpubr (version 0.6.0; Kassambara A, 2023)
- rgeoda (version 0.0.10.4; Li X, Anselin L, 2023)
- digest (version 0.6.36; Lucas DEwcbA et al., 2024)
- gtExtras (version 0.5.0; Mock T, 2023)
- here (version 1.0.2; Müller K, 2025)
- tibble (version 3.2.1; Müller K, Wickham H, 2023)
- pdftools (version 3.4.1; Ooms J, 2024)
- ggstatsplot (version 0.12.4; Patil I, 2021)
- sf (version 1.0.16; Pebesma E, Bivand R, 2023)
- patchwork (version 1.3.0; Pedersen T, 2024)
- R (version 4.4.1; R Core Team, 2024)
- gdata (version 3.0.1; Warnes GR et al., 2024)
- ggplot2 (version 3.5.1; Wickham H, 2016)
- forcats (version 1.0.0; Wickham H, 2023)
- stringr (version 1.5.1; Wickham H, 2023)
- rvest (version 1.0.4; Wickham H, 2024)
- tidyverse (version 2.0.0; Wickham H et al., 2019)
- usethis (version 3.1.0; Wickham H et al., 2024)
- dplyr (version 1.1.4; Wickham H et al., 2023)
- purrr (version 1.0.2; Wickham H, Henry L, 2023)
- readr (version 2.1.5; Wickham H et al., 2024)
- devtools (version 2.4.5; Wickham H et al., 2022)
- xml2 (version 1.3.6; Wickham H et al., 2023)
- tidyr (version 1.3.1; Wickham H et al., 2024)
- ggridges (version 0.5.6; Wilke C, 2024)
- ggtext (version 0.1.2; Wilke C, Wiernik B, 2022)

### Important Installation Notes

1. **Java Requirement**: The `tabulizer` package requires Java JDK.
   If you encounter issues, download and install Java JDK from:
   https://www.oracle.com/java/technologies/downloads/

   Then install:
   ```r
   install.packages("rJava")
   install.packages("tabulizer")
   ```

2. **Spatial Packages**: The `sf` package may require system dependencies:
   - Ubuntu/Debian: `sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev`
   - macOS: `brew install udunits gdal proj geos`
   - Windows: Usually works out of the box

3. **First-time Installation**: May take 10-15 minutes as it downloads and compiles packages.

## Workflow

### Step 1: Data Acquisition

**Script 1a:** `01_district_download_pdfs_and_scrape_data.R`
- Scrapes NFHS website for district fact sheet URLs
- Downloads ~705 district-level fact sheet PDFs for NFHS-4 and NFHS-5
- Extracts data tables from PDFs using `tabulizer`
- Cleans and standardizes variable names
- **Output:** `data/cleaned_df_20211130.rds` (combined district dataset)

**Script 1b:** `01_state_download_pdfs_and_scrape_data.R`
- Downloads state-level fact sheet PDFs
- Extracts state-level indicators with urban/rural breakdowns
- Processes NFHS-3, NFHS-4, and NFHS-5 state data
- Handles special cases (Delhi, Chandigarh)
- **Output:** `data/state_level_nfhs5_cleaned_df_20211201.rds`

### Step 2: Data Cleaning and Spatial Joining

**Script:** `02_cleaning_and_joining_with_spatial_data.R`
- Fixes data entry errors in state and district names
- Adds manually collected data for Lakshadweep (missing from PDFs)
- Reshapes data from wide to long format
- Harmonizes 100+ district name variations between NFHS data and shapefiles
- Fills missing district values with corresponding state-level averages
- Joins health indicators with spatial boundary geometries
- **Outputs:**
  - `files/sf_births_cs.rds` - Overall C-section rates with geometries
  - `files/sf_births_cs_private.rds` - Private facility C-sections
  - `files/sf_births_cs_public.rds` - Public facility C-sections

### Step 3: Exploratory Data Analysis

**Script:** `03_exploratory_data_analysis.R`
- Generates summary statistics tables by state and geographic zone
- Creates box plots comparing distributions across zones
- Produces descriptive statistics for manuscript tables
- Examines distributions of C-section rates across regions
- **Outputs:** Summary tables (GT format) and exploratory plots saved to `plots/` directory

### Step 4: Spatial Clustering Analysis

**Script:** `04_spatial_analysis.R`
- Constructs queen contiguity spatial weights matrices
- Performs Local Moran's I spatial autocorrelation analysis
- Identifies statistically significant spatial clusters:
  - **High-High**: Districts with high C-section rates surrounded by similar high-rate neighbors
  - **Low-Low**: Districts with low rates surrounded by similar low-rate neighbors
  - **High-Low**: Outliers with high rates surrounded by low-rate neighbors
  - **Low-High**: Outliers with low rates surrounded by high-rate neighbors
  - **Not Significant**: Districts without significant spatial autocorrelation
- Analyzes clustering for overall rates, public facilities, and private facilities
- **Outputs:** Cluster classification maps saved to `plots/` directory

### Step 5: Publication Figures

**Script:** `05_figures.R`
- Generates final publication-ready figures in EPS format (300 dpi)
- Creates multi-panel comparison figures for NFHS-4 vs NFHS-5
- Applies consistent color schemes and formatting
- **Outputs:**
  - `figure_2.eps` - Choropleth maps of overall C-section rates
  - `figure_3.eps` - Public vs private facility C-section maps
  - `figure_4.eps` - Spatial clustering patterns for overall C-sections
  - `figure_5.eps` - Spatial clustering for public vs private facilities

## Reproducibility

This analysis is fully reproducible. All scripts use the `here::here()` function for project-relative file paths, ensuring code runs regardless of working directory.

### To Reproduce the Full Analysis:

1. **Install packages**: `source("00_install_packages.R")`
2. **Run scripts sequentially**:
   ```r
   source("01_district_download_pdfs_and_scrape_data.R")
   source("01_state_download_pdfs_and_scrape_data.R")
   source("02_cleaning_and_joining_with_spatial_data.R")
   source("03_exploratory_data_analysis.R")
   source("04_spatial_analysis.R")
   source("05_figures.R")
   ```
3. **Requirements**:
   - Internet connectivity for PDF downloads (Step 1)
   - Sufficient disk space (~2 GB for PDFs and intermediate files)
   - Java JDK installed for `tabulizer` package

**Note**: Scripts starting with `01_*` may take several hours to complete due to downloading 700+ PDFs from the NFHS website.

### Quick Start (Using Pre-processed Data):

You can use the cleaned RDS files to skip Step 1 and start from Script 02:

```r
source("02_cleaning_and_joining_with_spatial_data.R")
source("03_exploratory_data_analysis.R")
source("04_spatial_analysis.R")
source("05_figures.R")
```

## Key Findings

This study represents the first national-level examination of district-level spatial clustering of C-sections in India. Key contributions include:

- Documentation of spatial disparities in C-section rates across districts of India
- Identification of geographic hotspots (High-High clusters) and coldspots (Low-Low clusters)
- Analysis of differential patterns between public and private healthcare sectors
- Evidence of demographic shifts in C-section prevalence from urban to rural areas
- Insights for targeted policy interventions and resource allocation

## Citation

If you use this code or data, please cite:

Arun Mitra. (2025). Computational Workflow 0.1.0: drarunmitra/nfhs-caesarean-paper-data-analysis. Zenodo. https://doi.org/10.5281/zenodo.17789152

```md
@software{arun_mitra_2025_17789152,
  author       = {Arun Mitra},
  title        = {Computational Workflow 0.1.0: drarunmitra/nfhs-caesarean-paper-data-analysis
                  },
  month        = dec,
  year         = 2025,
  publisher    = {Zenodo},
  version      = {computational-workflow},
  doi          = {10.5281/zenodo.17789152},
  url          = {https://doi.org/10.5281/zenodo.17789152},
  swhid        = {swh:1:dir:f50ea925b6d689a9684106b9cdbd64ec07e5e263
                   ;origin=https://doi.org/10.5281/zenodo.17789151;vi
                   sit=swh:1:snp:e99c859aef1ec1495b38ebadca99ce160223
                   2df5;anchor=swh:1:rel:dcbd396060b98b17bad2891f35d3
                   452661bb03e0;path=drarunmitra-nfhs-caesarean-
                   paper-data-analysis-0e3a920
                  },
}

```


## Author

**Dr. Arun Mitra**

Corresponding Author

## License

This repository is made publicly available to support transparency and reproducibility in research. Please cite appropriately if you use these materials.


## Repository DOI

<https://doi.org/10.5281/zenodo.17789152>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17789152.svg)](https://doi.org/10.5281/zenodo.17789152)



