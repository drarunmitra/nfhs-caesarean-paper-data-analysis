# Exploring Spatial Clusters of Caesarean Sections across India – Insights from NFHS Data

## Overview

This repository contains all data processing scripts, spatial analysis code, and data files for the manuscript "Exploring Spatial Clusters of Caesarean Sections across India – Insights from NFHS Data" submitted to PLOS Global Public Health.

## Study Description

This is the first national-level study to examine district-level spatial clustering of caesarean section (C-section) births in India. Using nationally representative aggregate data from the National Family Health Survey (NFHS-4 and NFHS-5), we analyze spatial patterns across 707 districts to understand:

- **Geographic disparities** in C-section rates across regions
- **Rural-urban differences** in caesarean delivery patterns
- **Public vs private sector variations** in C-section practices
- **Demographic shifts** in C-sections from urban/affluent populations to rural and socioeconomically weaker sections
- **Spatial clustering patterns** using Local Moran's I analysis

This research addresses a critical gap in understanding the spatial epidemiology of the C-section epidemic in India and can inform policy decisions and resource allocation for maternal health services.

## Data Sources

All data were obtained from the National Family Health Survey (NFHS) website:

- **NFHS-4 (2015-2016)**: http://rchiips.org/nfhs/factsheet_NFHS-4.shtml
- **NFHS-5 (2019-2021)**: http://rchiips.org/nfhs/factsheet_NFHS-5.shtml

District and state-level fact sheets were downloaded as PDF files and data were extracted programmatically using R.

## Repository Structure

```
.
├── 00_install_packages.R                           # Package installation script
├── 01_district_download_pdfs_and_scrape_data.R    # Download & extract district data
├── 01_state_download_pdfs_and_scrape_data.R       # Download & extract state data
├── 02_cleaning_and_joining_with_spatial_data.R    # Data cleaning & spatial joins
├── 03_exploratory_data_analysis.R                 # Summary statistics & EDA
├── 04_spatial_analysis.R                          # Local Moran's I clustering
├── 05_figures.R                                   # Generate publication figures
├── data/                                          # Processed datasets (RDS files)
├── files/                                         # Spatial shapefiles & data
├── plots/                                         # Generated plots and figures
└── README.md                                      # This file
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
  - `files/sf_insti_births_overall.rds` - Institutional births
  - `files/sf_insti_births_public_facility.rds` - Public facility births

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

## Methodology

### Spatial Analysis Approach

**Local Moran's I** (Local Indicators of Spatial Association - LISA) is used to identify spatial clustering patterns:

1. **Spatial Weights Matrix**: Queen contiguity weights define neighborhood structure (districts sharing any boundary point are neighbors)
2. **Local Autocorrelation**: Calculates local spatial autocorrelation for each district
3. **Significance Testing**: Uses permutation tests to assess statistical significance
4. **Cluster Classification**: Categorizes districts into cluster types (High-High, Low-Low, High-Low, Low-High, Not Significant)

This approach reveals geographic patterns and hotspots of high/low C-section rates that would not be apparent from non-spatial analyses.

### Data Imputation Strategy

Districts with missing values (primarily due to data unavailability in source PDFs) were imputed using state-level averages from the corresponding NFHS round. This approach:
- Ensures complete geographic coverage for mapping
- Maintains regional patterns
- Provides conservative estimates
- Is clearly documented in figure captions

### Geographic Scope

The analysis encompasses:
- **707 districts** across India
- **36 states and union territories**
- **Two time periods**: NFHS-4 (2015-2016) and NFHS-5 (2019-2021)
- **Population coverage**: Nationally representative data

## Key Indicators Analyzed

1. **Caesarean section births (%)** - Proportion of all deliveries by C-section
2. **C-sections in private facilities (%)** - Among births in private hospitals
3. **C-sections in public facilities (%)** - Among births in government hospitals
4. **Institutional births (%)** - Deliveries in health facilities (public or private)
5. **Births in public facilities (%)** - Proportion of institutional births in government facilities
6. **Skilled birth attendance (%)** - Births attended by trained health personnel

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

**Note**: Script 01 may take several hours to complete due to downloading ~705 PDFs from the NFHS website.

### Quick Start (Using Pre-processed Data):

If you have the cleaned RDS files, you can skip Step 1 and start from Script 02:

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

**Manuscript:**
Mitra A, et al. (2025). Exploring Spatial Clusters of Caesarean Sections across India – Insights from NFHS Data. *PLOS Global Public Health* [Under Review].

**Data Source:**
International Institute for Population Sciences (IIPS) and ICF. National Family Health Survey (NFHS-4 and NFHS-5), India. Mumbai: IIPS.
<http://rchiips.org/nfhs/index.shtml>


## Author

**Dr. Arun Mitra**
Corresponding Author

## License

This repository is made publicly available to support transparency and reproducibility in research. Please cite appropriately if you use these materials.

## Acknowledgments

Data were obtained from the National Family Health Survey (NFHS), conducted by the International Institute for Population Sciences (IIPS), Mumbai, under the stewardship of the Ministry of Health and Family Welfare (MoHFW), Government of India, with technical assistance from ICF International.

Spatial boundary files were obtained from publicly available sources and simplified for computational efficiency.

## Repository DOI

This repository can be archived on Zenodo or figshare to obtain a permanent DOI for citation purposes, as required by PLOS Global Public Health data availability policies.

---

**Last Updated**: December 2025
**Manuscript Status**: Under review at PLOS Global Public Health
