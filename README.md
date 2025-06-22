# Paleoecology
# Biodiversity Analysis of As-Sahabi Fossil Localities

This R script performs a comprehensive biodiversity analysis of mammalian fossil data from the As-Sahabi region, comparing taxonomic composition, diversity, and structure across different rock units (T, U1, U2, V).

## Analysis Overview

1. **Data Characterization**: Calculates percentage representation of different taxonomic levels in the dataset
2. **Taxonomic Composition**:
   - Pie charts showing relative abundance of taxa by rock unit
   - Bar plots of carnivoran family distributions
3. **Taxonomic Diversity**:
   - Beta diversity analysis using iNEXT.beta3D
   - Dissimilarity metrics visualization
4. **Community Structure**:
   - UPGMA clustering of localities and rock units
   - NMDS ordination
   - Rank abundance analysis
5. **Spatial Analysis**: Fossil locality mapping
6. **Turnover Analysis**: Beta diversity partitioning (turnover vs nestedness)

## Required Packages

The script uses the following R packages:

```r
library(tidyverse)      # Data manipulation and visualization
library(indicspecies)   # Indicator species analysis
library(iNEXT.beta3D)   # Diversity analysis
library(ggpubr)         # Plot arrangement
library(openxlsx)       # Excel file handling
library(FactoMineR)     # Multivariate analysis
library(factoextra)     # Visualization for multivariate results
library(vegan)          # Ecological diversity analysis
library(reshape2)       # Data reshaping
library(pvclust)        # Cluster analysis with p-values
library(dunn.test)      # Post-hoc Dunn's tests
library(ggrepel)        # Label positioning in plots
library(metan)          # Specialized plotting (ggpie)
library(officer)        # Word document generation
library(flextable)      # Tables for Word documents
library(sf)             # Spatial data handling
library(betapart)       # Beta diversity partitioning
