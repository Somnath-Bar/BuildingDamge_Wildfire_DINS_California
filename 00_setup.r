#' ===============================================================================
#' California Wildfire Building Damage Potential Analysis
#' ===============================================================================
#' Purpose: Setup script for data paths, libraries, and configurations
#' Author: [Somnath Bar]
#' Date: 2026-02-03
#' Paper: Science Advances - Wildfire Building Damage Prediction
#' ===============================================================================

# ---- Load Required Libraries ----
suppressPackageStartupMessages({
  library(sp)
  library(raster)
  library(sf)
  library(terra)
  library(dplyr)
  library(ggplot2)
  library(randomForest)
  library(caret)
  library(shapviz)
  library(corrplot)
  library(ggridges)
  library(RColorBrewer)
  library(DMwR)
  library(FSA)
})

# ---- Set Working Directory ----
detect_os_path <- function() {
  mac_path <- '~/Library/CloudStorage/OneDrive-UCIrvine/'
  win_path <- 'C:/Users/Somnath_UCI/OneDrive - UC Irvine/'
  
  if (file.exists(win_path)) {
    return(win_path)
  } else if (file.exists(mac_path)) {
    return(mac_path)
  } else {
    stop("Error: No valid path found. Update paths in setup.R")
  }
}

# Global path variable
BASE_PATH <- detect_os_path()
cat("✓ Working directory set to:", BASE_PATH, "\n")

# ---- Define Project Paths ----
PATHS <- list(
  shapes = file.path(BASE_PATH, "Shp"),
  building_data = file.path(BASE_PATH, "BuildingDamagePotential"),
  landfire = file.path(BASE_PATH, "BuildingDamagePotential/Landfire"),
  climate = file.path(BASE_PATH, "BuildingDamagePotential/FireDamageClimateData"),
  models = file.path(BASE_PATH, "BuildingDamagePotential/Modellings"),
  results = file.path(BASE_PATH, "BuildingDamagePotential/MS/V01"),
  figures = file.path(BASE_PATH, "BuildingDamagePotential/MS/V01/Figures"),
  tables = file.path(BASE_PATH, "BuildingDamagePotential/MS/V01/Tables")
)

# Create output directories if they don't exist
invisible(lapply(PATHS, function(p) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}))

# ---- Load Boundary Shapefiles ----
cal_shp <- shapefile(file.path(PATHS$shapes, "California_shapefile.shp"))
cal_counties <- shapefile(file.path(PATHS$shapes, "CA_Counties.shp"))

cat("✓ Setup complete\n")