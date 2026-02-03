#' ===============================================================================
#' Data Preprocessing: Load and Clean DINS Building Damage Data
#' ===============================================================================

source("src/00_setup.R")

#' Load POSTFIRE DINS Data
#' @return SpatialPointsDataFrame with cleaned damage records
load_dins_data <- function() {
  
  cat("Loading POSTFIRE damage data...\n")
  
  # Load shapefile
  dins <- shapefile(file.path(PATHS$building_data, 
                              "POSTFIRE_MASTER_DATA/POSTFIRE.shp"))
  
  # Filter for fire events only
  dins <- subset(dins, HAZARDTYPE == 'Fire')
  
  # Extract year from incident date
  dins$Year <- as.numeric(substr(dins$INCIDENTST, 1, 4))
  dins$InStDate <- as.Date(substr(dins$INCIDENTST, 1, 10))
  
  # Handle missing fire names
  dins$FIRENAME[is.na(dins$FIRENAME)] <- 'Fire Name Not-Defined'
  
  # Create binary damage variable
  dins$Damage01 <- ifelse(dins$DAMAGE == "No Damage", 0, 1)
  
  cat(sprintf("✓ Loaded %d damage records (%d fires)\n", 
              nrow(dins), length(unique(dins$FIRENAME))))
  
  return(dins)
}

#' Convert aspect to Northness and Eastness components
#' @param aspect Numeric vector of aspect values (0-360°)
#' @return Data frame with Northness and Eastness
calculate_aspect_components <- function(aspect) {
  data.frame(
    Northness = cos(aspect * pi / 180),
    Eastness = sin(aspect * pi / 180)
  )
}

#' Extract environmental predictors for building locations
#' @param points SpatialPointsDataFrame of building locations
#' @return Data frame with extracted environmental variables
extract_environmental_predictors <- function(points) {
  
  cat("Extracting environmental predictors...\n")
  
  # Load raster stacks
  landfire <- brick(file.path(PATHS$landfire, "Final_Stack_100m.tif"))
  
  # Extract values
  env_data <- extract(landfire, points, df = TRUE)
  
  # Handle missing values
  env_data$BuildingDensity[is.na(env_data$BuildingDensity)] <- 0
  env_data$BTF[is.na(env_data$BTF)] <- 0
  env_data$Frs_Area[is.na(env_data$Frs_Area)] <- 0
  env_data$GrsShr_Area[is.na(env_data$GrsShr_Area)] <- 0
  env_data$RoadLength[is.na(env_data$RoadLength)] <- 0
  env_data$WUI_Area[is.na(env_data$WUI_Area)] <- 0
  
  cat(sprintf("✓ Extracted %d predictors for %d locations\n", 
              ncol(env_data)-1, nrow(env_data)))
  
  return(env_data)
}

# ---- Execute Preprocessing ----
if (!exists("dins_data")) {
  dins_data <- load_dins_data()
}