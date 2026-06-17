#=================================================================================
# 01_DATA_PREPARATION.R
#
# Wildfire-Induced Building Damage in California — Data Preparation
#
# Purpose:
#   Load the CAL FIRE Damage Inspection (DINS) records, extract co-located
#   environmental covariates (LANDFIRE topography/vegetation, land-cover
#   fractions, building density, road density, WUI area, Composite Building
#   Flammability Rating (CBFR), and daily weather), and assemble the
#   analysis-ready data frame used in 02_statistical_analysis.R and
#   03_plots.R.
#=================================================================================

library(sp)
library(raster)
library(sf)
library(terra)
library(dplyr)

# ---------------------------------------------------------------------------
# 1. Resolve the data root directory (machine-dependent OneDrive path)
# ---------------------------------------------------------------------------
mac_path <- "~/Library/CloudStorage/OneDrive-UCIrvine/"
win_path <- "C:/Users/Somnath_UCI/OneDrive - UC Irvine/"

if (file.exists(win_path)) {
  location <- win_path
} else if (file.exists(mac_path)) {
  location <- mac_path
} else {
  stop("No valid data root path found. Update mac_path/win_path above.")
}

print(location)

# ---------------------------------------------------------------------------
# 2. Boundary polygons (California state + counties)
# ---------------------------------------------------------------------------
cal_shp    <- shapefile(paste0(location, "Shp/California_shapefile.shp"))
cal_shp.co <- shapefile(paste0(location, "Shp/CA_Counties.shp"))

# ---------------------------------------------------------------------------
# 3. Composite Building Flammability Rating (CBFR)
#    (computed in a separate sourced script; produces `CalDins.upd_CBFR`)
# ---------------------------------------------------------------------------
CBFR_path <- paste0(location, "R/CBFR.R")
source(CBFR_path)

CalDins.upd_CBFR <- CalDins.upd[47:length(CalDins.upd)]

# ---------------------------------------------------------------------------
# 4. Load CAL FIRE Damage Inspection (DINS) postfire master shapefile
# ---------------------------------------------------------------------------
CalDins.upd <- shapefile(paste0(location, "BuildingDamagePotential/POSTFIRE_MASTER_DATA/POSTFIRE.shp"))

CalDins.upd <- subset(CalDins.upd, HAZARDTYPE == "Fire")
CalDins.upd@data$FIRENAME[is.na(CalDins.upd@data$FIRENAME)] <- "Fire Name Not-Defined"

# Damage class frequency / percentage table
damage_counts      <- table(CalDins.upd@data$DAMAGE)
damage_percentages <- round(prop.table(damage_counts) * 100, 1)
print(damage_percentages)

# Derive year / date fields
CalDins.upd$Year         <- substr(CalDins.upd$INCIDENTST, 1, 4)
CalDins.upd$InStDate     <- as.Date(substr(CalDins.upd$INCIDENTST, 1, 10))
CalDins.upd$YearofDamage <- as.numeric(substr(CalDins.upd$INCIDENTST, 1, 4))

# ---------------------------------------------------------------------------
# 5. Reproject DINS points to match LANDFIRE raster CRS, then to WGS84
# ---------------------------------------------------------------------------
CalDins.upd_geo <- CalDins.upd

Landfire.list <- list.files(
  path = paste0(location, "/BuildingDamagePotential/Landfire"),
  pattern = "tif$", full.names = TRUE, recursive = TRUE
)
Landfire_stack <- stack(Landfire.list[3:14])

CalDins.upd_geo.t <- spTransform(CalDins.upd_geo, CRSobj = crs(Landfire_stack[[1]]))

# ---------------------------------------------------------------------------
# 6. Extract LANDFIRE topography / vegetation covariates at DINS locations
# ---------------------------------------------------------------------------
CalDins.upd_geo_landfire <- extract(
  Landfire_stack, CalDins.upd_geo.t,
  method = "simple", df = TRUE
)

shapefile_attributes      <- CalDins.upd_geo.t@data
CalDins.upd_geo_landfire_ <- cbind(shapefile_attributes, CalDins.upd_geo_landfire)

# ---------------------------------------------------------------------------
# 7. Extract Plant Functional Type (PFT) land-cover fraction (k=99)
# ---------------------------------------------------------------------------
CalDins.upd_geo.wgs <- spTransform(
  CalDins.upd_geo.t,
  CRSobj = "+proj=longlat +datum=WGS84 +no_defs"
)

coords    <- coordinates(CalDins.upd_geo.wgs)
Latitude  <- coords[, 2]
Longitude <- coords[, 1]

list.pfts <- list.files(
  path = paste0(location, "BuildingDamagePotential/PFT_LC/NLCD_2019"),
  pattern = "NLCD_2019_PFT*.*tif", full.names = TRUE
)
PFTR_K99 <- raster(list.pfts[4])

Ext_PFTR_K99 <- extract(PFTR_K99, CalDins.upd_geo.wgs, method = "simple", df = TRUE)
PFT_DF       <- data.frame(Ext_PFTR_K99[2])
names(PFT_DF) <- "PFT_k99"

# ---------------------------------------------------------------------------
# 8. Extract building density, forest/grass-shrub area, road density, WUI area
# ---------------------------------------------------------------------------
BuildingDensity <- raster(paste0(location, "BuildingDamagePotential/California building footprint/Density_/BuildingDensity300m.tif"))
BuildingForest  <- raster(paste0(location, "BuildingDamagePotential/California building footprint/Density_/BuildingArea_Forest_Area.tif"))
BuildingGrsShu  <- raster(paste0(location, "BuildingDamagePotential/California building footprint/Density_/BuildingArea_GrsShu_Area.tif"))

Forest_Area <- raster(paste0(location, "BuildingDamagePotential/PFT_LC/NLCD_2019/Forest_Area.tif"))
GrsShu_Area <- raster(paste0(location, "BuildingDamagePotential/PFT_LC/NLCD_2019/GrsShu_Area.tif"))

RoadLength_ <- raster(paste0(location, "BuildingDamagePotential/tl_rd22_06001_roads/RoadNetwork_Length.tif"))
RoadLength_ <- resample(RoadLength_, GrsShu_Area, method = "ngb")

PFT_Buildings_   <- stack(BuildingDensity, BuildingForest, BuildingGrsShu, Forest_Area, GrsShu_Area, RoadLength_)
PFT_Buildings_r  <- resample(PFT_Buildings_, PFTR_K99, method = "ngb")
PFT_Buildings_r  <- stack(PFT_Buildings_r, PFTR_K99)

WUI_Area    <- raster(paste0(location, "BuildingDamagePotential/WUI_Area_100mgrid/WUI_Area_sqkm_100m.tif"))
WUI_Area_df <- extract(WUI_Area, CalDins.upd_geo.wgs, method = "simple", df = TRUE)
names(WUI_Area_df) <- c("IDs", "WUI_Area_sqkm_100m")
WUI_Area_df$WUI_Area_sqkm_100m[is.na(WUI_Area_df$WUI_Area_sqkm_100m)] <- 0

Buildings_df <- extract(PFT_Buildings_r, CalDins.upd_geo.wgs, method = "simple", df = TRUE)
Buildings_df <- Buildings_df[2:8]
names(Buildings_df) <- c("BuildingDensity", "BTF", "BTGS", "Frs_Area", "GrsShr_Area", "RoadLength", "PFT_K99")

Buildings_df$BTF[is.na(Buildings_df$BTF)]                         <- 0
Buildings_df$BTGS[is.na(Buildings_df$BTGS)]                       <- 0
Buildings_df$BuildingDensity[is.na(Buildings_df$BuildingDensity)] <- 0
Buildings_df$RoadLength[is.na(Buildings_df$RoadLength)]           <- 0

# ---------------------------------------------------------------------------
# 9. Year of build, damage label, CBFR — assemble combined covariate table
# ---------------------------------------------------------------------------
YoB <- data.frame(CalDins.upd_geo.wgs@data$YEARBUILT)
names(YoB) <- "YoB"

CalDins.upd_geo_landfire$DAMAGE <- CalDins.upd_geo.wgs@data$DAMAGE

CalDins.upd_geo_landfire_Building <- cbind(
  CalDins.upd_geo_landfire,
  WUI_Area_df[2],
  Buildings_df,
  CalDins.upd_CBFR,
  YoB
)

# ---------------------------------------------------------------------------
# 10. Merge in extracted daily-weather covariates (precip, RH, wind, etc.)
# ---------------------------------------------------------------------------
clim_ext <- read.csv(paste0(location, "BuildingDamagePotential/FireDamageClimateData/FireDamage_ClimateData_Export_01.csv"))

CalDins.upd_geo_landfire_Building <- merge.data.frame(
  CalDins.upd_geo_landfire_Building, clim_ext,
  by = "ID"
)

# ---------------------------------------------------------------------------
# 11. Build the final modeling-ready subset with clean column names
# ---------------------------------------------------------------------------
# Note: column indices below mirror the original extraction order and should
# be re-checked with names(CalDins.upd_geo_landfire_Building) if the upstream
# extraction steps change.
Subset_DINsImageCat_landfire <- data.frame(
  CalDins.upd_geo_landfire_Building[1:4],
  CalDins.upd_geo_landfire_Building[13],
  CalDins.upd_geo_landfire_Building[6:8],
  CalDins.upd_geo_landfire_Building[14:15],
  CalDins.upd_geo_landfire_Building[50],
  CalDins.upd_geo_landfire_Building[17:22],
  CalDins.upd_geo_landfire_Building[33:34],
  CalDins.upd_geo_landfire_Building[40:49],
  CalDins.upd_geo_landfire_Building[51:53]
)

names(Subset_DINsImageCat_landfire) <- c(
  "IDs", "Aspect", "Elevation", "Slope", "TRI", "EVC", "EVH", "EVT",
  "Damage", "WUI_Area", "BuildingDensity", "BTF", "BTGS", "Frs_Area",
  "GrsShr_Area", "RoadLength", "PFTk9",
  "CBFR", "BuildYear", "prcp", "MaxRH", "MinRH",
  "Srad", "tdmean", "WD", "Tmin", "Tmax", "VPD", "WS10m", "Firename",
  "Longitude", "Latitude"
)

Subset_DINsImageCat_landfire_ <- Subset_DINsImageCat_landfire

# Replace structural NAs (true absence of feature, e.g. no buildings/roads
# in the pixel) with 0
Subset_DINsImageCat_landfire_$BuildingDensity[is.na(Subset_DINsImageCat_landfire_$BuildingDensity)] <- 0
Subset_DINsImageCat_landfire_$Frs_Area[is.na(Subset_DINsImageCat_landfire_$Frs_Area)]                <- 0
Subset_DINsImageCat_landfire_$GrsShr_Area[is.na(Subset_DINsImageCat_landfire_$GrsShr_Area)]          <- 0
Subset_DINsImageCat_landfire_$RoadLength[is.na(Subset_DINsImageCat_landfire_$RoadLength)]            <- 0
Subset_DINsImageCat_landfire_$BTF[is.na(Subset_DINsImageCat_landfire_$BTF)]                          <- 0
Subset_DINsImageCat_landfire_$BTGS[is.na(Subset_DINsImageCat_landfire_$BTGS)]                        <- 0

Subset_DINsImageCat_landfire_$Damage01 <- ifelse(Subset_DINsImageCat_landfire_$Damage == "No Damage", 0, 1)

Subset_DINsImageCat_landfire_$Firename[is.na(Subset_DINsImageCat_landfire_$Firename)] <- "No Fire Name Found"

# ---------------------------------------------------------------------------
# 12. Scale BuildingDensity to the [0, 0.0121] reference range used downstream
# ---------------------------------------------------------------------------
Subset_DINsImageCat_landfire_$BuildingDensity_scaled <-
  (Subset_DINsImageCat_landfire_$BuildingDensity / 0.493915) * 0.0121

# ---------------------------------------------------------------------------
# 13. Write the analysis-ready table to disk
# ---------------------------------------------------------------------------
out_path <- paste0(location, "BuildingDamagePotential/Modellings/CalDins_upd_extractions_subset_03.csv")
write.csv(Subset_DINsImageCat_landfire_, out_path, row.names = FALSE, quote = FALSE)

cat("Data preparation complete.\n")
cat("Rows written:", nrow(Subset_DINsImageCat_landfire_), "\n")
cat("Saved to:", out_path, "\n")