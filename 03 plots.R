#=================================================================================
# 03_PLOTS.R
#
# Wildfire-Induced Building Damage in California — Distribution Plots
#
# Purpose:
#   Produce ridgeline (density) plots of each environmental, structural, and
#   exposure covariate split by damage class, and combine them into
#   publication-style multi-panel figures.
#
# Input:
#   CalDins_upd_extractions_subset_03.csv  (produced by 01_data_prep.R)
#=================================================================================

library(ggplot2)
library(ggridges)
library(cowplot)
library(grid)

# ---------------------------------------------------------------------------
# 0. Resolve data root + load the prepared data frame
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

CalDins.upd_geo_landfire_Building <- read.csv(
  paste0(location, "BuildingDamagePotential/Modellings/CalDins_upd_extractions_subset_03.csv")
)

CalDins.upd_geo_landfire_Building$DAMAGE <- factor(
  CalDins.upd_geo_landfire_Building$Damage,
  levels = c("Inaccessible", "No Damage", "Affected (1-9%)",
             "Minor (10-25%)", "Major (26-50%)", "Destroyed (>50%)")
)

# Manual brown gradient: light (least damage) -> dark (most damage)
brown_palette <- c(
  "Inaccessible"      = "grey",
  "No Damage"         = "#A1887F",
  "Affected (1-9%)"   = "#8D6E63",
  "Minor (10-25%)"    = "#6D4C41",
  "Major (26-50%)"    = "#4E342E",
  "Destroyed (>50%)"  = "#3E2723"
)

# Common trimming theme applied to every ridgeline panel for a tight grid layout
trim_plot <- function(p) {
  p + theme(
    plot.margin    = unit(c(0.1, 0.1, 0.1, 0.1), "cm"),
    axis.text.y    = element_blank(),
    axis.ticks.y   = element_blank(),
    axis.title.y   = element_blank(),
    legend.position = "none"
  )
}

ridgeline_plot <- function(data, x_var, x_label, x_limits = NULL) {
  p <- ggplot(data, aes(x = .data[[x_var]], y = DAMAGE, fill = DAMAGE)) +
    geom_density_ridges(alpha = 0.7, scale = 1) +
    scale_fill_manual(values = brown_palette) +
    labs(x = x_label, y = "", title = "") +
    theme_minimal() +
    theme(
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none"
    )
  if (!is.null(x_limits)) {
    p <- p + scale_x_continuous(limits = x_limits)
  }
  p
}

# ---------------------------------------------------------------------------
# 1. Topography + vegetation structure panels (a-h)
# ---------------------------------------------------------------------------
ele.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "Elevation", "Elevation (m)", c(0, 2500))
slp.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "Slope", expression("Slope ("*degree*")"), c(0, 35))
asp.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "Aspect", expression("Aspect ("*degree*")"), c(0, 360))
tri.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "TRI", "TRI", c(0, 40))
evc.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "EVC", "EVC", c(0, 400))
evh.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "EVH", "EVH", c(0, 320))
evt.p <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "EVT", "EVT")
WUI_Area_sqkm_100m.p <- ridgeline_plot(
  CalDins.upd_geo_landfire_Building, "WUI_Area",
  expression("WUI Area (km"^2*") in 100m"), c(0, 0.1)
)

panel_a_h <- plot_grid(
  trim_plot(ele.p), trim_plot(slp.p), trim_plot(asp.p), trim_plot(tri.p),
  trim_plot(evc.p), trim_plot(evh.p), trim_plot(evt.p), trim_plot(WUI_Area_sqkm_100m.p),
  ncol   = 4,
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)"),
  align  = "hv",
  axis   = "tblr",
  rel_widths  = rep(1, 4),
  rel_heights = rep(1, 2)
)
print(panel_a_h)

# ---------------------------------------------------------------------------
# 2. Human footprint + flammability panels (i-q)
# ---------------------------------------------------------------------------
BuildingDensity.p <- ridgeline_plot(
  CalDins.upd_geo_landfire_Building, "BuildingDensity_scaled",
  "Building Density", c(0, 0.008)
)
btf.p   <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "BTF", "BTF", c(0, 0.03))
btgs.p  <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "BTGS", "BTGS", c(0, 0.1))
frs.p   <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "Frs_Area", "Forest Area", c(0, 0.08))
grs.p   <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "GrsShr_Area", "Grass-Shrub Area", c(0, 0.09))
road.p  <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "RoadLength", "Road Length", c(0, 0.002))
pft.p   <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "PFTk9", "PFT (k9)", c(0, 100))
yob.p   <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "BuildYear", "YoB", c(0, 2022))
cbfi.p  <- ridgeline_plot(CalDins.upd_geo_landfire_Building, "CBFR", "CBFR", c(0, 10))

panel_i_q <- plot_grid(
  trim_plot(BuildingDensity.p), trim_plot(btf.p), trim_plot(btgs.p), trim_plot(frs.p), trim_plot(grs.p),
  trim_plot(road.p), trim_plot(pft.p), trim_plot(yob.p), trim_plot(cbfi.p), NULL,
  ncol   = 5,
  labels = c("(i)", "(j)", "(k)", "(l)", "(m)", "(n)", "(o)", "(p)", "(q)", ""),
  align  = "hv",
  axis   = "tblr",
  rel_widths  = rep(1, 5),
  rel_heights = rep(1, 2)
)
print(panel_i_q)

cat("Plotting complete.\n")