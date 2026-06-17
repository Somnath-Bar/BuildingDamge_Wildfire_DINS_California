#=================================================================================
# 02_STATISTICAL_ANALYSIS.R
#
# Wildfire-Induced Building Damage in California — Statistical Analysis
#
# Purpose:
#   Run non-parametric group-comparison tests (Kruskal-Wallis + Dunn's
#   post-hoc), pairwise correlation analysis, and PCA on the environmental,
#   structural, and weather covariates associated with building damage class.
#
# Input:
#   CalDins_upd_extractions_subset_03.csv  (produced by 01_data_prep.R)
#=================================================================================

library(dplyr)
library(FSA)         # dunnTest
library(pbapply)      # progress-bar lapply
library(corrplot)
library(RColorBrewer)
library(FactoMineR)   # PCA
library(factoextra)   # PCA visualization

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

# Order damage classes consistently for all downstream tests/plots
CalDins.upd_geo_landfire_Building$DAMAGE <- factor(
  CalDins.upd_geo_landfire_Building$Damage,
  levels = c("Inaccessible", "No Damage", "Affected (1-9%)",
             "Minor (10-25%)", "Major (26-50%)", "Destroyed (>50%)")
)

# ---------------------------------------------------------------------------
# 1. Kruskal-Wallis tests across damage classes for each covariate
# ---------------------------------------------------------------------------
target_vars <- c(
  "Aspect", "Elevation", "Slope", "TRI",
  "EVC", "EVH", "EVT",
  "WUI_Area", "BuildingDensity", "BTGS", "BTF",
  "Frs_Area", "GrsShr_Area",
  "RoadLength", "PFTk9", "BuildYear", "CBFR"
)

get_kw_results <- function(var_name, data) {
  kw <- kruskal.test(reformulate("DAMAGE", var_name), data = data)
  data.frame(
    Variable    = var_name,
    Chi_squared = round(kw$statistic, 3),
    df          = kw$parameter,
    p_value     = ifelse(kw$p.value < 0.001, "<0.001", format.pval(kw$p.value, digits = 3))
  )
}

kw_results <- lapply(
  target_vars, get_kw_results,
  data = CalDins.upd_geo_landfire_Building
) %>% bind_rows()

write.csv(
  kw_results,
  paste0(location, "BuildingDamagePotential/MS/V01/Tables/kruskal_wallis_results_01.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 2. Dunn's test (Bonferroni-corrected) post-hoc pairwise comparisons
# ---------------------------------------------------------------------------
get_dunn_results <- function(var_name, data) {
  dunn <- FSA::dunnTest(
    reformulate("DAMAGE", var_name),
    data = data, method = "bonferroni"
  )
  
  dunn$res %>%
    select(Comparison, Z, P.adj) %>%
    mutate(
      Variable = var_name,
      Z        = round(Z, 3),
      P.adj    = if_else(P.adj < 0.001, "<0.001", as.character(round(P.adj, 3)))
    ) %>%
    select(Variable, Comparison, Z, P.adj)
}

dunn_results <- pblapply(
  target_vars, get_dunn_results,
  data = CalDins.upd_geo_landfire_Building
) %>% bind_rows()

# Comparisons involving "Destroyed (>50%)", excluding "Inaccessible"
destroyed_comparisons <- dunn_results %>%
  filter(grepl("Destroyed \\(>50%\\)", Comparison)) %>%
  filter(!grepl("Inaccessible", Comparison)) %>%
  arrange(Variable, Comparison)

write.csv(
  destroyed_comparisons,
  paste0(location, "BuildingDamagePotential/MS/V01/Tables/dunn_test_results_01.csv"),
  row.names = FALSE
)

# ---------------------------------------------------------------------------
# 3. Dunn's Z-score heatmap (Comparison x Variable), with significance stars
# ---------------------------------------------------------------------------
dune_test <- destroyed_comparisons %>%
  mutate(sig = ifelse(P.adj == "<0.001" | suppressWarnings(as.numeric(P.adj)) < 0.05, "*", ""))

z_matrix <- dune_test %>%
  select(Comparison, Variable, Z) %>%
  tidyr::pivot_wider(names_from = Variable, values_from = Z, values_fill = 0) %>%
  tibble::column_to_rownames("Comparison") %>%
  as.matrix()

sig_matrix <- dune_test %>%
  select(Comparison, Variable, sig) %>%
  tidyr::pivot_wider(names_from = Variable, values_from = sig, values_fill = "") %>%
  tibble::column_to_rownames("Comparison") %>%
  as.matrix()

# Short row labels for the 4 "Destroyed vs X" comparisons
rownames(z_matrix) <- c("a", "b", "c", "d")
row_explainer <- data.frame(
  Label = c("a", "b", "c", "d"),
  Comparison = c(
    "Destroyed - Affected (1-9%)",
    "Destroyed - Major (26-50%)",
    "Destroyed - Minor (10-25%)",
    "Destroyed - No Damage"
  )
)
print(row_explainer)

new_order <- c(
  "Elevation", "Slope", "Aspect", "TRI", "EVC", "EVH", "EVT",
  "WUI_Area", "BTF", "BTGS", "PFTk9", "Frs_Area",
  "GrsShr_Area", "BuildingDensity", "RoadLength", "CBFR", "BuildYear"
)
new_order <- intersect(new_order, colnames(z_matrix))  # guard against renamed columns
z_matrix  <- z_matrix[, new_order]

my_col <- colorRampPalette(rev(brewer.pal(n = 11, name = "PuOr")))(100)

corrplot(
  z_matrix,
  method      = "ellipse",
  col         = my_col,
  is.corr     = FALSE,
  tl.col      = "black",
  tl.cex      = 0.9,
  addCoef.col = "black",
  number.cex  = 0.7,
  insig       = "n",
  pch.cex     = 1.5,
  cl.ratio    = 0.25,
  cl.pos      = "b",
  pch.col     = "black"
)

# Overlay significance stars
for (i in seq_len(nrow(sig_matrix))) {
  for (j in seq_len(ncol(sig_matrix))) {
    if (sig_matrix[i, j] == "*") {
      text(j + 0.3, nrow(sig_matrix) - i + 1 + 0.4, "*", cex = 1.2, col = "firebrick")
    }
  }
}

# ---------------------------------------------------------------------------
# 4. Pairwise correlation matrix across covariates
# ---------------------------------------------------------------------------
desired_order <- c(
  "Elevation", "Aspect", "Slope", "TRI", "EVC", "EVH", "EVT",
  "WUI_Area", "BTF", "BTGS", "Frs_Area",
  "GrsShr_Area", "BuildingDensity", "RoadLength", "PFTk9",
  "CBFR", "BuildYear", "prcp", "MaxRH", "MinRH",
  "Srad", "tdmean", "WD", "Tmin", "Tmax", "VPD", "WS10m"
)

if (!all(desired_order %in% names(CalDins.upd_geo_landfire_Building))) {
  stop("Some variables in desired_order do not exist in the dataset.")
}

cor_matrix         <- cor(CalDins.upd_geo_landfire_Building[, desired_order], use = "complete.obs")
cor_matrix_ordered <- cor_matrix[desired_order, desired_order]

col1 <- colorRampPalette(rev(brewer.pal(9, "BrBG")))

corrplot(
  cor_matrix_ordered,
  method      = "square",
  order       = "original",
  tl.col      = "black",
  tl.cex      = 0.75,
  sig.level   = 0.05,
  insig       = "label_sig",
  pch.cex     = 1.5,
  col         = col1(100),
  number.cex  = 0.3,
  type        = "lower",
  addCoef.col = "black"
)

# Clipped version (correlations capped at +/-0.7) for a more legible color scale
cor_matrix_clipped <- cor_matrix_ordered
cor_matrix_clipped[cor_matrix_clipped >  0.7] <-  0.7
cor_matrix_clipped[cor_matrix_clipped < -0.7] <- -0.7
cor_matrix_clipped <- round(cor_matrix_clipped, 6)

corrplot(
  cor_matrix_clipped,
  method     = "color",
  col        = col1(100),
  is.corr    = FALSE,
  tl.col     = "black",
  tl.cex     = 0.7,
  cl.pos     = "b",
  cl.ratio   = 0.1,
  col.lim    = c(-0.7, 0.7),
  type       = "lower",
  addgrid.col = "grey80"
)

# ---------------------------------------------------------------------------
# 5. Principal Component Analysis (PCA) on the covariate set
# ---------------------------------------------------------------------------
df_subset_pca <- CalDins.upd_geo_landfire_Building[desired_order]

pca_results <- PCA(df_subset_pca, graph = FALSE)

write.csv(
  pca_results$eig,
  paste0(location, "BuildingDamagePotential/MS/V01/Tables/PCA_Eig.csv")
)

print(head(pca_results$eig))

# PCA variable factor map
fviz_pca_var(
  pca_results,
  col.var      = "cos2",
  geom         = c("arrow", "text"),
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  title        = " ",
  repel        = TRUE
)

# Scree plot: explained variance + cumulative variance (top 15 components)
eig15 <- data.frame(head(pca_results$eig, 15))
vals  <- eig15$percentage.of.variance
cumv  <- eig15$cumulative.percentage.of.variance

op <- par(mar = c(5, 4, 4, 4) + 0.1)
bp <- barplot(
  height   = vals,
  names.arg = paste0("PC", seq_len(nrow(eig15))),
  col      = "skyblue",
  ylim     = c(0, max(100, max(cumv, na.rm = TRUE))),
  ylab     = "Explained variance percent",
  xlab     = " ",
  main     = ""
)
grid()
lines(x = bp, y = cumv, type = "b", pch = 19, col = "tomato", lwd = 1)
axis(4, at = pretty(c(0, max(100, max(cumv, na.rm = TRUE)))), las = 1)
mtext("Cumulative percent", side = 4, line = 3)
legend(
  "topleft", c("Explained variance", "Cumulative"),
  fill = c("skyblue", NA), border = NA,
  lty = c(NA, 1), pch = c(NA, 19), col = c("skyblue", "tomato"),
  pt.cex = 1, lwd = c(NA, 2), bty = "n"
)
par(op)

cat("Statistical analysis complete.\n")