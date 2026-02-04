#' ===============================================================================
#' Exploratory Data Analysis: California Wildfire Building Damage
#' ===============================================================================
#' Purpose: Descriptive statistics, distributions, correlations, and PCA
#' Author: Somnath Bar
#' Date: 2025-02-03
#' Paper: Science Advances - Wildfire Building Damage Prediction
#' ===============================================================================

source("src/00_setup.R")
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(corrplot)
  library(FactoMineR)
  library(factoextra)
  library(ggridges)
  library(RColorBrewer)
})

# ============================================================================
# 1. DATA SUMMARY STATISTICS
# ============================================================================

#' Calculate summary statistics by damage class
#' @param data Data frame with damage and predictor variables
#' @param var_name Variable name to summarize
#' @param damage_var Name of damage variable
#' @return Data frame with summary statistics
summarize_by_damage <- function(data, var_name, damage_var = "DAMAGE") {
  
  data %>%
    group_by(!!sym(damage_var)) %>%
    summarise(
      Variable = var_name,
      N = n(),
      Mean = mean(!!sym(var_name), na.rm = TRUE),
      Median = median(!!sym(var_name), na.rm = TRUE),
      SD = sd(!!sym(var_name), na.rm = TRUE),
      Min = min(!!sym(var_name), na.rm = TRUE),
      Max = max(!!sym(var_name), na.rm = TRUE),
      Q1 = quantile(!!sym(var_name), 0.25, na.rm = TRUE),
      Q3 = quantile(!!sym(var_name), 0.75, na.rm = TRUE)
    ) %>%
    mutate(across(where(is.numeric), ~round(., 3)))
}

#' Generate summary statistics for all predictors
#' @param data Data frame with predictors
#' @param predictor_vars Vector of predictor variable names
#' @export
generate_summary_tables <- function(data, predictor_vars) {
  
  cat("Generating summary statistics tables...\\n")
  
  summary_list <- lapply(predictor_vars, function(var) {
    tryCatch(
      summarize_by_damage(data, var),
      error = function(e) {
        cat(sprintf("Warning: Could not summarize %s\\n", var))
        NULL
      }
    )
  })
  
  summary_df <- bind_rows(summary_list)
  
  # Save to CSV
  write.csv(summary_df, 
            file.path(PATHS$tables, "summary_statistics_by_damage.csv"),
            row.names = FALSE)
  
  cat(sprintf("✓ Summary table saved (%d variables)\\n", 
              length(unique(summary_df$Variable))))
  
  return(summary_df)
}

# ============================================================================
# 2. DISTRIBUTION ANALYSIS
# ============================================================================

#' Create ridge plots for continuous variables by damage class
#' @param data Data frame with damage and predictor
#' @param var_name Variable name to plot
#' @param x_label X-axis label
#' @param xlim X-axis limits (optional)
#' @return ggplot object
plot_ridge_distribution <- function(data, var_name, x_label, xlim = NULL) {
  
  # Define damage class order and colors
  data$DAMAGE <- factor(data$DAMAGE,
                       levels = c("Inaccessible", "No Damage", 
                                  "Affected (1-9%)", "Minor (10-25%)", 
                                  "Major (26-50%)", "Destroyed (>50%)"))
  
  brown_palette <- c(
    "Inaccessible" = "grey", 
    "No Damage" = "#A1887F",
    "Affected (1-9%)" = "#8D6E63",
    "Minor (10-25%)" = "#6D4C41",
    "Major (26-50%)" = "#4E342E",
    "Destroyed (>50%)" = "#3E2723"
  )
  
  p <- ggplot(data, aes(x = !!sym(var_name), y = DAMAGE, fill = DAMAGE)) +
    geom_density_ridges(alpha = 0.7, scale = 1) +
    scale_fill_manual(values = brown_palette) +
    labs(x = x_label, y = "", title = "") +
    theme_minimal() +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank(),
      legend.position = "none",
      text = element_text(size = 11)
    )
  
  if (!is.null(xlim)) {
    p <- p + scale_x_continuous(limits = xlim)
  }
  
  return(p)
}

#' Generate all distribution plots
#' @param data Data frame with damage and predictors
#' @export
generate_distribution_plots <- function(data) {
  
  cat("Generating distribution plots...\\n")
  
  # Define variables to plot
  plot_specs <- list(
    list(var = "Elevation", label = "Elevation (m)", xlim = c(0, 2500)),
    list(var = "Slope", label = "Slope (°)", xlim = c(0, 35)),
    list(var = "Aspect", label = "Aspect (°)", xlim = c(0, 360)),
    list(var = "TRI", label = "TRI", xlim = c(0, 40)),
    list(var = "EVC", label = "EVC", xlim = c(0, 400)),
    list(var = "EVH", label = "EVH", xlim = c(0, 320)),
    list(var = "EVT", label = "EVT", xlim = NULL),
    list(var = "WUI_Area", label = expression("WUI Area (km"^2*")"), xlim = c(0, 0.1))
  )
  
  plots <- lapply(plot_specs, function(spec) {
    plot_ridge_distribution(data, spec$var, spec$label, spec$xlim)
  })
  
  # Combine plots using cowplot
  library(cowplot)
  combined_plot <- plot_grid(
    plotlist = plots,
    ncol = 4,
    labels = letters[1:length(plots)],
    align = "hv"
  )
  
  # Save combined plot
  ggsave(
    file.path(PATHS$figures, "Figure2_distributions_topography_vegetation.pdf"),
    combined_plot,
    width = 14, height = 7, units = "in", dpi = 300
  )
  
  cat("✓ Distribution plots saved\\n")
  
  return(plots)
}

# ============================================================================
# 3. CORRELATION ANALYSIS
# ============================================================================

#' Calculate and visualize correlation matrix
#' @param data Data frame with predictors
#' @param predictor_vars Vector of variable names
#' @export
analyze_correlations <- function(data, predictor_vars) {
  
  cat("Calculating correlation matrix...\\n")
  
  # Select predictors and remove NA
  data_clean <- data %>%
    select(all_of(predictor_vars)) %>%
    na.omit()
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_clean, use = "complete.obs")
  
  # Clip extreme correlations for better visualization
  cor_matrix_clipped <- cor_matrix
  cor_matrix_clipped[cor_matrix_clipped > 0.7] <- 0.7
  cor_matrix_clipped[cor_matrix_clipped < -0.7] <- -0.7
  
  # Color palette
  col_palette <- colorRampPalette(rev(brewer.pal(9, "BrBG")))(100)
  
  # Create correlation plot
  pdf(file.path(PATHS$figures, "Figure_S1_correlation_matrix.pdf"),
      width = 10, height = 10)
  
  corrplot(
    cor_matrix_clipped,
    method = "color",
    col = col_palette,
    tl.col = "black",
    tl.cex = 0.7,
    cl.pos = "b",
    cl.ratio = 0.1,
    col.lim = c(-0.7, 0.7),
    type = "lower",
    addgrid.col = "grey80"
  )
  
  dev.off()
  
  # Save correlation matrix to CSV
  write.csv(cor_matrix, 
            file.path(PATHS$tables, "correlation_matrix.csv"))
  
  cat("✓ Correlation analysis complete\\n")
  
  return(cor_matrix)
}

# ============================================================================
# 4. PRINCIPAL COMPONENT ANALYSIS (PCA)
# ============================================================================

#' Perform PCA on predictor variables
#' @param data Data frame with predictors
#' @param predictor_vars Vector of variable names
#' @export
perform_pca <- function(data, predictor_vars) {
  
  cat("Performing PCA...\\n")
  
  # Prepare data
  data_pca <- data %>%
    select(all_of(predictor_vars)) %>%
    na.omit()
  
  # Perform PCA
  pca_results <- PCA(data_pca, graph = FALSE)
  
  # Save eigenvalues
  eig_df <- as.data.frame(pca_results$eig[1:15, ])
  eig_df$PC <- paste0("PC", 1:nrow(eig_df))
  
  write.csv(eig_df, 
            file.path(PATHS$tables, "PCA_eigenvalues.csv"),
            row.names = FALSE)
  
  # Scree plot
  pdf(file.path(PATHS$figures, "Figure_S2_PCA_scree_plot.pdf"),
      width = 10, height = 6)
  
  par(mar = c(5, 4, 4, 4) + 0.1)
  vals <- eig_df$percentage.of.variance
  cumv <- eig_df$cumulative.percentage.of.variance
  
  bp <- barplot(
    height = vals,
    names.arg = eig_df$PC,
    col = "skyblue",
    ylim = c(0, max(100, max(cumv, na.rm = TRUE))),
    ylab = "Explained variance (%)",
    xlab = "",
    main = "PCA Scree Plot"
  )
  
  grid()
  lines(x = bp, y = cumv, type = "b", pch = 19, col = "tomato", lwd = 2)
  axis(4, at = pretty(c(0, 100)), las = 1)
  mtext("Cumulative (%)", side = 4, line = 3)
  
  legend("topleft", 
         c("Explained variance", "Cumulative"),
         fill = c("skyblue", NA), 
         border = NA,
         lty = c(NA, 1), 
         pch = c(NA, 19), 
         col = c("skyblue", "tomato"),
         pt.cex = 1, 
         lwd = c(NA, 2), 
         bty = "n")
  
  dev.off()
  
  # Variable contributions plot
  pdf(file.path(PATHS$figures, "Figure_S3_PCA_variable_contributions.pdf"),
      width = 10, height = 8)
  
  print(fviz_pca_var(
    pca_results, 
    col.var = "cos2",
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE,
    title = "PCA - Variable Contributions"
  ))
  
  dev.off()
  
  cat("✓ PCA complete\\n")
  
  return(pca_results)
}

# ============================================================================
# 5. TEMPORAL TRENDS
# ============================================================================

#' Plot damage observations over years
#' @param data Data frame with Year and Damage variables
#' @export
plot_temporal_trends <- function(data) {
  
  cat("Generating temporal trend plots...\\n")
  
  # Count observations per year
  yearly_counts <- data %>%
    group_by(Year) %>%
    summarise(Count = n(), .groups = 'drop')
  
  # Bar plot
  p1 <- ggplot(yearly_counts, aes(x = Year, y = Count)) +
    geom_col(fill = 'skyblue', color = 'grey2', width = 0.8) +
    labs(title = "",
         x = "",
         y = "Number of DINS Observations") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 11, face = "bold")
    ) +
    scale_y_continuous(limits = c(0, 30000), 
                      expand = expansion(mult = c(0, 0.05)))
  
  # Damage class distribution
  damage_counts <- data %>%
    group_by(DAMAGE) %>%
    summarise(Count = n(), .groups = 'drop')
  
  damage_counts$DAMAGE <- factor(
    damage_counts$DAMAGE,
    levels = c("Inaccessible", "No Damage", "Affected (1-9%)", 
               "Minor (10-25%)", "Major (26-50%)", "Destroyed (>50%)")
  )
  
  p2 <- ggplot(damage_counts, aes(x = DAMAGE, y = Count)) +
    geom_col(fill = 'skyblue', color = 'black', width = 0.5) +
    labs(title = "",
         x = "",
         y = "Number of Observations") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 11, face = "bold")
    )
  
  # Combine plots
  library(cowplot)
  combined <- plot_grid(p1, p2, ncol = 2, labels = c("a", "b"))
  
  ggsave(
    file.path(PATHS$figures, "Figure_S4_temporal_damage_trends.pdf"),
    combined,
    width = 12, height = 5, units = "in", dpi = 300
  )
  
  cat("✓ Temporal trend plots saved\\n")
  
  return(list(p1 = p1, p2 = p2))
}

# ============================================================================
# 6. MAIN EXECUTION
# ============================================================================

if (!interactive()) {
  
  cat("\\n========================================\\n")
  cat("EXPLORATORY DATA ANALYSIS\\n")
  cat("========================================\\n\\n")
  
  # Load processed data
  data_file <- file.path(PATHS$models, "CalDins_upd_extractions_subset_03.csv")
  
  if (!file.exists(data_file)) {
    stop("Error: Processed data file not found. Run 01_data_preprocessing.R first.")
  }
  
  df <- read.csv(data_file)
  
  # Define predictor variables
  predictor_vars <- c(
    'Elevation', 'Aspect', 'Slope', 'TRI', 'EVC', 'EVH', 'EVT',
    'WUI_Area', 'BTF', 'BTGS', 'Frs_Area', 'GrsShr_Area',
    'BuildingDensity', 'RoadLength', 'PFTk9', 'CBFR', 'BuildYear',
    'prcp', 'MaxRH', 'MinRH', 'Srad', 'tdmean', 'WD',
    'Tmin', 'Tmax', 'VPD', 'WS10m'
  )
  
  # 1. Summary statistics
  summary_stats <- generate_summary_tables(df, predictor_vars)
  
  # 2. Distribution plots
  dist_plots <- generate_distribution_plots(df)
  
  # 3. Correlation analysis
  cor_matrix <- analyze_correlations(df, predictor_vars)
  
  # 4. PCA
  pca_results <- perform_pca(df, predictor_vars)
  
  # 5. Temporal trends
  temporal_plots <- plot_temporal_trends(df)
  
  cat("\\n✓ Exploratory analysis complete\\n")
  cat("Results saved to:", PATHS$figures, "\\n")
}