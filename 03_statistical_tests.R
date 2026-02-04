#' ===============================================================================
#' Statistical Tests: Non-parametric comparisons across damage classes
#' ===============================================================================
#' Purpose: Kruskal-Wallis tests, Dunn's post-hoc, circular statistics
#' Author: Somnath Bar
#' Date: 2025-02-03
#' Paper: Science Advances - Wildfire Building Damage Prediction
#' ===============================================================================

source("src/00_setup.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(FSA)
  library(circular)
  library(DescTools)
  library(broom)
})

# ============================================================================
# 1. KRUSKAL-WALLIS TESTS
# ============================================================================

#' Perform Kruskal-Wallis test for a single variable
#' @param data Data frame with damage and predictor
#' @param var_name Variable name to test
#' @param damage_var Name of damage class variable
#' @return Data frame with test results
kruskal_test_single <- function(data, var_name, damage_var = "DAMAGE") {
  
  formula <- reformulate(damage_var, var_name)
  kw <- kruskal.test(formula, data = data)
  
  data.frame(
    Variable = var_name,
    Chi_squared = round(kw$statistic, 3),
    df = kw$parameter,
    p_value = ifelse(kw$p.value < 0.001, "<0.001", 
                    format.pval(kw$p.value, digits = 3)),
    stringsAsFactors = FALSE
  )
}

#' Run Kruskal-Wallis tests for all predictors
#' @param data Data frame with predictors
#' @param predictor_vars Vector of variable names
#' @export
run_kruskal_wallis_tests <- function(data, predictor_vars) {
  
  cat("Running Kruskal-Wallis tests...\\n")
  
  kw_results <- lapply(predictor_vars, function(var) {
    tryCatch(
      kruskal_test_single(data, var),
      error = function(e) {
        cat(sprintf("Warning: Could not test %s\\n", var))
        NULL
      }
    )
  }) %>% bind_rows()
  
  # Save results
  write.csv(
    kw_results,
    file.path(PATHS$tables, "kruskal_wallis_results.csv"),
    row.names = FALSE
  )
  
  cat(sprintf("✓ Kruskal-Wallis tests complete (%d variables)\\n", 
              nrow(kw_results)))
  
  return(kw_results)
}

# ============================================================================
# 2. DUNN'S POST-HOC TESTS
# ============================================================================

#' Perform Dunn's test for a single variable
#' @param data Data frame with damage and predictor
#' @param var_name Variable name to test
#' @param damage_var Name of damage class variable
#' @return Data frame with pairwise comparison results
dunn_test_single <- function(data, var_name, damage_var = "DAMAGE") {
  
  formula <- reformulate(damage_var, var_name)
  dunn <- FSA::dunnTest(formula, data = data, method = "bonferroni")
  
  dunn$res %>%
    select(Comparison, Z, P.adj) %>%
    mutate(
      Variable = var_name,
      Z = round(Z, 3),
      P.adj = if_else(P.adj < 0.001, "<0.001", as.character(round(P.adj, 3)))
    ) %>%
    select(Variable, Comparison, Z, P.adj)
}

#' Run Dunn's tests for all predictors
#' @param data Data frame with predictors
#' @param predictor_vars Vector of variable names
#' @export
run_dunn_tests <- function(data, predictor_vars) {
  
  cat("Running Dunn's post-hoc tests (this may take a while)...\\n")
  
  library(pbapply)
  dunn_results <- pblapply(predictor_vars, function(var) {
    tryCatch(
      dunn_test_single(data, var),
      error = function(e) {
        cat(sprintf("Warning: Could not test %s\\n", var))
        NULL
      }
    )
  }) %>% bind_rows()
  
  # Filter for comparisons involving "Destroyed (>50%)" but not "Inaccessible"
  destroyed_comparisons <- dunn_results %>%
    filter(grepl("Destroyed \\\\(>50%\\\\)", Comparison)) %>%
    filter(!grepl("Inaccessible", Comparison)) %>%
    arrange(Variable, Comparison)
  
  # Save full results
  write.csv(
    dunn_results,
    file.path(PATHS$tables, "dunn_test_all_comparisons.csv"),
    row.names = FALSE
  )
  
  # Save destroyed-focused results
  write.csv(
    destroyed_comparisons,
    file.path(PATHS$tables, "dunn_test_destroyed_comparisons.csv"),
    row.names = FALSE
  )
  
  cat(sprintf("✓ Dunn's tests complete (%d total comparisons)\\n", 
              nrow(dunn_results)))
  
  return(list(
    all = dunn_results,
    destroyed = destroyed_comparisons
  ))
}

#' Create heatmap of Dunn's test Z-scores
#' @param dunn_results Data frame from run_dunn_tests()
#' @export
plot_dunn_heatmap <- function(dunn_results) {
  
  cat("Creating Dunn's test heatmap...\\n")
  
  library(tidyr)
  library(corrplot)
  
  # Filter for destroyed comparisons
  dunn_filtered <- dunn_results$destroyed %>%
    mutate(sig = ifelse(P.adj < 0.05, "*", ""))
  
  # Create Z-score matrix
  z_matrix <- dunn_filtered %>%
    pivot_wider(names_from = Variable, values_from = Z, values_fill = 0) %>%
    column_to_rownames("Comparison") %>%
    as.matrix()
  
  # Shorten row labels
  rownames(z_matrix) <- c("a", "b", "c", "d")
  
  # Reorder columns
  new_order <- c(
    "Elevation", "Slope", "Aspect", "TRI", "EVC", "EVH", "EVT",
    "WUI_Area", "BTF", "BTGS", "PFTk9", "Frs_Area", "GrsShr_Area",
    "BuildingDensity", "RoadLength", "CBFR", "BuildYear"
  )
  
  z_matrix <- z_matrix[, intersect(new_order, colnames(z_matrix))]
  
  # Create significance matrix
  sig_matrix <- dunn_filtered %>%
    pivot_wider(names_from = Variable, values_from = sig, values_fill = "") %>%
    column_to_rownames("Comparison") %>%
    as.matrix()
  
  rownames(sig_matrix) <- c("a", "b", "c", "d")
  sig_matrix <- sig_matrix[, colnames(z_matrix)]
  
  # Plot
  pdf(file.path(PATHS$figures, "Figure3_dunn_test_heatmap.pdf"),
      width = 12, height = 5)
  
  my_col <- colorRampPalette(rev(brewer.pal(n = 11, name = "PuOr")))(100)
  
  corrplot(
    z_matrix,
    method = "ellipse",
    col = my_col,
    is.corr = FALSE,
    tl.col = "black",
    tl.cex = 0.9,
    addCoef.col = "black",
    number.cex = 0.7,
    insig = "n",
    pch.cex = 1.5,
    cl.ratio = 0.25,
    cl.pos = "b",
    pch.col = "black"
  )
  
  # Add significance stars
  for (i in 1:nrow(sig_matrix)) {
    for (j in 1:ncol(sig_matrix)) {
      if (sig_matrix[i, j] == "*") {
        text(j + 0.3, nrow(sig_matrix) - i + 1 + 0.4, "*", 
             cex = 1.2, col = "firebrick")
      }
    }
  }
  
  dev.off()
  
  cat("✓ Dunn's test heatmap saved\\n")
}

# ============================================================================
# 3. CIRCULAR STATISTICS FOR ASPECT
# ============================================================================

#' Calculate circular statistics for aspect
#' @param data Data frame with Aspect and Damage variables
#' @export
analyze_aspect_circular <- function(data) {
  
  cat("Analyzing aspect using circular statistics...\\n")
  
  # Calculate Northness and Eastness components
  data$Northness <- cos(data$Aspect * pi / 180)
  data$Eastness <- sin(data$Aspect * pi / 180)
  
  # Component means and SDs
  component_stats <- data %>%
    group_by(Damage01) %>%
    summarise(
      Northness_mean = mean(Northness, na.rm = TRUE),
      Northness_sd = sd(Northness, na.rm = TRUE),
      Eastness_mean = mean(Eastness, na.rm = TRUE),
      Eastness_sd = sd(Eastness, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Circular mean and SD
  aspect_radians <- atan2(data$Eastness, data$Northness)
  aspect_degrees <- (aspect_radians * 180 / pi + 360) %% 360
  aspect_circular <- circular(aspect_degrees, units = "degrees", 
                             template = "geographics")
  
  circular_stats <- data %>%
    group_by(Damage01) %>%
    summarise(
      Circular_mean = mean.circular(
        circular(atan2(Eastness, Northness) * 180 / pi, 
                units = "degrees", template = "geographics")
      ),
      Circular_sd = sd.circular(
        circular(atan2(Eastness, Northness) * 180 / pi, 
                units = "degrees", template = "geographics")
      ),
      .groups = 'drop'
    )
  
  # Combine results
  aspect_results <- bind_cols(component_stats, 
                              circular_stats %>% select(-Damage01))
  
  # Save results
  write.csv(
    aspect_results,
    file.path(PATHS$tables, "aspect_circular_statistics.csv"),
    row.names = FALSE
  )
  
  cat("✓ Aspect circular statistics complete\\n")
  
  return(aspect_results)
}

# ============================================================================
# 4. ANOVA FOR CATEGORICAL VARIABLES
# ============================================================================

#' Perform two-way ANOVA for year built vs. year of damage
#' @param data Data frame with building year and damage year
#' @export
analyze_building_year_anova <- function(data) {
  
  cat("Running ANOVA for building year analysis...\\n")
  
  # Prepare data
  year_data <- data %>%
    filter(!is.na(YearBuilt), !is.na(YearofDamage)) %>%
    filter(YearBuilt >= 1776) %>%
    filter(DAMAGE == "Destroyed (>50%)")
  
  # Summarize counts
  year_summary <- year_data %>%
    group_by(YearofDamage, YearBuilt, DAMAGE) %>%
    summarise(count = n(), .groups = 'drop')
  
  # Two-way ANOVA
  anova_model <- aov(count ~ YearofDamage + YearBuilt, data = year_summary)
  anova_summary <- summary(anova_model)
  
  # Save results
  capture.output(anova_summary, 
                file = file.path(PATHS$tables, "building_year_anova.txt"))
  
  cat("✓ Building year ANOVA complete\\n")
  
  return(list(
    model = anova_model,
    summary = anova_summary
  ))
}

# ============================================================================
# 5. MAIN EXECUTION
# ============================================================================

if (!interactive()) {
  
  cat("\\n========================================\\n")
  cat("STATISTICAL TESTS\\n")
  cat("========================================\\n\\n")
  
  # Load processed data
  data_file <- file.path(PATHS$models, "CalDins_upd_extractions_subset_03.csv")
  
  if (!file.exists(data_file)) {
    stop("Error: Processed data file not found. Run 01_data_preprocessing.R first.")
  }
  
  df <- read.csv(data_file)
  
  # Define predictor variables
  predictor_vars <- c(
    'Elevation', 'Slope', 'Aspect', 'TRI', 'EVC', 'EVH', 'EVT',
    'WUI_Area', 'BTF', 'BTGS', 'Frs_Area', 'GrsShr_Area',
    'BuildingDensity', 'RoadLength', 'PFTk9', 'CBFR', 'BuildYear'
  )
  
  # 1. Kruskal-Wallis tests
  kw_results <- run_kruskal_wallis_tests(df, predictor_vars)
  
  # 2. Dunn's post-hoc tests
  dunn_results <- run_dunn_tests(df, predictor_vars)
  
  # 3. Plot Dunn's test heatmap
  plot_dunn_heatmap(dunn_results)
  
  # 4. Circular statistics for aspect
  aspect_stats <- analyze_aspect_circular(df)
  
  # 5. Building year ANOVA (if data available)
  if (all(c("YearBuilt", "YearofDamage") %in% names(df))) {
    year_anova <- analyze_building_year_anova(df)
  }
  
  cat("\\n✓ Statistical tests complete\\n")
  cat("Results saved to:", PATHS$tables, "\\n")
}