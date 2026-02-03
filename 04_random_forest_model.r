#' ===============================================================================
#' Random Forest Model Training and Evaluation
#' ===============================================================================

source("src/00_setup.R")

#' Train Random Forest model with SMOTE balancing
#' @param data Data frame with predictors and response variable
#' @param response_var Name of binary response variable
#' @param ntree Number of trees (default: 3000)
#' @return List containing model, predictions, and performance metrics
train_rf_model <- function(data, response_var = "Damage01", ntree = 3000) {
  
  set.seed(123)
  
  # Remove rows with NA
  data_clean <- na.omit(data)
  
  # Apply SMOTE for class balancing
  formula_smote <- as.formula(paste(response_var, "~ ."))
  data_balanced <- SMOTE(formula_smote, data = data_clean, 
                         k = 5, perc.over = 110, perc.under = 200)
  
  cat(sprintf("✓ Class distribution after SMOTE:\n"))
  print(table(data_balanced[[response_var]]))
  
  # Split into training/testing
  train_idx <- createDataPartition(data_balanced[[response_var]], 
                                   p = 0.7, list = FALSE)
  train_data <- data_balanced[train_idx, ]
  test_data <- data_balanced[-train_idx, ]
  
  # Train Random Forest
  rf_formula <- as.formula(paste(response_var, "~ ."))
  rf_model <- randomForest(rf_formula, data = train_data, 
                           ntree = ntree, importance = TRUE)
  
  # Predictions
  predictions <- predict(rf_model, newdata = test_data)
  
  # Confusion matrix
  conf_mat <- confusionMatrix(predictions, test_data[[response_var]])
  
  cat(sprintf("\n✓ Model trained: Accuracy = %.2f%%\n", 
              conf_mat$overall['Accuracy'] * 100))
  
  return(list(
    model = rf_model,
    predictions = predictions,
    confusion_matrix = conf_mat,
    train_data = train_data,
    test_data = test_data
  ))
}

#' Plot variable importance with colored categories
#' @param rf_model Random Forest model object
#' @param save_path Path to save plot
plot_variable_importance <- function(rf_model, save_path = NULL) {
  
  # Extract importance
  var_imp <- importance(rf_model)
  imp_df <- data.frame(
    Variable = rownames(var_imp),
    Importance = var_imp[, "MeanDecreaseGini"]
  )
  
  # Calculate percentages
  imp_df$Percentage <- (imp_df$Importance / sum(imp_df$Importance)) * 100
  imp_df <- imp_df[order(imp_df$Percentage, decreasing = TRUE), ]
  
  # Assign categories
  categories <- list(
    'Topography' = c('Elevation', 'Aspect', 'Slope', 'TRI'),
    'Ecosystem' = c('EVH', 'EVT', 'BTF', 'Frs_Area', 'PFTk9'),
    'Weather' = c('prcp', 'MaxRH', 'Srad', 'tdmean', 'WD', 'Tmax', 'VPD', 'WS10m'),
    'Human footprint' = c('BuildingDensity', 'RoadLength', 'WUI_Area'),
    'Building Vulnerability' = 'CBFR'
  )
  
  # Map categories
  cat_df <- stack(categories)
  names(cat_df) <- c("Variable", "Group")
  imp_df <- left_join(imp_df, cat_df, by = "Variable")
  imp_df$Group[is.na(imp_df$Group)] <- "Other"
  
  # Plot
  p <- ggplot(imp_df, aes(x = reorder(Variable, Percentage), y = Percentage)) +
    geom_segment(aes(xend = Variable, yend = 0, color = Group),
                 linetype = "dashed", linewidth = 0.5) +
    geom_point(aes(color = Group, size = Percentage)) +
    coord_flip() +
    labs(x = "", y = "Mean Decrease Accuracy (%)", color = "Category") +
    scale_color_manual(values = c(
      "Topography" = "#0077BB",
      "Ecosystem" = "#009988",
      "Weather" = "#EE7733",
      "Human footprint" = "#CC3311",
      "Building Vulnerability" = "black",
      "Other" = "gray50"
    )) +
    scale_size_continuous(range = c(2, 5), guide = "none") +
    theme_minimal() +
    theme(legend.position = c(0.7, 0.3),
          text = element_text(size = 11, face = "bold"))
  
  if (!is.null(save_path)) {
    ggsave(save_path, p, width = 8, height = 6, dpi = 300)
    cat(sprintf("✓ Plot saved to %s\n", save_path))
  }
  
  return(p)
}