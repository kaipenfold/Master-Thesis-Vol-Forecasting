###Combining all results into one dataset###
combine_results_all_datasets <- function(all_results, all_forecasts2, in_sample_VaR_results, model_types) {
  combined_results <- list()
  
  for (dataset_name in names(all_results)) {
    print(paste("Processing dataset:", dataset_name))
    
    dataset_data <- all_results[[dataset_name]]$full_data
    train_data <- all_results[[dataset_name]]$train_data
    test_data <- all_results[[dataset_name]]$test_data
    n_train <- nrow(train_data)
    n_test <- nrow(test_data)
    n_data <- nrow(dataset_data)
    
    # Create combined data with TrainTest flag
    combined_data <- dataset_data %>%
      mutate(TrainTest = ifelse(row_number() <= n_train, 'Train', 'Test'))
    
    # Add sum_returns column
    combined_data <- combined_data %>%
      mutate(sum_returns = sapply(1:n(), function(i) {
        if (i <= n() - 10) {
          return(sum(returns[i:(i + 9)]))
        } else {
          return(NA)
        }
      }))
    
    # Add fitted values for each model in the train set
    for (model_type in names(all_results[[dataset_name]]$fitted_models)) {
      fitted_col_name <- paste0(model_type, '_fitted')
      if (model_type == "standard_garch") {
        fitted_values <- all_results[[dataset_name]]$fitted_models[[model_type]]$fitted_values$sigma2
      } else {
        fitted_values <- all_results[[dataset_name]]$fitted_models[[model_type]]$fitted_values$log_sigma2
      }
      combined_data[[fitted_col_name]] <- c(fitted_values, rep(NA, n_test))
    }
    
    # Combine in-sample and forecasted VaR values
    for (model_type in model_types) {
      for (horizon in c(1, 10)) {
        var_0_05_col_name <- paste0(model_type, '_VaR_0.05_', horizon, 'd')
        var_0_01_col_name <- paste0(model_type, '_VaR_0.01_', horizon, 'd')
        
        in_sample_var_0_05 <- in_sample_VaR_results[[dataset_name]][[model_type]][[paste0('VaR_', horizon, '_days')]]$VaR_0.05
        in_sample_var_0_01 <- in_sample_VaR_results[[dataset_name]][[model_type]][[paste0('VaR_', horizon, '_days')]]$VaR_0.01
        
        forecast_horizon_key <- paste0(model_type, "_horizon_", horizon)
        forecasted_values <- all_forecasts2[[dataset_name]][[forecast_horizon_key]]
        
        # Combine in-sample and forecasted VaR values
        combined_var_0_05 <- c(in_sample_var_0_05, forecasted_values$VaR_0.05)
        combined_var_0_01 <- c(in_sample_var_0_01, forecasted_values$VaR_0.01)
        
        # Ensure the lengths match the full data length
        combined_var_0_05 <- c(combined_var_0_05, rep(NA, n_data - length(combined_var_0_05)))
        combined_var_0_01 <- c(combined_var_0_01, rep(NA, n_data - length(combined_var_0_01)))
        
        print(paste("Dataset:", dataset_name, "Model:", model_type, "Horizon:", horizon))
        print(paste("Length of combined VaR 0.05:", length(combined_var_0_05)))
        print(paste("Length of combined VaR 0.01:", length(combined_var_0_01)))
        
        combined_data[[var_0_05_col_name]] <- combined_var_0_05
        combined_data[[var_0_01_col_name]] <- combined_var_0_01
      }
    }
    
    combined_results[[dataset_name]] <- combined_data
  }
  
  return(combined_results)
}


combined_results_all <- combine_results_all_datasets(all_results, all_forecasts2, in_sample_VaR_results, model_types)

