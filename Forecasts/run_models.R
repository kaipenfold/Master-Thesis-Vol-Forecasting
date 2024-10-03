###This script fits our models and run the forecast function for every dataset we have####

#data_dir <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Scripts/Datasets
#data_dir2 <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Scripts/Test Datasets" ##using this as a test with only 2 datasets
data_dir3 <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Scripts/SPY"
#data_dir_iv <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Scripts/IV FINAL"
##Fitting all our models###
apply_models_to_all_datasets <- function(data_dir, model_types, param_names, split = 0.75) {
  file_list <- list.files(data_dir, full.names = TRUE)
  all_results <- list()
  
  for (file in file_list) {
    data <- read.csv(file)  # Assuming the datasets are in CSV format
    
    # Split the data into train (first 80%) and test (remaining 20%) sets
    split_index <- floor(split * nrow(data))
    train_data <- data[1:split_index, ]
    test_data <- data[(split_index + 1):nrow(data), ]
    
    results <- fit_models_and_get_parameters(train_data, model_types, param_names)
    results$train_data <- train_data
    results$test_data <- test_data
    results$full_data <- data
    all_results[[basename(file)]] <- results
  }
  
  return(all_results = all_results)
}

model_type <- c("realised_garch", "realised_har_garch")
calculate_in_sample_VaR_all <- function(model_type, results, forecast_horizons = c(1, 10), alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  all_in_sample_VaR <- list()
  
  for (dataset_name in names(results)) {
    dataset_results <- results[[dataset_name]]
    
    for (model_type in model_types) {
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing Dataset:", dataset_name, "Model:", model_type, "Forecast Horizon:", forecast_horizon))
        in_sample_VaR <- tryCatch({
          in_sample_all_models_VaR(model_type, forecast_horizon, dataset_results, alpha_0.05, alpha_0.01)
        }, error = function(e) {
          print(paste("Error in model", model_type, "for dataset", dataset_name, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(in_sample_VaR)) {
          if (!is.null(all_in_sample_VaR[[dataset_name]])) {
            if (!is.null(all_in_sample_VaR[[dataset_name]][[model_type]])) {
              all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
            } else {
              all_in_sample_VaR[[dataset_name]][[model_type]] <- list()
              all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
            }
          } else {
            all_in_sample_VaR[[dataset_name]] <- list()
            all_in_sample_VaR[[dataset_name]][[model_type]] <- list()
            all_in_sample_VaR[[dataset_name]][[model_type]][[paste0("VaR_", forecast_horizon, "_days")]] <- in_sample_VaR
          }
        }
      }
    }
  }
  
  return(all_in_sample_VaR)
}

##FORECASTING###
apply_forecasts_to_all_datasets <- function(results, model_types, forecast_horizon = 1 , alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      if (model_type == "realised_har_garch") {
        forecasted_values <- har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01)
      } else if (model_type == "realised_garch") {
        forecasted_values <- realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01)
      } else {
        next
      }
      
      dataset_forecasts[[model_type]] <- forecasted_values
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}

####THIS IS SLIGHTLY DIFFERENT THEN THE FUNCTION ABOVE, NOT SURE WHY, THEY ARE CALLING THE SAME FUNCTION BUT CAN'T REALLY UNDERSTAND WHY THE RESULTS ARE A BIT
###DIFFERENT###

apply_forecasts_to_all_datasets <- function(results, model_types, forecast_horizons = 1, alpha_0.05 = 0.05, alpha_0.01 = 0.01) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "_horizon_", forecast_horizon)]] <- forecasted_values
        }
      }
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}

apply_forecasts_to_all_datasets_2 <- function(results, model_types, forecast_horizons = c(1, 10), alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  all_forecasts <- list()
  
  for (dataset_name in names(results)) {
    print(paste("Processing dataset:", dataset_name))
    train_data <- results[[dataset_name]]$train_data
    test_data <- results[[dataset_name]]$test_data
    dataset_forecasts <- list()
    
    for (model_type in model_types) {
      params <- results[[dataset_name]]$fitted_models[[model_type]]$optimal_params
      
      for (forecast_horizon in forecast_horizons) {
        print(paste("Processing forecast horizon:", forecast_horizon, "for model:", model_type))
        
        forecasted_values <- tryCatch({
          if (model_type == "realised_har_garch") {
            har_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else if (model_type == "realised_garch") {
            realised_garch_var_forecast(params, model_type, forecast_horizon, results[[dataset_name]], train_data, test_data, alpha_0.05, alpha_0.01, p)
          } else {
            next
          }
        }, error = function(e) {
          print(paste("Error in forecasting for model", model_type, "with horizon", forecast_horizon, ":", e$message))
          return(NULL)
        })
        
        if (!is.null(forecasted_values)) {
          dataset_forecasts[[paste0(model_type, "horizon", forecast_horizon)]] <- forecasted_values
        }
      }
    }
    
    all_forecasts[[dataset_name]] <- dataset_forecasts
  }
  
  return(all_forecasts)
}



forecast_horizon = 1
forecast_horizons <- c(1, 10)
all_results <- apply_models_to_all_datasets(data_dir2, model_types, param_names) 
in_sample_VaR_results_just_spy <- calculate_in_sample_VaR_all(model_types, all_results)
all_forecasts_just_spy <- apply_forecasts_to_all_datasets_2(all_results, model_types, forecast_horizons)
#all_forecasts2 <- apply_forecasts_to_all_datasets(all_results, model_types, forecast_horizons)

mse(all_results$MCD.csv$full_data$log_x_adj, all_results$MCD.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2)
###TRYING TO UNDERSTAND WHY THERE IS A SMALL DIFF IN THESE 2 FUNCTIONS ABOVE, MAYBE DUE TO MY ENVIRONMENT BUT DIDN'T WANT TO RESET AS IT WOULD TAKE 
##TOO LONG TO RUN IT AGAIN.
# Compare forecasted values for a specific dataset and model
dataset_name <- "AAPL.csv"
model_type <- "realised_garch"
forecast_horizon <- 1

# From the first function
forecast_values_1 <- all_forecasts[[dataset_name]][[model_type]]$rolling_forecasts

# From the second function
forecast_values_2 <- all_forecasts2[[dataset_name]][[paste0(model_type, "_horizon_", forecast_horizon)]]$rolling_forecasts

# Compare the first few values
print("Forecast values from the first function:")
print(head(forecast_values_1))

print("Forecast values from the second function:")
print(head(forecast_values_2))

# Check if they are identical
identical(forecast_values_1, forecast_values_2)









# # Export in-sample var
# Define the base path for "In Sample VaR Results"
base_path <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Results/FINAL In Sample VaR Results"

# Create the base directory if it doesn't exist
if (!dir.exists(base_path)) {
  dir.create(base_path)
}

# Function to log the progress and errors
log_message <- function(message) {
  cat(paste0(Sys.time(), " - ", message, "\n"))
}

# Function to export specific results from nested list contents to CSV
export_selected_results_to_csv <- function(nested_list, base_path) {
  selected_results <- c("VaR_0.05", "VaR_0.01", "ES_0.05", "ES_0.01")

  for (ticker in names(nested_list)) {
    ticker_data <- nested_list[[ticker]]
    ticker_path <- file.path(base_path, ticker)
    dir.create(ticker_path, showWarnings = FALSE)

    for (model_name in names(ticker_data)) {
      model_data <- ticker_data[[model_name]]

      for (result_period in c("VaR_1_days", "VaR_10_days")) {
        if (!is.null(model_data[[result_period]])) {
          period_data <- model_data[[result_period]]

          for (result_name in selected_results) {
            if (!is.null(period_data[[result_name]])) {
              result_data <- period_data[[result_name]]

              # Construct the file name
              file_name <- paste0(model_name, "_", result_period, "_", result_name, ".csv")

              # Save vector data
              if (is.vector(result_data)) {
                try({
                  result_df <- data.frame(Value = result_data)
                  write.csv(result_df, file = file.path(ticker_path, file_name), row.names = FALSE)
                  log_message(paste("Saved vector data to", file_name))
                }, silent = TRUE)
              }

              # Save matrix or data frame data
              else if (is.matrix(result_data) || is.data.frame(result_data)) {
                try({
                  write.csv(result_data, file = file.path(ticker_path, file_name), row.names = FALSE)
                  log_message(paste("Saved matrix/data frame data to", file_name))
                }, silent = TRUE)
              }
            }
          }
        }
      }
    }
  }
}

# Example usage
#Assuming 'in_sample_VaR_results' is your nested list structure
export_selected_results_to_csv(in_sample_VaR_results, base_path)
#
# # Export Forecasts
# # Define the base path for "All Forecasts"
base_path <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Results/FINAL Forecast VaR Results"

# Create the base directory if it doesn't exist
if (!dir.exists(base_path)) {
  dir.create(base_path)
}

# Function to export specific forecast results to CSV
export_forecast_results_to_csv <- function(all_forecasts, base_path) {
  selected_results <- c("rolling_forecasts", "VaR_0.05", "VaR_0.01", "ES_0.05", "ES_0.01")

  for (dataset_name in names(all_forecasts)) {
    dataset_data <- all_forecasts[[dataset_name]]
    dataset_path <- file.path(base_path, dataset_name)
    dir.create(dataset_path, showWarnings = FALSE)

    for (model_forecast_name in names(dataset_data)) {
      model_forecast_data <- dataset_data[[model_forecast_name]]

      for (result_name in selected_results) {
        if (!is.null(model_forecast_data[[result_name]])) {
          result_data <- model_forecast_data[[result_name]]

          # Construct the file name
          file_name <- paste0(model_forecast_name, "_", result_name, ".csv")

          # Save vector data
          if (is.vector(result_data)) {
            try({
              result_df <- data.frame(Value = result_data)
              write.csv(result_df, file = file.path(dataset_path, file_name), row.names = FALSE)
              print(paste("Saved vector data to", file_name))
            }, silent = TRUE)
          }

          # Save matrix or data frame data
          else if (is.matrix(result_data) || is.data.frame(result_data)) {
            try({
              write.csv(result_data, file = file.path(dataset_path, file_name), row.names = FALSE)
              print(paste("Saved matrix/data frame data to", file_name))
            }, silent = TRUE)
          }
        }
      }
    }
  }
}

# Example usage
# Assuming 'all_forecasts' is your nested list structure
export_forecast_results_to_csv(all_forecasts, base_path)





