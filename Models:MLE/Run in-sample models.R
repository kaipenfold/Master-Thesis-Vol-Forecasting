data_dir <- "/Users/kai/Documents/BSE/Term 3/Master Thesis/Data/All series data"



# Function to apply models to all datasets with train-test split
apply_models_to_all_datasets <- function(data_dir, model_types, param_names) {
  file_list <- list.files(data_dir, full.names = TRUE)
  all_results <- list()
  
  for (file in file_list) {
    data <- read.csv(file)  # Assuming the datasets are in CSV format
    
    # Split the data into train (first 80%) and test (remaining 20%) sets
    split_index <- floor(0.8 * nrow(data))
    train_data <- data[1:split_index, ]
    test_data <- data[(split_index + 1):nrow(data), ]
    
    results <- fitted_models(train_data, model_types, param_names)
    results$train_data <- train_data
    results$test_data <- test_data
    results$full_data <- data
    all_results[[basename(file)]] <- results
  }
return(all_results = all_results)
}

# Run the function
all_results <- apply_models_to_all_datasets(data_dir, model_types, param_names)
#all_results$AAPL.csv$mse_table
train_aapl <- all_results$AAPL.csv$train_data
test_aapl <- all_results$AAPL.csv$test_data
aapl_data <- all_results$AAPL.csv$full_data
#all_results$AAPL.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2


train_aapl <- all_results$SPY.csv$train_data
test_aapl <- all_results$SPY.csv$test_data
aapl_data <- all_results$SPY.csv$full_data