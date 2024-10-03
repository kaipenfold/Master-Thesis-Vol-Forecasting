# # Function to calculate rolling sum for a given horizon
# calculate_rolling_sum <- function(returns, horizon) {
#   roll_sum <- sapply(1:(length(returns) - horizon + 1), function(i) {
#     sum(returns[i:(i + horizon - 1)])
#   })
#   return(roll_sum)
# }

##### EITHER THIS ONE
##### FINAL WORKING
compute_fz_loss <- function(returns, VaR, ES, alpha) {
  tryCatch({
    FZ_loss_result <- FZLoss(data = returns, VaR = -VaR, ES = -ES, alpha = alpha)
    return(mean(FZ_loss_result))
  }, error = function(e) {
    print(paste("Error in compute_fz_loss:", e$message))
    return(NA)
  })
}


calculate_rolling_sum <- function(returns, horizon) {
  roll_sum <- sapply(1:(length(returns) - horizon + 1), function(i) {
    sum(returns[i:(i + horizon - 1)])
  })
  return(roll_sum)
}

compute_all_es_metrics <- function(returns, VaR, ES, alpha, horizon = 1) {
  tryCatch({
    # Adjust returns for the forecast horizon
    if (horizon > 1) {
      adjusted_returns <- calculate_rolling_sum(returns, horizon)
      adjusted_returns <- head(adjusted_returns, length(VaR)) # Ensure same length
    } else {
      adjusted_returns <- returns
    }
    
    # Compute V_ES_1
    delta_t <- adjusted_returns - -ES
    K_alpha <- which(adjusted_returns < -VaR)
    T1 <- length(K_alpha)
    if (T1 > 0) {
      V_ES_1 <- sum(delta_t[K_alpha]) / T1
    } else {
      V_ES_1 <- 0
    }
    
    # Compute V_ES_2
    D_t <- adjusted_returns - -ES
    D_p <- quantile(D_t, alpha)
    tau_alpha <- which(D_t < D_p)
    T2 <- length(tau_alpha)
    if (T2 > 0) {
      V_ES_2 <- sum(D_t[tau_alpha]) / T2
    } else {
      V_ES_2 <- 0
    }
    
    # Combine V_ES_1 and V_ES_2
    V_alpha <- (abs(V_ES_1) + abs(V_ES_2)) / 2
    
    # Compute ESR p-value
    esr_backtest_result <- esr_backtest(r = adjusted_returns, q = -VaR, e = -ES, alpha = alpha / 2, version = 1)
    ESR_p_value <- esr_backtest_result$pvalue_twosided_asymptotic
    
    # Compute FZ Loss
    FZ_loss <- compute_fz_loss(adjusted_returns, VaR, ES, alpha)
    
    # Return all metrics as a list
    return(list(
      V_alpha = V_alpha,
      ESR_p_value = ESR_p_value,
      FZ_loss = FZ_loss
    ))
  }, error = function(e) {
    print(paste("Error in compute_all_es_metrics:", e$message))
    return(list(
      V_alpha = NA,
      ESR_p_value = NA,
      FZ_loss = NA
    ))
  })
}


# Initialize an empty data frame to store results
es_results_df <- data.frame()

# Loop through each stock and each model to calculate metrics
for (stock in list.files(forecast_dir, full.names = TRUE)) {
  stock_name <- basename(stock)
  actual <- all_results[[stock_name]]$test_data$returns
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 10)) {  # Adjust horizons as needed
      model_name <- paste0(model, horizon)
      
      for (alpha_level in c(0.01, 0.05)) {
        es_file <- file.path(stock, paste0(model_name, "_ES_", alpha_level, ".csv"))
        var_file <- file.path(stock, paste0(model_name, "_VaR_", alpha_level, ".csv"))
        if (file.exists(es_file) && file.exists(var_file)) {
          es_data <- read.csv(es_file)
          var_data <- read.csv(var_file)
          es_forecast <- es_data$Value
          var_forecast <- var_data$Value
          
          # Adjust the length of returns and calculate rolling sums for the 10-day horizon
          if (horizon == 10) {
            actual_adjusted <- calculate_rolling_sum(actual, horizon)
            actual_adjusted <- head(actual_adjusted, length(actual_adjusted) - horizon + 1) # Trim the last few days
          } else {
            actual_adjusted <- actual
          }
          
          # Debugging: Print the lengths of the inputs
          print(paste("Stock:", stock_name))
          print(paste("Model:", model_name))
          print(paste("Horizon:", horizon))
          print(paste("Alpha Level:", alpha_level))
          print(paste("Length of actual_adjusted:", length(actual_adjusted)))
          print(paste("Length of es_forecast:", length(es_forecast)))
          print(paste("Length of var_forecast:", length(var_forecast)))
          
          # Check if lengths match before calling the function
          if (length(actual_adjusted) == length(es_forecast) && length(actual_adjusted) == length(var_forecast)) {
            es_metrics <- compute_all_es_metrics(actual_adjusted, var_forecast, es_forecast, alpha_level, horizon)
            es_metrics$Stock <- stock_name
            es_metrics$Model <- ifelse(grepl("realised_garch", model_name), "Realised GARCH", "HAR-GARCH")
            es_metrics$Horizon <- horizon
            es_metrics$Alpha <- alpha_level
            es_results_df <- bind_rows(es_results_df, es_metrics)
          } else {
            print("Lengths do not match. Skipping this combination.")
          }
        }
      }
    }
  }
}



##### OR THIS ONE
# 
compute_all_es_metrics <- function(returns, VaR, ES, alpha) {
  tryCatch({
     # Compute V_alpha
    # Compute V_ES_1
    delta_t <- returns -ES
    K_alpha <- which(returns < -VaR)
    T1 <- length(K_alpha)
    if (T1 > 0) {
      V_ES_1 <- sum(delta_t[K_alpha]) / T1
    } else {
      V_ES_1 <- 0
    }
    
    # Compute V_ES_2
    D_t <- returns -ES
    D_p <- quantile(D_t, alpha)
    tau_alpha <- which(D_t < D_p)
    T2 <- length(tau_alpha)
    if (T2 > 0) {
      V_ES_2 <- sum(D_t[tau_alpha]) / T2
    } else {
      V_ES_2 <- 0
    }
    
    # Combine V_ES_1 and V_ES_2
    V_alpha <- (abs(V_ES_1) + abs(V_ES_2)) / 2

    # Compute ESR p-value
    esr_backtest_result <- esr_backtest(r = returns, q = -VaR, e = -ES, alpha = alpha / 2, version = 1)
    ESR_p_value <- esr_backtest_result$pvalue_twosided_asymptotic

    # Compute FZ Loss
    FZ_loss <- compute_fz_loss(returns, VaR, ES, alpha)

    # Return all metrics as a list
    return(list(
      V_alpha = V_alpha,
      ESR_p_value = ESR_p_value,
      FZ_loss = FZ_loss
    ))
  }, error = function(e) {
    print(paste("Error in compute_all_es_metrics:", e$message))
    return(list(
      V_alpha = NA,
      ESR_p_value = NA,
      FZ_loss = NA
    ))
  })
}

# Initialize an empty data frame to store results
es_results_df <- data.frame()

# Loop through each stock and each model to calculate metrics
for (stock in list.files(forecast_dir, full.names = TRUE)) {
  stock_name <- basename(stock)
  actual <- all_results[[stock_name]]$test_data$returns
  for (model in c("realised_garch_horizon_", "realised_har_garch_horizon_")) {
    for (horizon in c(1, 10)) {  # Adjust horizons as needed
      model_name <- paste0(model, horizon)
      
      for (alpha_level in c(0.01, 0.05)) {
        es_file <- file.path(stock, paste0(model_name, "_ES_", alpha_level, ".csv"))
        var_file <- file.path(stock, paste0(model_name, "_VaR_", alpha_level, ".csv"))
        if (file.exists(es_file) && file.exists(var_file)) {
          es_data <- read.csv(es_file)
          var_data <- read.csv(var_file)
          es_forecast <- es_data$Value
          var_forecast <- var_data$Value
          
          # Adjust the length of returns and calculate rolling sums for the 10-day horizon
          if (horizon == 10) {
            actual_adjusted <- calculate_rolling_sum(actual, horizon)
            actual_adjusted <- head(actual_adjusted, length(actual_adjusted))
          } else {
            actual_adjusted <- actual
          }
          
          # Debugging: Print the lengths of the inputs
          print(paste("Stock:", stock_name))
          print(paste("Model:", model_name))
          print(paste("Horizon:", horizon))
          print(paste("Alpha Level:", alpha_level))
          print(paste("Length of actual_adjusted:", length(actual_adjusted)))
          print(paste("Length of es_forecast:", length(es_forecast)))
          print(paste("Length of var_forecast:", length(var_forecast)))
          
          # Check if lengths match before calling the function
          if (length(actual_adjusted) == length(es_forecast) && length(actual_adjusted) == length(var_forecast)) {
            es_metrics <- compute_all_es_metrics(actual_adjusted, var_forecast, es_forecast, alpha_level)
            es_metrics$Stock <- stock_name
            es_metrics$Model <- ifelse(grepl("realised_garch", model_name), "Realised GARCH", "HAR-GARCH")
            es_metrics$Horizon <- horizon
            es_metrics$Alpha <- alpha_level
            es_results_df <- bind_rows(es_results_df, es_metrics)
          } else {
            print("Lengths do not match. Skipping this combination.")
          }
        }
      }
    }
  }
}

# View and save the final ES results table
es_results_df <- es_results_df %>%
  select(Stock, Model, Horizon, Alpha, V_alpha, ESR_p_value, FZ_loss) %>%
  rename(
    `V_alpha` = V_alpha,
    `ESR p-value` = ESR_p_value,
    `FZ Loss` = FZ_loss
  )

# Save final table
write.csv(es_results_df, file.path(forecast_dir, "final_es_metrics_table.csv"), row.names = FALSE)

rounded_es_results_df <- es_results_df %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

write.csv(rounded_es_results_df, file.path(forecast_dir, "rounded_es_results_NEW_banana.csv"), row.names = FALSE)
