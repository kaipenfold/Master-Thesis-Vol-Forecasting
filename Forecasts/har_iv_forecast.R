###HAR_GARCH_IV_FORECAST###

har_iv_garch_forecast <- function(params, model, forecast_horizon, results, train_data, test_data) {
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  gamma_iv_w <- params[7]
  gamma_iv_m <- params[8]
  xi <- params[9]
  phi <- params[10]
  tau1 <- params[11]
  tau2 <- params[12]
  sigma_ut <- params[13]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day values
  rm_history <- as.list(train_data$RM_day)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { #starting from point that ensures we have enough data for k-day forecast
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    IV_day <- current_train_data$IV_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week and RM_month
    rm_history <- append(rm_history, list(RM_day))
    iv_history <- append(iv_history, list(IV_day))
    
    # Calculate RM_week
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    IV_week <- mean(unlist(iv_history[(length(iv_history)-5):(length(iv_history)-2)]))
    
    # Calculate RM_month
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    IV_month <- mean(unlist(iv_history[(length(iv_history)-22):(length(iv_history)-6)]))
    
    # Combining z and ut so we can simulate the values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      # Drawing 5000 random pairs
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      # Getting epsilon and log_x for each simulation pair
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        # Compute epsilon_t
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) + (phi * gamma_iv_d * IV_day) + (phi * gamma_iv_w * IV_week) + (phi * gamma_iv_m * IV_month) + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month + gamma_iv_d * IV_day + gamma_iv_w * IV_week + gamma_iv_m * IV_month
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      # Calculate the mean of the simulated log_sigma values
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      IV_day <- 
        
      # Update RM_week and RM_month dynamically for the next step
      rm_history <- append(rm_history, list(RM_day))
      iv_history <- append(iv_history,list(IV_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      IV_week <- mean(unlist(iv_history[(length(iv_history)-5):(length(iv_history)-2)]))
      IV_month <- mean(unlist(iv_history[(length(iv_history)-22):(length(iv_history)-6)]))
      
      # Update epsilon_t_prev for the next iteration using the mean of the current iteration's values
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    # Store the forecast
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
  }
  
  return(rolling_forecasts)
}

### Testing for 10-day horizon: ###
#params <- results$fitted_models$realised_har_garch$optimal_params
params <- all_results$AAPL.csv$fitted_models$realised_har_garch$optimal_params
forecast_horizon <- 1  # 10-day ahead forecast
rolling_forecast_results <- multi_period_har_garch_forecast(params, "realised_har_garch", forecast_horizon, all_results$AAPL.csv, train_aapl, test_aapl)


### Creating Adjusted RM ###
adjustment_factor_train <- mean(train_aapl$returns^2) / mean(exp(train_aapl$log_x))
adjustment_factor_test <- mean(test_aapl$returns^2) / mean(exp(test_aapl$log_x))

train_aapl$RM_adj <- adjustment_factor_train * exp(train_aapl$log_x)
test_aapl$RM_adj <- adjustment_factor_test * exp(test_aapl$log_x)

#finally:
train_aapl$log_x_adj <- log(train_aapl$RM_adj)
test_aapl$log_x_adj <- log(test_aapl$RM_adj)


### MSE ###
forecasted_values <- rolling_forecast_results[rolling_forecast_results != 0] #gets rid of gap of 0s at front of forecasts
actual_values <- test_aapl$log_x_adj[(forecast_horizon):(forecast_horizon + length(forecasted_values) - 1)]

mse_har_garch_forecast <- calculate_mse(actual_values, forecasted_values)
print(mse_har_garch_forecast)


### Plotting: ###
# Combine training data, test data, and forecasts for plotting
train_data_len <- nrow(train_aapl)
test_data_len <- nrow(test_aapl)

combined_data <- data.frame(
  Day = 1:(train_data_len + test_data_len),
  Log_x = c(exp(train_aapl$log_x_adj), exp(test_aapl$log_x_adj)),
  Type = rep(c("Train", "Test"), times = c(train_data_len, test_data_len))
)

# Add fitted values for the training period
combined_data$Fitted <- NA
combined_data$Fitted[1:train_data_len] <- exp(all_results$AAPL.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2)

# Add rolling forecasted values for the test period
combined_data$Forecast <- NA
combined_data$Forecast[(train_data_len + forecast_horizon):(train_data_len + forecast_horizon + length(forecasted_values) - 1)] <- exp(forecasted_values)

p <- ggplot(combined_data, aes(x = Day)) +
  geom_line(aes(y = Log_x, color = "Actual Values")) +
  geom_line(aes(y = Fitted, color = "Fitted Values"), na.rm = TRUE) +
  geom_line(aes(y = Forecast, color = "Forecasted Values"), na.rm = TRUE) +
  geom_vline(xintercept = train_data_len, linetype = "dashed", color = "black") +
  labs(title = "Log Values: Training and 10-Day Forecasting", x = "Day", y = "Log Values") +
  scale_color_manual(values = c("Actual Values" = "blue", "Fitted Values" = "green", "Forecasted Values" = "red")) +
  scale_y_continuous(limits = c(0, 0.03)) +
  theme_minimal()

print(p)

# Histogram of adjusted log_x values
hist(train_aapl$log_x_adj, main = "Histogram of Adjusted Log RM Values", xlab = "Log RM Adjusted", breaks = 50)
hist(train_aapl$log_x, main = "Histogram of Adjusted Log RM Values", xlab = "Log RM Adjusted", breaks = 50)
