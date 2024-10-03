library(MASS)
library(sn)  # Load the sn package for skew-normal distribution

realised_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Getting model parameters
  omega <- params[1]
  beta <- params[2]
  gamma <- params[3]
  xi <- params[4]
  phi <- params[5]
  tau1 <- params[6]
  tau2 <- params[7]
  sigma_ut <- params[8]
  
  mu_x <- phi * omega + xi * (1 - beta)
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    end <- start + forecast_horizon - 1
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    log_x_t <- current_train_data$log_x[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        log_x_i <- mu_x + (beta + phi * gamma) * log_x_t + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        log_sigma_i <- omega + beta * log_sigma_t + gamma * log_x_i
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      log_x_t <- mean(simulated_log_x)
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    sigma_t <- sqrt(exp(log_sigma_t))
    
    # Fit skew-normal distribution to in-sample residuals
    fit <- selm(z ~ 1, family = "SN")
    dp <- fit@param$dp
    location <- dp["xi"]
    scale <- dp["omega"]
    shape <- dp["alpha"]
    
    # Debugging output
    print(paste("Location:", location, "Scale:", scale, "Shape:", shape))
    
    # Ensure parameters are correctly extracted
    if (length(scale) == 0 || length(shape) == 0 || length(location) == 0) {
      stop("Failed to extract distribution parameters")
    }
    
    # Calculate VaR and ES using skew-normal distribution
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}
COME_ON_RG <- realised_garch_var_forecast(all_results$SPY.csv$fitted_models$realised_garch$optimal_params, "realised_garch", forecast_horizon = 1, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data)

har_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  
  # Extracting fitted values from the model
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Getting model parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  xi <- params[6]
  phi <- params[7]
  tau1 <- params[8]
  tau2 <- params[9]
  sigma_ut <- params[10]
  
  # Calculating mu_x, a constant term used in the measurement equation
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize vectors to store forecasts and risk measures
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day values for calculating RM_week and RM_month
  rm_history <- as.list(train_data$RM_day)
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point for rolling forecast
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    rm_history <- append(rm_history, list(RM_day))
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      
      rm_history <- append(rm_history, list(RM_day))
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    sigma_t <- sqrt(exp(log_sigma_t))
    
    # Fit skew-normal distribution to in-sample residuals
    fit <- selm(z ~ 1, family = "SN")
    params <- coef(fit)
    dp <- fit@param$dp
    scale <- dp["omega"]
    shape <- dp["alpha"]
    location <- dp[1]

    
    # Debugging output
    print(paste("Location:", location, "Scale:", scale, "Shape:", shape))
    
    # Ensure parameters are correctly extracted
    if (length(scale) == 0 || length(shape) == 0 || length(location) == 0) {
      stop("Failed to extract distribution parameters")
    }
    
    # Calculate VaR and ES using skew-normal distribution
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}
COME_ON_2 <- har_garch_var_forecast(params = all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data, alpha_0.05,alpha_0.01, p=8)
COME_ON_2 <- har_garch_var_forecast(params = all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data, alpha_0.05,alpha_0.01, p=8)

# 1 Day Forecasts SPY
# Combine the results into a data frame for COME_ON_RG
COME_ON_RG_df <- data.frame(
  rolling_forecasts = COME_ON_RG$rolling_forecasts,
  VaR_0.05 = COME_ON_RG$VaR_0.05,
  VaR_0.01 = COME_ON_RG$VaR_0.01,
  ES_0.05 = COME_ON_RG$ES_0.05,
  ES_0.01 = COME_ON_RG$ES_0.01
)

# Combine the results into a data frame for COME_ON_2
COME_ON_2_df <- data.frame(
  rolling_forecasts = COME_ON_2$rolling_forecasts,
  VaR_0.05 = COME_ON_2$VaR_0.05,
  VaR_0.01 = COME_ON_2$VaR_0.01,
  ES_0.05 = COME_ON_2$ES_0.05,
  ES_0.01 = COME_ON_2$ES_0.01
)
# Write the data frames to CSV files
write.csv(COME_ON_RG_df, file = "Realised_GARCH_VaR_1day_SPY.csv", row.names = FALSE)
write.csv(COME_ON_2_df, file = "Har_GARCH_VaR_1day_SPY.csv", row.names = FALSE)


# 10 Day Forecasts SPY
COME_ON_RG_10_DAY <- realised_garch_var_forecast(all_results$SPY.csv$fitted_models$realised_garch$optimal_params, "realised_garch", forecast_horizon = 10, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data)
COME_ON_2_10_DAY <- har_garch_var_forecast(params = all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon = 10, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data, alpha_0.05,alpha_0.01, p=8)

COME_ON_RG_10_day_df <- data.frame(
  rolling_forecasts = COME_ON_RG_10_DAY$rolling_forecasts,
  VaR_0.05 = COME_ON_RG_10_DAY$VaR_0.05,
  VaR_0.01 = COME_ON_RG_10_DAY$VaR_0.01,
  ES_0.05 = COME_ON_RG_10_DAY$ES_0.05,
  ES_0.01 = COME_ON_RG_10_DAY$ES_0.01
)

# Combine the results into a data frame for COME_ON_2
COME_ON_2_10_day_df <- data.frame(
  rolling_forecasts = COME_ON_2_10_DAY$rolling_forecasts,
  VaR_0.05 = COME_ON_2_10_DAY$VaR_0.05,
  VaR_0.01 = COME_ON_2_10_DAY$VaR_0.01,
  ES_0.05 = COME_ON_2_10_DAY$ES_0.05,
  ES_0.01 = COME_ON_2_10_DAY$ES_0.01
)

# Write the data frames to CSV files
write.csv(COME_ON_RG_10_day_df, file = "Realised_GARCH_VaR_10day_SPY.csv", row.names = FALSE)
write.csv(COME_ON_2_10_day_df, file = "Har_GARCH_VaR_10day_SPY.csv", row.names = FALSE)






