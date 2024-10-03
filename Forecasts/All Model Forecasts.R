library(skewt)
##This script performs forecasts###
# Define the standardised Student's t-distribution function
# standardised_student_t <- function(df, gamma) {
#   student.t(chi = df, gamma = gamma, mu = 0, sigma = 1)
# }

standardised_student_t <- function(nu, gamma) {
  student.t(nu = nu, gamma = gamma, mu = 0)
}
#student.t(x = z, mu = 0.03890325, sigma = 1, lambda = -0.5, skew = 0.876)
######################################################################################################
### IN-SAMPLE: 1-day horizon VaR for 5% and 1% Levels ###
######################################################################################################
# Define in-sample VaR calculation function
in_sample_all_models_VaR <- function(model, results, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  if (model %in% c("standard_garch", "realised_har_iv_garch")) {
    return(NULL)
  }
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    # Generate p quantiles evenly spaced between 0 and alpha, including alpha
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  
  sigma2 <- results$fitted_models[[model]]$fitted_values$sigma2
  z <- results$fitted_models[[model]]$fitted_values$z
  
  # Fit skew-normal distribution to in-sample residuals
  fit <- selm(z ~ 1, family = "SN")
  dp <- fit@param$dp
  location <- dp["xi"]
  scale <- dp["omega"]
  shape <- dp["alpha"]
  
  # Debugging output
  print(paste("Location:", location, "Scale:", scale, "Shape:", shape))
  
  # Ensure parameters are correctly extracted
  if (is.na(location) || is.na(scale) || is.na(shape)) {
    stop("Failed to extract distribution parameters")
  }
  
  in_sample_var_0.05 <- numeric(length(sigma2))
  in_sample_var_0.01 <- numeric(length(sigma2))
  in_sample_es_0.05 <- numeric(length(sigma2))
  in_sample_es_0.01 <- numeric(length(sigma2))
  all_var_quantiles_0.05 <- matrix(0, nrow = length(sigma2), ncol = p)
  all_var_quantiles_0.01 <- matrix(0, nrow = length(sigma2), ncol = p)
  
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  for (t in 1:length(sigma2)) {
    sigma_t <- sqrt(sigma2[t])
    
    # Calculate VaR and ES using skew-normal distribution
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    in_sample_var_0.05[t] <- VaR_quantiles_0.05[1]
    in_sample_var_0.01[t] <- VaR_quantiles_0.01[1]
    in_sample_es_0.05[t] <- mean(VaR_quantiles_0.05[1:p])
    in_sample_es_0.01[t] <- mean(VaR_quantiles_0.01[1:p])
    
    all_var_quantiles_0.05[t, ] <- VaR_quantiles_0.05
    all_var_quantiles_0.01[t, ] <- VaR_quantiles_0.01
  }
  
  return(list(
    VaR_0.05 = in_sample_var_0.05, 
    VaR_0.01 = in_sample_var_0.01, 
    ES_0.05 = in_sample_es_0.05, 
    ES_0.01 = in_sample_es_0.01,
    quantiles_0.05 = quantiles_0.05,
    quantiles_0.01 = quantiles_0.01,
    all_var_quantiles_0.05 = all_var_quantiles_0.05,
    all_var_quantiles_0.01 = all_var_quantiles_0.01
  ))
}


in_sample_var_realised_garch <- in_sample_all_models_VaR("realised_garch", results = all_results$SPY.csv)
in_sample_var_har_garch <- in_sample_all_models_VaR("realised_har_garch",results = all_results$SPY.csv)


######################################################################################################
### Out of Sample VaR Forecasts Function ###
######################################################################################################
## Realised GARCH
realised_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    # Generate p quantiles evenly spaced between 0 and alpha, including alpha
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # VaR Setup
  fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  #params_gh <- coef(fit)
  df <- fit@chi
  skew <- fit@gamma[1]
  
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
    VaR_dist <- standardised_student_t(df, skew)
    #VaR_dist <- fit    
    VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(alpha, VaR_dist) * sigma_t * sqrt(forecast_horizon))
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}

## Realised HAR-GARCH
har_garch_var_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # # VaR Setup
  # fit <- fit.ghypuv(data = z, symmetric = FALSE)
  # params_gh <- coef(fit)
  # # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # # df <- fit@chi
  # # skew <- fit@gamma[1]
  
  fit <- fitdist(distribution = "norm", x = z)
  df <- fit$pars[4]
  skew <- fit$pars[3]

  # mu <-  0.03890325
  # sigma <- 0.952 
  # #lambda <-  -0.5
  # gamma <- 0.876
  
  # Getting our parameters
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
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialise a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)

  # Buffers to store past RM_day values
  rm_history <- as.list(train_data$RM_day)
  
  # Calculate quantiles
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)

  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) { 
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week and RM_month
    rm_history <- append(rm_history, list(RM_day))
    # Calculate RM_week
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    # Calculate RM_month
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
    
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
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      # Calculate the mean of the simulated log_sigma values
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      
      # Update RM_week and RM_month dynamically for the next step
      rm_history <- append(rm_history, list(RM_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      # Update epsilon_t_prev for the next iteration using the mean of the current iteration's values
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    # Store the forecast
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    VaR_dist <- standardised_student_t(df, skew)
    # VaR_dist <- fit
    VaR_quantiles_0.05 <- sapply(quantiles_0.05, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)
    VaR_quantiles_0.01 <- sapply(quantiles_0.01, function(alpha) qghyp(alpha, VaR_dist) * sigma_t)

    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}
spy_7th_har <- har_garch_var_forecast(params = all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data, alpha_0.05,alpha_0.01, p=8)


forecast_iv_arma <- function(iv_history, forecast_horizon) {
  # Fit ARMA(1,1) model
  arma_model <- Arima(iv_history, order = c(1, 0, 1))
  # Forecast future IV values
  iv_forecast <- forecast(arma_model, h = forecast_horizon)
  return(as.numeric(iv_forecast$mean))
}

## Realised HAR-IV-GARCH
har_iv_garch_forecast <- function(params, model, forecast_horizon, results, train_data, test_data, alpha_0.05 = 0.05, alpha_0.01 = 0.01, p = 8) {
  # Function to calculate quantiles
  calculate_quantiles <- function(alpha, p) {
    alpha_values <- rev(seq(0, alpha, length.out = p + 1)[-1])
    return(alpha_values)
  }
  # Extracting fitted values
  z <- results$fitted_models[[model]]$fitted_values$z
  ut <- results$fitted_models[[model]]$fitted_values$ut
  log_sigma <- results$fitted_models[[model]]$fitted_values$log_sigma2
  
  # Fitting skewed t-distribution
  # fit <- fit.ghypuv(data = z, symmetric = FALSE, lambda = -0.5)
  # df <- fit@chi
  # skew <- fit@gamma[1]
  # 
  # Model parameters
  omega <- params[1]
  beta <- params[2]
  gamma_d <- params[3]
  gamma_w <- params[4]
  gamma_m <- params[5]
  gamma_iv_d <- params[6]
  xi <- params[7]
  phi <- params[8]
  tau1 <- params[9]
  tau2 <- params[10]
  sigma_ut <- params[11]
  
  # Calculating mu_x
  mu_x <- phi * omega + xi * (1 - beta)
  
  # Initialize a list to store forecasts
  rolling_forecasts <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_VaR_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.05 <- numeric(nrow(test_data) - forecast_horizon + 1)
  rolling_ES_0.01 <- numeric(nrow(test_data) - forecast_horizon + 1)
  
  # Buffers to store past RM_day and IV_day values
  rm_history <- as.list(train_data$RM_day)
  iv_history <- as.list(train_data$IV_day)
  
  # Calculate quantiles
  quantiles_0.05 <- calculate_quantiles(alpha_0.05, p)
  quantiles_0.01 <- calculate_quantiles(alpha_0.01, p)
  
  # Perform rolling forecast
  for (start in 1:(nrow(test_data) - forecast_horizon + 1)) {
    end <- start + forecast_horizon - 1
    
    # Use the training data up to the current point
    current_train_data <- rbind(train_data, test_data[1:(start-1), ])
    log_sigma_t <- log_sigma[length(log_sigma)]
    RM_day <- current_train_data$RM_day[nrow(current_train_data)]
    IV_day <- current_train_data$IV_day[nrow(current_train_data)]
    z_t_prev <- z[length(z)]
    u_t_prev <- ut[length(ut)]
    epsilon_t_prev <- tau1 * z_t_prev + tau2 * (z_t_prev^2 - 1) + u_t_prev
    
    # Update RM_week, RM_month, IV_week, and IV_month
    rm_history <- append(rm_history, list(RM_day))
    iv_history <- append(iv_history, list(IV_day))
    
    RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
    RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))

    # Forecast IV using ARMA(1,1) model
    iv_forecast <- forecast_iv_arma(unlist(iv_history), forecast_horizon)
    
    # Combining z and ut to simulate values
    Wt <- cbind(z, ut)
    
    for (m in 1:forecast_horizon) {
      sim_pairs <- Wt[sample(nrow(Wt), 5000, replace = TRUE), ]
      simulated_log_x <- numeric(5000)
      simulated_log_sigma <- numeric(5000)
      
      for (i in 1:5000) {
        z_i <- sim_pairs[i, 1]
        u_i <- sim_pairs[i, 2]
        epsilon_t <- tau1 * z_i + tau2 * (z_i^2 - 1) + u_i
        
        # Update log_x using the measurement equation
        log_x_i <- mu_x + (beta + phi * gamma_d) * RM_day + (phi * gamma_w * RM_week) + (phi * gamma_m * RM_month) +
          (beta + phi * gamma_iv_d) * IV_day + epsilon_t - beta * epsilon_t_prev
        simulated_log_x[i] <- log_x_i
        
        # Get forecasted IV
        iv_i <- iv_forecast[m]
        
        # Calculate log_sigma
        log_sigma_i <- omega + beta * log_sigma_t + gamma_d * RM_day + gamma_w * RM_week + gamma_m * RM_month + 
          gamma_iv_d * IV_day
        simulated_log_sigma[i] <- log_sigma_i
      }
      
      log_sigma_t <- mean(simulated_log_sigma)
      RM_day <- mean(simulated_log_x)
      IV_day <- iv_forecast[m]
      
      rm_history <- append(rm_history, list(RM_day))
      iv_history <- append(iv_history, list(IV_day))
      
      RM_week <- mean(unlist(rm_history[(length(rm_history)-5):(length(rm_history)-2)]))
      RM_month <- mean(unlist(rm_history[(length(rm_history)-22):(length(rm_history)-6)]))
      
      epsilon_t_prev <- mean(tau1 * sim_pairs[, 1] + tau2 * (sim_pairs[, 1]^2 - 1) + sim_pairs[, 2])
    }
    
    rolling_forecasts[start + forecast_horizon - 1] <- log_sigma_t
    
    # Calculate VaR using standardised skewed student's t-distribution
    sigma_t <- sqrt(exp(log_sigma_t))
    # VaR_dist <- standardised_student_t(df, skew)
    
    fit <- selm(z ~ 1, family = "SN")
    params <- coef(fit)
    dp <- fit@param$dp
    scale <- dp["omega"]
    shape <- dp["alpha"]
    location <- dp[1]
    
    VaR_quantiles_0.05 <- qsn(quantiles_0.05, xi = location, omega = scale, alpha = shape) * sigma_t
    VaR_quantiles_0.01 <- qsn(quantiles_0.01, xi = location, omega = scale, alpha = shape) * sigma_t
    
    rolling_VaR_0.05[start + forecast_horizon - 1] <- VaR_quantiles_0.05[1]
    rolling_VaR_0.01[start + forecast_horizon - 1] <- VaR_quantiles_0.01[1]
    rolling_ES_0.05[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.05[1:p])
    rolling_ES_0.01[start + forecast_horizon - 1] <- mean(VaR_quantiles_0.01[1:p])
  }
  return(list(rolling_forecasts = rolling_forecasts, VaR_0.05 = rolling_VaR_0.05, VaR_0.01 = rolling_VaR_0.01, ES_0.05 = rolling_ES_0.05, ES_0.01 = rolling_ES_0.01))
}


### RUNNING ###
#params <- all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params
forecast_horizon <- 1 # 1-day ahead forecast
alpha_0.05 <- 0.05
alpha_0.01 <- 0.01 

# In-Sample NEED TO PASS p NOW AS WELL
#in_sample_results_har <- in_sample_all_models_VaR("realised_har_garch", forecast_horizon, all_results$IBM.csv, alpha_0.05, alpha_0.01, p = 8)
#in_sample_results_rgarch <- in_sample_all_models_VaR("realised_garch", forecast_horizon, all_results$SPY.csv, alpha_0.05, alpha_0.01, p = 8)
#in_sample_results_har_iv <- in_sample_all_models_VaR("realised_har_iv_garch", forecast_horizon, all_results$AAPL.csv, alpha_0.05, alpha_0.01, p = 8)


# Out of Sample (same with)
har_garch_forecast_1day <- har_garch_var_forecast(all_results$IBM.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$IBM.csv, all_results$IBM.csv$train_data, all_results$IBM.csv$test_data, alpha_0.05, alpha_0.01, p = 8)
realised_garch_forecast_1day <- realised_garch_var_forecast(all_results$IBM.csv$fitted_models$realised_garch$optimal_params, "realised_garch", forecast_horizon, all_results$IBM.csv, all_results$IBM.csv$train_data, all_results$IBM.csv$test_data, alpha_0.05, alpha_0.01, p = 8)

spy_har_garch_1day <- har_garch_var_forecast(all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$SPY.csv, all_results$SPY.csv$train_data, all_results$SPY.csv$test_data, alpha_0.05, alpha_0.01, p = 8)
spy_har_garch_10day <- har_garch_var_forecast(all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params, "realised_har_garch", forecast_horizon, all_results$SPY.csv, all_results$SPY.csv$train_data, all_results$SPY.csv$test_data, alpha_0.05, alpha_0.01, p = 8)


#har_iv_params_test <- c(0.4, 0.15, 0.15, 0.0001, 0.1, 0.0005, 0.0005, 0.000005, -0.1, 0.95, -0.01, 0.03, 0.2)

har_iv_garch_forecast_1day <- har_iv_garch_forecast(raph_iv_test_params, "realised_har_iv_garch", forecast_horizon, all_results$AAPL.csv, all_results$AAPL.csv$train_data, all_results$AAPL.csv$test_data, alpha_0.05, alpha_0.01, p = 8)

raph_iv_test_params <- c(0.252, 0.47, 0.325, 0.29, -0.1, 0.3, -0.417, 0.953, -0.085, 0.087, 0.88)
start_params_r_har_iv_garch <- c(0.00252, 0.15, 0.25, 0.114, 0.075, 0.001, -0.385, 0.953, -0.285, 0.287, 0.24) #paper's params

all_results$AAPL.csv$param_table
#SPY TESTING
spy_7th_june_test2 <- realised_garch_var_forecast(params = all_results$SPY.csv$fitted_models$realised_garch$optimal_params, "realised_garch", forecast_horizon, all_results$SPY.csv, train_data = all_results$SPY.csv$train_data, test_data = all_results$SPY.csv$test_data, alpha_0.05,alpha_0.01, p=8)





mse(all_results$AAPL.csv$test_data$log_x_adj, har_iv_garch_forecast_1day$rolling_forecasts) #har garch 


# 1-day MSE SPY
mse(all_results$SPY.csv$test_data$log_x_adj, har_garch_forecast_1day$rolling_forecasts) #har garch 
mse(all_results$SPY.csv$test_data$log_x_adj, realised_garch_forecast_1day$rolling_forecasts) #realised garch 



# Extracting VaR & ES forecasts
VaR_0.05_out <- har_iv_garch_forecast_1day$VaR_0.05
VaR_0.01_out <- har_iv_garch_forecast_1day$VaR_0.01
ES_0.05_out <- har_iv_garch_forecast_1day$ES_0.05
ES_0.01_out <- har_iv_garch_forecast_1day$ES_0.01

# # Extracting VaR & ES forecasts
# VaR_0.05_out <- realised_garch_forecast$VaR_0.05
# VaR_0.01_out <- realised_garch_forecast$VaR_0.01
# ES_0.05_out <- realised_garch_forecast$ES_0.05
# ES_0.01_out <- realised_garch_forecast$ES_0.01


# Combine in-sample and out-of-sample VaR & ES
VaR_full_0.05 <- c(in_sample_results_har_iv$VaR_0.05, VaR_0.05_out)
VaR_full_0.01 <- c(in_sample_results_har_iv$VaR_0.01, VaR_0.01_out)
ES_full_0.05 <- c(in_sample_results_har_iv$ES_0.05, ES_0.05_out)
ES_full_0.01 <- c(in_sample_results_har_iv$ES_0.01, ES_0.01_out)

# Preparing combined data for plotting
all_results$AAPL.csv$full_data$Day <- as.Date(all_results$AAPL.csv$full_data$Day, format = "%Y-%m-%d")
aapl_data_len <- nrow(all_results$AAPL.csv$full_data)
combined_data_var_full<- data.frame(
  Day = all_results$AAPL.csv$full_data$Day,
  #Log_x = all_results$AAPL.csv$full_data$log_x_adj,
  Returns = all_results$AAPL.csv$full_data$returns,
  VaR_0.05 = c(rep(NA, length(in_sample_results_har_iv$VaR_0.05)), VaR_0.05_out),
  VaR_0.01 = c(rep(NA, length(in_sample_results_har_iv$VaR_0.01)), VaR_0.01_out),
  ES_0.05 = c(rep(NA, length(in_sample_results_har_iv$ES_0.05)), ES_0.05_out),
  ES_0.01 = c(rep(NA, length(in_sample_results_har_iv$ES_0.01)), ES_0.01_out)
)

# Add VaR and ES to the combined data
combined_data_var_full$VaR_0.05 <- VaR_full_0.05
combined_data_var_full$VaR_0.01 <- VaR_full_0.01
combined_data_var_full$ES_0.05 <- ES_full_0.05
combined_data_var_full$ES_0.01 <- ES_full_0.01

# Plot VaR, ES, and daily returns
p_var_es_full <- ggplot(combined_data_var_full, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = -VaR_0.05, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dotted") +
  geom_line(aes(y = -VaR_0.01, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dotted") +
  #geom_line(aes(y = -ES_0.05, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed") +
  #geom_line(aes(y = -ES_0.01, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed") +
  #geom_ribbon(aes(ymin = -ES_0.05, ymax = -VaR_0.05), fill = "lightgrey", alpha = 0.4, show.legend = FALSE) +
  #geom_ribbon(aes(ymin = -ES_0.01, ymax = -VaR_0.01), fill = "grey", alpha = 0.6, show.legend = FALSE) +
  geom_vline(xintercept = combined_data_var_full$Day[nrow(train_aapl)], linetype = "dashed", color = "black") +
  labs(title = "VaR and ES: 1-Day Forecasts at 5% and 1% Levels with Daily Returns", x = "Year", y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange", 
                                "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  scale_y_continuous(
    limits = c(-0.25, 0.25),  # Adjust these limits based on your data range
    #labels = function(x) paste0(x, "%")  # Add percentage sign to labels
  ) +
  #scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal() + 
  theme(legend.position = "bottom")

print(p_var_es_full)


x <- compute_var_metrics(returns = all_results$SPY.csv$test_data$returns[1:1276], VaR = har_garch_forecast$VaR_0.05 , alpha = 0.01)
x$VRate
x$UC_LRstat

VaRTest(alpha = 0.05, actual = all_results$SPY.csv$test_data$returns[1:1276], VaR = - har_garch_forecast$VaR_0.01, conf.level = 0.99)



sn::
skew <- 1.5  # Example skewness parameter, adjust as necessary
df <- 7  # Example degrees of freedom, adjust as necessary

VaR_dist <- dst(x = 0.05, alpha = skew, nu = df)




