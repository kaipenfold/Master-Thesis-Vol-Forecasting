# Parameters for Realized GARCH
params_realised_garch <- all_results$IBM.csv$fitted_models$realised_garch$optimal_params
params_realised_garch <- c(0.16, 0.6, 0.394, -0.41, 0.95, -0.085, 0.117, 0.240)
omega <- params_realised_garch[1]
beta <- params_realised_garch[2]
gamma <- params_realised_garch[3]
xi <- params_realised_garch[4]
phi <- params_realised_garch[5]
tau1 <- params_realised_garch[6]
tau2 <- params_realised_garch[7]
sigma_ut <- params_realised_garch[8]

# Define AR and MA coefficients for an ARMA(22,22) model based on the structure of the Realized GARCH
# Since the realized GARCH is specified as an ARMA(1,1), we will map this to a (22,22) structure for comparison.
# This is more conceptual since the actual realized GARCH doesn't have 22 lags explicitly.
ar_coeffs <-   beta + gamma * phi  # Lag 1

ma_coeffs <-  gamma  # Lag 1


# Calculate the theoretical ACF using ARMAacf for ARMA(22,22)
acf_theoretical <- ARMAacf(ar = ar_coeffs, ma = ma_coeffs, lag.max = 100, pacf = FALSE)

# Assuming log_fitted_vol_realized_garch is your log-fitted volatility series for Realized GARCH
# Replace log_fitted_vol_realized_garch with the actual log-fitted volatility series
log_fitted_vol_realized_garch <- all_results$IBM.csv$fitted_models$realised_garch$fitted_values$log_sigma2  # Replace with actual log-fitted values

# Calculate the sample ACF
acf_sample <- acf(log_fitted_vol_realized_garch, lag.max = 100, plot = FALSE)
acf_sample_df <- data.frame(lag = acf_sample$lag, acf = acf_sample$acf)
acf_theoretical_df <- data.frame(lag = 0:(length(acf_theoretical) - 1), acf = acf_theoretical)

# Plot the ACFs
ggplot(acf_sample_df, aes(x = lag, y = acf)) +
  geom_line(color = "black") +
  geom_line(data = acf_theoretical_df, aes(x = lag, y = acf), color = "blue", linetype = "dashed") +
  labs(title = "Theoretical vs Sample ACF: Realized GARCH (ARMA(22,22))", x = "Lags", y = "ACF") +
  ylim(0, 1)
























# Parameters for HAR-GARCH
optimal_params <- all_results$IBM.csv$fitted_models$realised_har_garch$optimal_params
optimal_params <- c(0.252, 0.388, 0.425, 0.114, 0.075, -0.417, 0.953, -0.085, 0.087, 0.24)

omega <- optimal_params[1]
beta <- optimal_params[2]
gamma_d <- optimal_params[3]
gamma_w <- optimal_params[4]
gamma_m <- optimal_params[5]
xi <- optimal_params[6]
phi <- optimal_params[7]
tau1 <- optimal_params[8]
tau2 <- optimal_params[9]
sigma_ut <- optimal_params[10]

# Define AR and MA coefficients based on HAR-GARCH structure
ar_coeffs <- c(
  beta + phi * gamma_d,  # Lag 1
  rep(phi * gamma_w / 4, 4),  # Lags 2 to 5
  rep(phi * gamma_m / 17, 17)  # Lags 6 to 22
)
ma_coeffs <- c(
  gamma_d,  # Lag 1
  rep(gamma_w / 4, 4),  # Lags 2 to 5
  rep(gamma_m / 17, 17)  # Lags 6 to 22
)

# Calculate the theoretical ACF using ARMAacf
acf_theoretical <- ARMAacf(ar = ar_coeffs, ma = ma_coeffs, lag.max = 100, pacf = FALSE)

# Assuming log_fitted_vol is your log-fitted volatility series
# Replace log_fitted_vol with the actual log-fitted volatility series
log_fitted_vol <- all_results$IBM.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2  # Replace with actual log-fitted values

# Calculate the sample ACF
acf_sample <- acf(log_fitted_vol, lag.max = 100, plot = FALSE)
acf_sample_df <- data.frame(lag = acf_sample$lag, acf = acf_sample$acf)
acf_theoretical_df <- data.frame(lag = 0:(length(acf_theoretical) - 1), acf = acf_theoretical)

# Plot the ACFs
ggplot(acf_sample_df, aes(x = lag, y = acf)) +
  geom_line(color = "black") +
  geom_line(data = acf_theoretical_df, aes(x = lag, y = acf), color = "blue", linetype = "dashed") +
  labs(title = "Theoretical vs Sample ACF: HAR-GARCH", x = "Lags", y = "ACF") +
  ylim(0, 1)

