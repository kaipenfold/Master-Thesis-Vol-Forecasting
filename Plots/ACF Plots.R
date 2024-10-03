library(astsa)
# ACF Plots
params_har_garch <- all_results$SPY.csv$fitted_models$realised_har_garch$optimal_params
omega <- params_har_garch[1]
beta <- params_har_garch[2]
gamma_d <- params_har_garch[3]
gamma_w <- params_har_garch[4]
gamma_m <- params_har_garch[5]
xi <- params_har_garch[6]
phi <- params_har_garch[7]
tau1 <- params_har_garch[8]
tau2 <- params_har_garch[9]
sigma_ut <- params_har_garch[10]

# Separate Coefficients
ar_coeffs <- c(
  beta + phi * gamma_d,
  rep(phi * gamma_w / 4, 4),  # for lags 2 to 5
  rep(phi * gamma_m / 17, 17)  # for lags 6 to 22
)
ma_coeffs <- c(
  gamma_d,
  rep(gamma_w / 4, 4),  # for lags 2 to 5
  rep(gamma_m / 17, 17)  # for lags 6 to 22
)


# Function to evaluate polynomial
polyval <- function(p, x) {
  n <- length(p)
  y <- rep(0, length(x))
  for (i in 1:n) {
    y <- y + p[i] * x^(n - i)
  }
  return(y)
}

# Frequencies
n_freq <- 1000
omega <- seq(-pi, pi, length.out = n_freq)

# Compute the spectral density manually
arma_spectral_density <- function(omega, ar_poly, ma_poly, sigma2) {
  num <- abs(polyval(ma_poly, exp(-1i * omega)))^2
  denom <- abs(polyval(ar_poly, exp(-1i * omega)))^2
  return((sigma2 / (2 * pi)) * num / denom)
}

spectral_density_manual <- arma_spectral_density(omega, ar_coeffs, ma_coeffs, sigma2 = sigma_ut)


spec <- arma.spec(ar = ar_coeffs, ma = ma_coeffs, var.noise = sigma_ut, n.freq = n_freq, plot = FALSE)
spectral_density_arma_spec <- spec$spec

# Plot to compare
library(ggplot2)

df <- data.frame(
  omega = omega,
  Manual = spectral_density_manual,
  arma_spec = spectral_density_arma_spec
)

ggplot(df, aes(x = omega)) +
  geom_line(aes(y = Manual, color = "Manual")) +
  geom_line(aes(y = arma_spec, color = "arma.spec")) +
  labs(title = "Spectral Density Comparison", y = "Spectral Density") +
  scale_color_manual(name = "Method", values = c("Manual" = "blue", "arma.spec" = "red"))


# Calculate Theoretical ACF from Manual Spectral Density
acf_theoretical_manual <- Re(fft(spectral_density_manual, inverse = TRUE))
acf_theoretical_manual <- acf_theoretical_manual / acf_theoretical_manual[1]  # Normalize the ACF

# Ensure we only take the first half of the ACF, as the FFT will give us a symmetric result
acf_theoretical_manual <- acf_theoretical_manual[1:(length(acf_theoretical_manual) / 2)]
acf_theoretical_manual <- acf_theoretical_manual[1:100]  # Take first 100 lags for plotting

# Assuming log_fitted_vol is your log-fitted volatility series
log_fitted_vol <- all_results$SPY.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2  # Replace with actual log-fitted values

# Sample ACF
acf_sample <- acf(log_fitted_vol, plot = FALSE)
acf_sample_df <- data.frame(lag = acf_sample$lag, acf = acf_sample$acf)
acf_theoretical_df_manual <- data.frame(lag = 0:(length(acf_theoretical_manual)-1), acf = acf_theoretical_manual)

# Plot the ACFs
ggplot(acf_sample_df, aes(x = lag, y = acf)) +
  geom_line(color = "black") +
  geom_line(data = acf_theoretical_df_manual, aes(x = lag, y = acf), color = "blue", linetype = "dashed") +
  labs(title = "Theoretical vs Sample ACF: HAR-GARCH (Manual Spectral Density)", x = "Lags", y = "ACF") +
  ylim(0, 1)






























# Calculate Spectral Density
spec <- arma.spec(ar = ar_coeffs, ma = ma_coeffs, var.noise = sigma_ut, n.freq = 1000, plot = FALSE)
spectral_density <- spec$spec

# Calculate Theoretical ACF from Spectral Density
acf_theoretical <- Re(fft(spectral_density, inverse = TRUE))
acf_theoretical <- acf_theoretical / acf_theoretical[1]  # Normalize the ACF

# Ensure we only take the first half of the ACF, as the FFT will give us a symmetric result
acf_theoretical <-  acf_theoretical[1:100]

# Plot the ACFs
acf_sample <- acf(all_results$SPY.csv$fitted_models$realised_har_garch$fitted_values$log_sigma2, lag.max = 100, plot = FALSE)
acf_sample_df <- data.frame(lag = acf_sample$lag, acf = acf_sample$acf)
acf_theoretical_df <- data.frame(lag = 0:(length(acf_theoretical)-1), acf = acf_theoretical)

ggplot(acf_sample_df, aes(x = lag, y = acf)) +
  geom_line(color = "black") +
  geom_line(data = acf_theoretical_df, aes(x = lag, y = acf), color = "red", linetype = "dashed") +
  labs(title = "Theoretical vs Sample ACF: HAR-GARCH", x = "Lags", y = "ACF") + ylim(0, 1)

