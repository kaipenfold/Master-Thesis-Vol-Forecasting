############## Plots for all forecasts ##############
###### APPL ######
# Realised GARCH
appl_vol_forecast_10day_realised <- 100 * exp(all_forecasts$AAPL.csv$realised_garchhorizon10$rolling_forecasts) # 10-day ahead

# HAR-GARCH
appl_vol_forecast_10day_har <- 100 * exp(all_forecasts$AAPL.csv$realised_har_garchhorizon10$rolling_forecasts) # 10-day ahead

# Make dataframe
combined_aapl_10day <- data.frame(
  Day = as.Date(all_results$AAPL.csv$test_data$Day[1:(length(all_results$AAPL.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$AAPL.csv$test_data$RM_adj[1:(length(all_results$AAPL.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = appl_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = appl_vol_forecast_10day_har
)

aapl_date_range <- range(combined_aapl_10day$Day, na.rm = TRUE)


# Plot graph
aapl_10day_plot <- 
  ggplot(combined_aapl_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.3, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "AAPL", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = aapl_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

ggsave("aapl_10day_plot.png", plot = aapl_10day_plot, width = 10, height = 6, dpi = 600)
print(aapl_10day_plot)

# IBM
ibm_vol_forecast_10day_realised <- 100 * exp(all_forecasts$IBM.csv$realised_garchhorizon10$rolling_forecasts) # 10-day ahead
ibm_vol_forecast_10day_har <- 100 * exp(all_forecasts$IBM.csv$realised_har_garchhorizon10$rolling_forecasts) # 10-day ahead

combined_ibm_10day <- data.frame(
  Day = as.Date(all_results$IBM.csv$test_data$Day[1:(length(all_results$IBM.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$IBM.csv$test_data$RM_adj[1:(length(all_results$IBM.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = ibm_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = ibm_vol_forecast_10day_har
)

ibm_date_range <- range(combined_ibm_10day$Day, na.rm = TRUE)


# IBM plot
ibm_10day_plot <- 
  ggplot(combined_ibm_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.3, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "IBM", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = ibm_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(ibm_10day_plot)
ggsave("IBM_10day_plot.png", plot = ibm_10day_plot, width = 10, height = 6, dpi = 600)



#### PG ####
# PG data preparation
pg_vol_forecast_10day_realised <- 100 * exp(all_forecasts$PG.csv$realised_garchhorizon10$rolling_forecasts) # 10-day ahead
pg_vol_forecast_10day_har <- 100 * exp(all_forecasts$PG.csv$realised_har_garchhorizon10$rolling_forecasts) # 10-day ahead

combined_pg_10day <- data.frame(
  Day = as.Date(all_results$PG.csv$test_data$Day[1:(length(all_results$PG.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$PG.csv$test_data$RM_adj[1:(length(all_results$PG.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = pg_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = pg_vol_forecast_10day_har
)

pg_date_range <- range(combined_pg_10day$Day, na.rm = TRUE)

# PG plot
pg_10day_plot <- 
  ggplot(combined_pg_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "PG", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.60), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = pg_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(pg_10day_plot)
ggsave("PG_10day_plot.png", plot = pg_10day_plot, width = 10, height = 6, dpi = 600)


#### WMT ####
# WMT data preparation
wmt_vol_forecast_10day_realised <- 100 * exp(all_forecasts$WMT.csv$realised_garchhorizon10$rolling_forecasts) # 10-day ahead
wmt_vol_forecast_10day_har <- 100 * exp(all_forecasts$WMT.csv$realised_har_garchhorizon10$rolling_forecasts) # 10-day ahead

combined_wmt_10day <- data.frame(
  Day = as.Date(all_results$WMT.csv$test_data$Day[1:(length(all_results$WMT.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$WMT.csv$test_data$RM_adj[1:(length(all_results$WMT.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = wmt_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = wmt_vol_forecast_10day_har
)

wmt_date_range <- range(combined_wmt_10day$Day, na.rm = TRUE)

# WMT plot
wmt_10day_plot <- 
  ggplot(combined_wmt_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.4) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "WMT", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 0.75), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = wmt_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(wmt_10day_plot)
ggsave("WMT_10day_plot.png", plot = wmt_10day_plot, width = 10, height = 6, dpi = 600)


#### JPM ####
# JP data preparation
jpm_realised <- read.csv("realised_garch_horizon_10.csv")
jpm_har <- read.csv("realised_har_garch_horizon_10.csv")
jp_vol_forecast_10day_realised <- 100 * exp(jpm_realised$forecast_data) # 10-day ahead
jp_vol_forecast_10day_har <- 100 * exp(jpm_har$forecast_data) # 10-day ahead

combined_jp_10day <- data.frame(
  Day = as.Date(all_results$JPM.csv$test_data$Day[9:(length(all_results$JPM.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$JPM.csv$test_data$RM_adj[9:(length(all_results$JPM.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = jp_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = jp_vol_forecast_10day_har
)

jp_date_range <- range(combined_jp_10day$Day, na.rm = TRUE)

# JP plot
jp_10day_plot <- 
  ggplot(combined_jp_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.35, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "JP", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.25), breaks = seq(0, 3, by = 0.25)) +
  scale_x_date(limits = jp_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(jp_10day_plot)
ggsave("JP_10day_plot.png", plot = jp_10day_plot, width = 10, height = 6, dpi = 600)



# DIS data preparation
dis_vol_forecast_10day_realised <- 100 * exp(all_forecasts$DIS.csv$realised_garchhorizon10$rolling_forecasts) # 10-day ahead
dis_vol_forecast_10day_har <- 100 * exp(all_forecasts$DIS.csv$realised_har_garchhorizon10$rolling_forecasts) # 10-day ahead

combined_dis_10day <- data.frame(
  Day = as.Date(all_results$DIS.csv$test_data$Day[1:(length(all_results$DIS.csv$test_data$Day))], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$DIS.csv$test_data$RM_adj[1:(length(all_results$DIS.csv$test_data$Day))],
  Realised_Garch_Volatilty_Forecast_10day = dis_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = dis_vol_forecast_10day_har
)

dis_date_range <- range(combined_dis_10day$Day, na.rm = TRUE)

# DIS plot
dis_10day_plot <- 
  ggplot(combined_dis_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "DIS", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = dis_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(dis_10day_plot)
ggsave("DIS_10day_plot.png", plot = dis_10day_plot, width = 10, height = 6, dpi = 600)





## NIKE 
# DIS data preparation
nike_vol_forecast_10day_realised <- 100 * exp(all_forecasts$NKE.csv$realised_garch_horizon_10$rolling_forecasts) # 10-day ahead
nike_vol_forecast_10day_har <- 100 * exp(all_forecasts$NKE.csv$realised_har_garch_horizon_10$rolling_forecasts) # 10-day ahead

combined_dis_10day <- data.frame(
  Day = as.Date(all_results$NKE.csv$test_data$Day[1:(length(all_results$NKE.csv$test_data$Day) - 9)], format = "%Y-%m-%d"),
  Adjusted_RM = 100 * all_results$NKE.csv$test_data$RM_adj[1:(length(all_results$NKE.csv$test_data$Day) - 9)],
  Realised_Garch_Volatilty_Forecast_10day = nike_vol_forecast_10day_realised,
  HAR_Garch_Volatility_Forecast_10day = nike_vol_forecast_10day_har
)

dis_date_range <- range(combined_dis_10day$Day, na.rm = TRUE)

# DIS plot
dis_10day_plot <- 
  ggplot(combined_dis_10day, aes(x = Day)) +
  geom_line(aes(y = Adjusted_RM, color = "Adjusted RM"), size = 0.5) +
  geom_line(aes(y = Realised_Garch_Volatilty_Forecast_10day, color = "Realised GARCH"), size = 0.4, na.rm = TRUE) +
  geom_line(aes(y = HAR_Garch_Volatility_Forecast_10day, color = "Realised HAR-GARCH"), size = 0.4, na.rm = TRUE) +
  labs(x = "DIS", y = "", color = NULL) +
  scale_color_manual(values = c("Adjusted RM" = "black", "Realised GARCH" = "blue", "Realised HAR-GARCH" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 3, by = 0.5)) +
  scale_x_date(limits = dis_date_range, date_labels = "%m/%Y", date_breaks = "6 months") +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )
print(dis_10day_plot)
ggsave("DIS_10day_plot.png", plot = dis_10day_plot, width = 10, height = 6, dpi = 600)


################ ################ ################ ################ ################ ################ ################ ################ ################ 
                                                ################ VaR Plots ################ 
################ ################ ################ ################ ################ ################ ################ ################ ################ 
#### SPY ####
# Extracting VaR forecasts for SPY
VaR_0.05_out_har <- all_forecasts_just_spy$SPY.csv$realised_har_garchhorizon1$VaR_0.05 #these forecasts are the real deal. All these 4 lines.
VaR_0.01_out_har <- all_forecasts_just_spy$SPY.csv$realised_har_garchhorizon1$VaR_0.01
VaR_0.05_out_realised <-  COME_ON_RG$VaR_0.05
VaR_0.01_out_realised <-  COME_ON_RG$VaR_0.01

# Combine in-sample and out-of-sample VaR for SPY
VaR_full_0.05_har <- c(in_sample_var_har_garch$VaR_0.05, VaR_0.05_out_har)
VaR_full_0.01_har <- c(in_sample_var_har_garch$VaR_0.01, VaR_0.01_out_har)
VaR_full_0.05_realised <- c(in_sample_var_realised_garch$VaR_0.05, VaR_0.05_out_realised)
VaR_full_0.01_realised <- c(in_sample_var_realised_garch$VaR_0.01, VaR_0.01_out_realised)

# Preparing combined data for plotting for SPY
combined_data_var_full_spy <- data.frame(
  Day = as.Date(all_results$SPY.csv$full_data$Day[3:length(all_results$SPY.csv$full_data$Day)], format = "%Y-%m-%d"),
  Returns = 100 * all_results$SPY.csv$full_data$returns[3:length(all_results$SPY.csv$full_data$returns)],
  VaR_0.05_har = 100 * VaR_full_0.05_har[3:length(VaR_full_0.05_har)],
  VaR_0.01_har = 100 * VaR_full_0.01_har[3:length(VaR_full_0.01_har)],
  VaR_0.05_realised = 100 * VaR_full_0.05_realised[3:length(VaR_full_0.05_realised)],
  VaR_0.01_realised = 100 * VaR_full_0.01_realised[3:length(VaR_full_0.01_realised)]
)

# Plot VaR for HAR-IV model for SPY
p_var_har_iv_spy <- ggplot(combined_data_var_full_spy, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_spy$Day[nrow(all_results$SPY.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_spy)
ggsave("SPY_VaR_HAR_GARCH.png", plot = p_var_har_iv_spy, width = 13, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for IBM
p_var_realised_spy <- ggplot(combined_data_var_full_spy, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_spy$Day[nrow(all_results$SPY.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_spy)
ggsave("SPY_VaR_Realised_GARCH.png", plot = p_var_realised_spy, width = 13, height = 6, dpi = 600)


#### IBM ####
# Extracting VaR forecasts for IBM
VaR_0.05_out_har <- all_forecasts$IBM.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har <- all_forecasts$IBM.csv$realised_har_garchhorizon1$VaR_0.01
VaR_0.05_out_realised <- all_forecasts$IBM.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised <- all_forecasts$IBM.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for IBM
VaR_full_0.05_har <- c(in_sample_VaR_results$IBM.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har)
VaR_full_0.01_har <- c(in_sample_VaR_results$IBM.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har)
VaR_full_0.05_realised <- c(in_sample_VaR_results$IBM.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised)
VaR_full_0.01_realised <- c(in_sample_VaR_results$IBM.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised)

# Preparing combined data for plotting for IBM
combined_data_var_full_ibm <- data.frame(
  Day = as.Date(all_results$IBM.csv$full_data$Day[3:length(all_results$IBM.csv$full_data$Day)], format = "%Y-%m-%d"),
  Returns = 100 * all_results$IBM.csv$full_data$returns[3:length(all_results$IBM.csv$full_data$returns)],
  VaR_0.05_har = 100 * VaR_full_0.05_har[3:length(VaR_full_0.05_har)],
  VaR_0.01_har = 100 * VaR_full_0.01_har[3:length(VaR_full_0.01_har)],
  VaR_0.05_realised = 100 * VaR_full_0.05_realised[3:length(VaR_full_0.05_realised)],
  VaR_0.01_realised = 100 * VaR_full_0.01_realised[3:length(VaR_full_0.01_realised)]
)

# Plot VaR for HAR-IV model for IBM
p_var_har_iv_ibm <- ggplot(combined_data_var_full_ibm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_ibm$Day[nrow(all_results$IBM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_ibm)
ggsave("IBM_VaR_HAR_GARCH.png", plot = p_var_har_iv_ibm, width = 13, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for IBM
p_var_realised_ibm <- ggplot(combined_data_var_full_ibm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_ibm$Day[nrow(all_results$IBM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_ibm)
ggsave("IBM_VaR_Realised_GARCH.png", plot = p_var_realised_ibm, width = 13, height = 6, dpi = 600)


#### AAPL ####
# Extracting VaR forecasts for AAPL
VaR_0.05_out_har_aapl <- all_forecasts$AAPL.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_aapl <- all_forecasts$AAPL.csv$realised_har_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for AAPL
VaR_full_0.05_har_aapl <- c(in_sample_VaR_results$AAPL.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_aapl)
VaR_full_0.01_har_aapl <- c(in_sample_VaR_results$AAPL.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_aapl)

# Preparing combined data for plotting for AAPL
combined_data_var_full_aapl <- data.frame(
  Day = as.Date(all_results$AAPL.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$AAPL.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_aapl,
  VaR_0.01_har = 100 * VaR_full_0.01_har_aapl
)

# Plot VaR for HAR-IV model for AAPL
p_var_har_iv_aapl <- ggplot(combined_data_var_full_aapl, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_aapl$Day[nrow(all_results$AAPL.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_aapl)
ggsave("AAPL_VaR_HAR_GARCH.png", plot = p_var_har_iv_aapl, width = 13, height = 6, dpi = 600)

# Extracting VaR forecasts for AAPL
VaR_0.05_out_realised_aapl <- all_forecasts$AAPL.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised_aapl <- all_forecasts$AAPL.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for AAPL
VaR_full_0.05_realised_aapl <- c(in_sample_VaR_results$AAPL.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised_aapl)
VaR_full_0.01_realised_aapl <- c(in_sample_VaR_results$AAPL.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised_aapl)

# Preparing combined data for plotting for AAPL
combined_data_var_full_aapl <- data.frame(
  Day = as.Date(all_results$AAPL.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$AAPL.csv$full_data$returns,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_aapl,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_aapl
)

# Plot VaR for Realised GARCH model for AAPL
p_var_realised_aapl <- ggplot(combined_data_var_full_aapl, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_aapl$Day[nrow(all_results$AAPL.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_aapl)
ggsave("AAPL_VaR_Realised_GARCH.png", plot = p_var_realised_aapl, width = 13, height = 6, dpi = 600)


###### BA #######
# Extracting VaR forecasts for BA
VaR_0.05_out_har_ba <- all_forecasts$BA.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_ba <- all_forecasts$BA.csv$realised_har_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for BA
VaR_full_0.05_har_ba <- c(in_sample_VaR_results$BA.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_ba)
VaR_full_0.01_har_ba <- c(in_sample_VaR_results$BA.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_ba)

# Preparing combined data for plotting for BA
combined_data_var_full_ba <- data.frame(
  Day = as.Date(all_results$BA.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$BA.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_ba,
  VaR_0.01_har = 100 * VaR_full_0.01_har_ba
)

# Plot VaR for HAR-IV model for BA
p_var_har_iv_ba <- ggplot(combined_data_var_full_ba, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_ba$Day[nrow(all_results$BA.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_ba)
ggsave("BA_VaR_HAR_GARCH.png", plot = p_var_har_iv_ba, width = 13, height = 6, dpi = 600)

# Extracting VaR forecasts for BA
VaR_0.05_out_har_ba <- all_forecasts$BA.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_har_ba <- all_forecasts$BA.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for BA
VaR_full_0.05_realised_ba <- c(in_sample_VaR_results$BA.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_ba)
VaR_full_0.01_realised_ba <- c(in_sample_VaR_results$BA.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_ba)

# Preparing combined data for plotting for BA
combined_data_var_full_ba <- data.frame(
  Day = as.Date(all_results$BA.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$BA.csv$full_data$returns,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_ba,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_ba
)

# Plot VaR for Realised GARCH model for BA
p_var_realised_ba <- ggplot(combined_data_var_full_ba, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_ba$Day[nrow(all_results$BA.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_ba)
ggsave("BA_VaR_Realised_GARCH.png", plot = p_var_realised_ba, width = 13, height = 6, dpi = 600)


###### GS ######
# Extracting VaR forecasts for GS
VaR_0.05_out_har_gs <- all_forecasts$GS.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_gs <- all_forecasts$GS.csv$realised_har_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for GS
VaR_full_0.05_har_gs <- c(in_sample_VaR_results$GS.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_gs)
VaR_full_0.01_har_gs <- c(in_sample_VaR_results$GS.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_gs)

# Preparing combined data for plotting for GS
combined_data_var_full_gs <- data.frame(
  Day = as.Date(all_results$GS.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$GS.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_gs,
  VaR_0.01_har = 100 * VaR_full_0.01_har_gs
)

# Plot VaR for HAR-IV model for GS
p_var_har_iv_gs <- ggplot(combined_data_var_full_gs, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_gs$Day[nrow(all_results$GS.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_gs)
ggsave("GS_VaR_HAR_GARCH.png", plot = p_var_har_iv_gs, width = 13, height = 6, dpi = 600)

# Extracting VaR forecasts for GS
VaR_0.05_out_har_gs <- all_forecasts$GS.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_har_gs <- all_forecasts$GS.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for GS
VaR_full_0.05_realised_gs <- c(in_sample_VaR_results$GS.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_gs)
VaR_full_0.01_realised_gs <- c(in_sample_VaR_results$GS.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_gs)

# Preparing combined data for plotting for GS
combined_data_var_full_gs <- data.frame(
  Day = as.Date(all_results$GS.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$GS.csv$full_data$returns,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_gs,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_gs
)

# Plot VaR for Realised GARCH model for GS
p_var_realised_gs <- ggplot(combined_data_var_full_gs, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_gs$Day[nrow(all_results$GS.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_gs)
ggsave("GS_VaR_Realised_GARCH.png", plot = p_var_realised_gs, width = 13, height = 6, dpi = 600)


#### JPM ####
# Extracting VaR forecasts for JPM
VaR_0.05_out_har_jpm <- all_forecasts$JPM.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_jpm <- all_forecasts$JPM.csv$realised_har_garchhorizon1$VaR_0.01

VaR_0.05_out_realised_jpm <-  all_forecasts$JPM.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised_jpm <-  all_forecasts$JPM.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for JPM
VaR_full_0.05_har_jpm <- c(in_sample_VaR_results$JPM.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_jpm)
VaR_full_0.01_har_jpm <- c(in_sample_VaR_results$JPM.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_jpm)

VaR_full_0.05_realised_jpm <- c(in_sample_VaR_results$JPM.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised_jpm)
VaR_full_0.01_realised_jpm <- c(in_sample_VaR_results$JPM.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised_jpm)


# Preparing combined data for plotting for JPM
combined_data_var_full_jpm <- data.frame(
  Day = as.Date(all_results$JPM.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$JPM.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_jpm,
  VaR_0.01_har = 100 * VaR_full_0.01_har_jpm,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_jpm,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_jpm
)

# Plot VaR for HAR-IV model for JPM
p_var_har_iv_jpm <- ggplot(combined_data_var_full_jpm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_jpm$Day[nrow(all_results$JPM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_jpm)
ggsave("JPM_VaR_HAR_GARCH.png", plot = p_var_har_iv_jpm, width = 13, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for JPM
p_var_realised_jpm <- ggplot(combined_data_var_full_jpm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_jpm$Day[nrow(all_results$JPM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_jpm)
ggsave("JPM_VaR_Realised_GARCH.png", plot = p_var_realised_jpm, width = 13, height = 6, dpi = 600)


###### MMM ######
# Extracting VaR forecasts for MMM
VaR_0.05_out_har_mmm <- all_forecasts$MMM.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_mmm <- all_forecasts$MMM.csv$realised_har_garchhorizon1$VaR_0.01

VaR_0.05_out_realised_mmm <- all_forecasts$MMM.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised_mmm <- all_forecasts$MMM.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for MMM
VaR_full_0.05_har_mmm <- c(in_sample_VaR_results$MMM.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_mmm)
VaR_full_0.01_har_mmm <- c(in_sample_VaR_results$MMM.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_mmm)

VaR_full_0.05_realised_mmm <- c(in_sample_VaR_results$MMM.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised_mmm)
VaR_full_0.01_realised_mmm <- c(in_sample_VaR_results$MMM.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised_mmm)

# Preparing combined data for plotting for MMM
combined_data_var_full_mmm <- data.frame(
  Day = as.Date(all_results$MMM.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$MMM.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_mmm,
  VaR_0.01_har = 100 * VaR_full_0.01_har_mmm,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_mmm,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_mmm
)

# Plot VaR for HAR-IV model for MMM
p_var_har_iv_mmm <- ggplot(combined_data_var_full_mmm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_mmm$Day[nrow(all_results$MMM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_mmm)
ggsave("MMM_VaR_HAR_GARCH.png", plot = p_var_har_iv_mmm, width = 13, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for MMM
p_var_realised_mmm <- ggplot(combined_data_var_full_mmm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_mmm$Day[nrow(all_results$MMM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_mmm)
ggsave("MMM_VaR_Realised_GARCH.png", plot = p_var_realised_mmm, width = 13, height = 6, dpi = 600)



##### PG #####
# Extracting VaR forecasts for PG
VaR_0.05_out_har_pg <- all_forecasts$PG.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_pg <- all_forecasts$PG.csv$realised_har_garchhorizon1$VaR_0.01

VaR_0.05_out_realised_pg <- all_forecasts$PG.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised_pg <- all_forecasts$PG.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for PG
VaR_full_0.05_har_pg <- c(in_sample_VaR_results$PG.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_pg)
VaR_full_0.01_har_pg <- c(in_sample_VaR_results$PG.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_pg)

VaR_full_0.05_realised_pg <- c(in_sample_VaR_results$PG.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised_pg)
VaR_full_0.01_realised_pg <- c(in_sample_VaR_results$PG.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised_pg)

# Preparing combined data for plotting for PG
combined_data_var_full_pg <- data.frame(
  Day = as.Date(all_results$PG.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$PG.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_pg,
  VaR_0.01_har = 100 * VaR_full_0.01_har_pg,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_pg,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_pg
)

# Plot VaR for HAR-IV model for PG
p_var_har_iv_pg <- ggplot(combined_data_var_full_pg, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_pg$Day[nrow(all_results$PG.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_pg)
ggsave("PG_VaR_HAR_GARCH.png", plot = p_var_har_iv_pg, width = 10, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for PG
p_var_realised_pg <- ggplot(combined_data_var_full_pg, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_pg$Day[nrow(all_results$PG.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_pg)
ggsave("PG_VaR_Realised_GARCH.png", plot = p_var_realised_pg, width = 10, height = 6, dpi = 600)



#### WMT ####
# Extracting VaR forecasts for WMT
VaR_0.05_out_har_wmt <- all_forecasts$WMT.csv$realised_har_garchhorizon1$VaR_0.05
VaR_0.01_out_har_wmt <- all_forecasts$WMT.csv$realised_har_garchhorizon1$VaR_0.01

VaR_0.05_out_realised_wmt <- all_forecasts$WMT.csv$realised_garchhorizon1$VaR_0.05
VaR_0.01_out_realised_wmt <- all_forecasts$WMT.csv$realised_garchhorizon1$VaR_0.01

# Combine in-sample and out-of-sample VaR for WMT
VaR_full_0.05_har_wmt <- c(in_sample_VaR_results$WMT.csv$realised_har_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_har_wmt)
VaR_full_0.01_har_wmt <- c(in_sample_VaR_results$WMT.csv$realised_har_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_har_wmt)

VaR_full_0.05_realised_wmt <- c(in_sample_VaR_results$WMT.csv$realised_garch$VaR_1_days$VaR_0.05, VaR_0.05_out_realised_wmt)
VaR_full_0.01_realised_wmt <- c(in_sample_VaR_results$WMT.csv$realised_garch$VaR_1_days$VaR_0.01, VaR_0.01_out_realised_wmt)

# Preparing combined data for plotting for WMT
combined_data_var_full_wmt <- data.frame(
  Day = as.Date(all_results$WMT.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$WMT.csv$full_data$returns,
  VaR_0.05_har = 100 * VaR_full_0.05_har_wmt,
  VaR_0.01_har = 100 * VaR_full_0.01_har_wmt,
  VaR_0.05_realised = 100 * VaR_full_0.05_realised_wmt,
  VaR_0.01_realised = 100 * VaR_full_0.01_realised_wmt
)

# Plot VaR for HAR-IV model for WMT
p_var_har_iv_wmt <- ggplot(combined_data_var_full_wmt, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_har, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_har, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_wmt$Day[nrow(all_results$WMT.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_har_iv_wmt)
ggsave("WMT_VaR_HAR_GARCH.png", plot = p_var_har_iv_wmt, width = 10, height = 6, dpi = 600)

# Plot VaR for Realised GARCH model for WMT
p_var_realised_wmt <- ggplot(combined_data_var_full_wmt, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = VaR_0.05_realised, color = "VaR 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = VaR_0.01_realised, color = "VaR 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_var_full_wmt$Day[nrow(all_results$WMT.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "VaR 1-day 5%" = "purple", "VaR 1-day 1%" = "orange")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_var_realised_wmt)
ggsave("WMT_VaR_Realised_GARCH.png", plot = p_var_realised_wmt, width = 10, height = 6, dpi = 600)


######################################################################################################################################## 
                            ################################## ES PLOTS################################## 
######################################################################################################################################## 
############
#### SPY ####
# Extracting ES forecasts for SPY
ES_0.05_out_har <- all_forecasts_just_spy$SPY.csv$realised_har_garchhorizon1$ES_0.05
ES_0.01_out_har <- all_forecasts_just_spy$SPY.csv$realised_har_garchhorizon1$ES_0.01
ES_0.05_out_realised <- COME_ON_RG$ES_0.05
ES_0.01_out_realised <- COME_ON_RG$ES_0.01

# Combine in-sample and out-of-sample ES for SPY
ES_full_0.05_har <- c(in_sample_var_har_garch$ES_0.05, ES_0.05_out_har)
ES_full_0.01_har <- c(in_sample_var_har_garch$ES_0.01, ES_0.01_out_har)
ES_full_0.05_realised <- c(in_sample_var_realised_garch$ES_0.05, ES_0.05_out_realised)
ES_full_0.01_realised <- c(in_sample_var_realised_garch$ES_0.01, ES_0.01_out_realised)

# Preparing combined data for plotting for SPY
combined_data_es_full_spy <- data.frame(
  Day = as.Date(all_results$SPY.csv$full_data$Day[3:length(all_results$SPY.csv$full_data$returns)], format = "%Y-%m-%d"),
  Returns = 100 * all_results$SPY.csv$full_data$returns[3:length(all_results$SPY.csv$full_data$returns)],
  ES_0.05_har = 100 * ES_full_0.05_har[3:length(ES_full_0.05_har)],
  ES_0.01_har = 100 * ES_full_0.01_har[3:length(ES_full_0.01_har)],
  ES_0.05_realised = 100 * ES_full_0.05_realised[3:length(ES_full_0.05_realised)],
  ES_0.01_realised = 100 * ES_full_0.01_realised[3:length(ES_full_0.01_realised)]
)

# Plot ES for HAR-IV model for SPY
p_es_har_iv_spy <- ggplot(combined_data_es_full_spy, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_aapl$Day[nrow(all_results$SPY.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_spy)
ggsave("SPY_ES_HAR_GARCH.png", plot = p_es_har_iv_spy, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for IBM
p_es_realised_spy <- ggplot(combined_data_es_full_spy, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_aapl$Day[nrow(all_results$SPY.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_spy)
ggsave("SPY_ES_Realised_GARCH.png", plot = p_es_realised_spy, width = 13, height = 6, dpi = 600)


##############
#### AAPL ####

# Extracting ES forecasts for AAPL
ES_0.05_out_har <- rhg_pred_h1_aapl$ES_0.05
ES_0.01_out_har <- rhg_pred_h1_aapl$ES_0.01
ES_0.05_out_realised <- rg_pred_h1_aapl$ES_0.05
ES_0.01_out_realised <- rg_pred_h1_aapl$ES_0.01

# Combine in-sample and out-of-sample ES for AAPL
ES_full_0.05_har <- c(in_sample_VaR_results$AAPL.csv$realised_har_garch$VaR_1_days$ES_0.05, ES_0.05_out_har)
ES_full_0.01_har <- c(in_sample_VaR_results$AAPL.csv$realised_har_garch$VaR_1_days$ES_0.01, ES_0.01_out_har)
ES_full_0.05_realised <- c(in_sample_VaR_results$AAPL.csv$realised_garch$VaR_1_days$ES_0.05, ES_0.05_out_realised)
ES_full_0.01_realised <- c(in_sample_VaR_results$AAPL.csv$realised_garch$VaR_1_days$ES_0.01, ES_0.01_out_realised)

# Preparing combined data for plotting for AAPL
combined_data_es_full_aapl <- data.frame(
  Day = as.Date(all_results$AAPL.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$AAPL.csv$full_data$returns,
  ES_0.05_har = 100 * ES_full_0.05_har,
  ES_0.01_har = 100 * ES_full_0.01_har,
  ES_0.05_realised = 100 * ES_full_0.05_realised,
  ES_0.01_realised = 100 * ES_full_0.01_realised
)

# Plot ES for HAR-IV model for AAPL
p_es_har_iv_aapl <- ggplot(combined_data_es_full_aapl, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_aapl$Day[nrow(all_results$AAPL.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_aapl)
ggsave("AAPL_ES_HAR_GARCH.png", plot = p_es_har_iv_aapl, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for IBM
p_es_realised_aapl <- ggplot(combined_data_es_full_aapl, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_aapl$Day[nrow(all_results$AAPL.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_aapl)
ggsave("AAPL_ES_Realised_GARCH.png", plot = p_es_realised_aapl, width = 13, height = 6, dpi = 600)

###############
##### IBM #####
###############
# Extracting ES forecasts for IBM
ES_0.05_out_har <- all_forecasts$IBM.csv$realised_har_garchhorizon1$ES_0.05
ES_0.01_out_har <- all_forecasts$IBM.csv$realised_har_garchhorizon1$ES_0.01
ES_0.05_out_realised <- all_forecasts$IBM.csv$realised_garchhorizon1$ES_0.05
ES_0.01_out_realised <- all_forecasts$IBM.csv$realised_garchhorizon1$ES_0.01

# Combine in-sample and out-of-sample ES for IBM
ES_full_0.05_har <- c(in_sample_VaR_results$IBM.csv$realised_har_garch$VaR_1_days$ES_0.05, ES_0.05_out_har)
ES_full_0.01_har <- c(in_sample_VaR_results$IBM.csv$realised_har_garch$VaR_1_days$ES_0.01, ES_0.01_out_har)
ES_full_0.05_realised <- c(in_sample_VaR_results$IBM.csv$realised_garch$VaR_1_days$ES_0.05, ES_0.05_out_realised)
ES_full_0.01_realised <- c(in_sample_VaR_results$IBM.csv$realised_garch$VaR_1_days$ES_0.01, ES_0.01_out_realised)

# Preparing combined data for plotting for IBM
combined_data_es_full_ibm <- data.frame(
  Day = as.Date(all_results$IBM.csv$full_data$Day[3:length(all_results$IBM.csv$full_data$Day)], format = "%Y-%m-%d"),
  Returns = 100 * all_results$IBM.csv$full_data$returns[3:length(all_results$IBM.csv$full_data$returns)],
  ES_0.05_har = 100 * ES_full_0.05_har[3:length(ES_full_0.05_har)],
  ES_0.01_har = 100 * ES_full_0.01_har[3:length(ES_full_0.01_har)],
  ES_0.05_realised = 100 * ES_full_0.05_realised[3:length(ES_full_0.05_realised)],
  ES_0.01_realised = 100 * ES_full_0.01_realised[3:length(ES_full_0.01_realised)]
)

# Plot ES for HAR-IV model for IBM
p_es_har_iv_ibm <- ggplot(combined_data_es_full_ibm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_ibm$Day[nrow(all_results$IBM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_ibm)
ggsave("IBM_ES_HAR_GARCH.png", plot = p_es_har_iv_ibm, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for IBM
p_es_realised_ibm <- ggplot(combined_data_es_full_ibm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_ibm$Day[nrow(all_results$IBM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_ibm)
ggsave("IBM_ES_Realised_GARCH.png", plot = p_es_realised_ibm, width = 13, height = 6, dpi = 600)


###############
##### BA #####
###############
# Extracting ES forecasts for BA
ES_0.05_out_har_ba <- all_forecasts$BA.csv$realised_har_garchhorizon1$ES_0.05
ES_0.01_out_har_ba <- all_forecasts$BA.csv$realised_har_garchhorizon1$ES_0.01

ES_0.05_out_realised_ba <- all_forecasts$BA.csv$realised_garchhorizon1$ES_0.05
ES_0.01_out_realised_ba <- all_forecasts$BA.csv$realised_garchhorizon1$ES_0.01

# Combine in-sample and out-of-sample ES for BA
ES_full_0.05_har_ba <- c(in_sample_VaR_results$BA.csv$realised_har_garch$VaR_1_days$ES_0.05, ES_0.05_out_har_ba)
ES_full_0.01_har_ba <- c(in_sample_VaR_results$BA.csv$realised_har_garch$VaR_1_days$ES_0.01, ES_0.01_out_har_ba)

ES_full_0.05_realised_ba <- c(in_sample_VaR_results$BA.csv$realised_garch$VaR_1_days$ES_0.05, ES_0.05_out_realised_ba)
ES_full_0.01_realised_ba <- c(in_sample_VaR_results$BA.csv$realised_garch$VaR_1_days$ES_0.01, ES_0.01_out_realised_ba)

# Preparing combined data for plotting for BA
combined_data_es_full_ba <- data.frame(
  Day = as.Date(all_results$BA.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$BA.csv$full_data$returns,
  ES_0.05_har = 100 * ES_full_0.05_har_ba,
  ES_0.01_har = 100 * ES_full_0.01_har_ba,
  ES_0.05_realised = 100 * ES_full_0.05_realised_ba,
  ES_0.01_realised = 100 * ES_full_0.01_realised_ba
)

# Plot ES for HAR-IV model for BA
p_es_har_iv_ba <- ggplot(combined_data_es_full_ba, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_ba$Day[nrow(all_results$BA.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_ba)
ggsave("BA_ES_HAR_GARCH.png", plot = p_es_har_iv_ba, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for BA
p_es_realised_ba <- ggplot(combined_data_es_full_ba, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_ba$Day[nrow(all_results$BA.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_ba)
ggsave("BA_ES_Realised_GARCH.png", plot = p_es_realised_ba, width = 13, height = 6, dpi = 600)


###############
##### JPM #####
###############
# Extracting ES forecasts for JPM
ES_0.05_out_har_jpm <- all_forecasts$JPM.csv$realised_har_garchhorizon1$ES_0.05
ES_0.01_out_har_jpm <- all_forecasts$JPM.csv$realised_har_garchhorizon1$ES_0.01

ES_0.05_out_realised_jpm <-  all_forecasts$JPM.csv$realised_garchhorizon1$ES_0.05
ES_0.01_out_realised_jpm <-  all_forecasts$JPM.csv$realised_garchhorizon1$ES_0.01

# Combine in-sample and out-of-sample ES for JPM
ES_full_0.05_har_jpm <- c(in_sample_VaR_results$JPM.csv$realised_har_garch$VaR_1_days$ES_0.05, ES_0.05_out_har_jpm)
ES_full_0.01_har_jpm <- c(in_sample_VaR_results$JPM.csv$realised_har_garch$VaR_1_days$ES_0.01, ES_0.01_out_har_jpm)

ES_full_0.05_realised_jpm <- c(in_sample_VaR_results$JPM.csv$realised_garch$VaR_1_days$ES_0.05, ES_0.05_out_realised_jpm)
ES_full_0.01_realised_jpm <- c(in_sample_VaR_results$JPM.csv$realised_garch$VaR_1_days$ES_0.01, ES_0.01_out_realised_jpm)

# Preparing combined data for plotting for JPM
combined_data_es_full_jpm <- data.frame(
  Day = as.Date(all_results$JPM.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$JPM.csv$full_data$returns,
  ES_0.05_har = 100 * ES_full_0.05_har_jpm,
  ES_0.01_har = 100 * ES_full_0.01_har_jpm,
  ES_0.05_realised = 100 * ES_full_0.05_realised_jpm,
  ES_0.01_realised = 100 * ES_full_0.01_realised_jpm
)

# Plot ES for HAR-IV model for JPM
p_es_har_iv_jpm <- ggplot(combined_data_es_full_jpm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_jpm$Day[nrow(all_results$JPM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_jpm)
ggsave("JPM_ES_HAR_GARCH.png", plot = p_es_har_iv_jpm, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for JPM
p_es_realised_jpm <- ggplot(combined_data_es_full_jpm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_jpm$Day[nrow(all_results$JPM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-30, 30), breaks = seq(-40, 40, by = 10)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_jpm)
ggsave("JPM_ES_Realised_GARCH.png", plot = p_es_realised_jpm, width = 13, height = 6, dpi = 600)


###############
##### MMM #####
###############
# Extracting ES forecasts for MMM
ES_0.05_out_har_mmm <- all_forecasts$MMM.csv$realised_har_garchhorizon1$ES_0.05
ES_0.01_out_har_mmm <- all_forecasts$MMM.csv$realised_har_garchhorizon1$ES_0.01

ES_0.05_out_realised_mmm <- all_forecasts$MMM.csv$realised_garchhorizon1$ES_0.05
ES_0.01_out_realised_mmm <- all_forecasts$MMM.csv$realised_garchhorizon1$ES_0.01

# Combine in-sample and out-of-sample ES for MMM
ES_full_0.05_har_mmm <- c(in_sample_VaR_results$MMM.csv$realised_har_garch$VaR_1_days$ES_0.05, ES_0.05_out_har_mmm)
ES_full_0.01_har_mmm <- c(in_sample_VaR_results$MMM.csv$realised_har_garch$VaR_1_days$ES_0.01, ES_0.01_out_har_mmm)

ES_full_0.05_realised_mmm <- c(in_sample_VaR_results$MMM.csv$realised_garch$VaR_1_days$ES_0.05, ES_0.05_out_realised_mmm)
ES_full_0.01_realised_mmm <- c(in_sample_VaR_results$MMM.csv$realised_garch$VaR_1_days$ES_0.01, ES_0.01_out_realised_mmm)

# Preparing combined data for plotting for MMM
combined_data_es_full_mmm <- data.frame(
  Day = as.Date(all_results$MMM.csv$full_data$Day, format = "%Y-%m-%d"),
  Returns = 100 * all_results$MMM.csv$full_data$returns,
  ES_0.05_har = 100 * ES_full_0.05_har_mmm,
  ES_0.01_har = 100 * ES_full_0.01_har_mmm,
  ES_0.05_realised = 100 * ES_full_0.05_realised_mmm,
  ES_0.01_realised = 100 * ES_full_0.01_realised_mmm
)

# Plot ES for HAR-IV model for MMM
p_es_har_iv_mmm <- ggplot(combined_data_es_full_mmm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_har, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_har, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_mmm$Day[nrow(all_results$MMM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1),   
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_har_iv_mmm)
ggsave("MMM_ES_HAR_GARCH.png", plot = p_es_har_iv_mmm, width = 13, height = 6, dpi = 600)

# Plot ES for Realised GARCH model for MMM
p_es_realised_mmm <- ggplot(combined_data_es_full_mmm, aes(x = Day)) +
  geom_line(aes(y = Returns, color = "Daily Returns"), linetype = "solid") +
  geom_line(aes(y = ES_0.05_realised, color = "ES 1-day 5%"), na.rm = TRUE, linetype = "dashed",size = 0.3) +
  geom_line(aes(y = ES_0.01_realised, color = "ES 1-day 1%"), na.rm = TRUE, linetype = "dashed" ,size = 0.3) +
  geom_vline(xintercept = combined_data_es_full_mmm$Day[nrow(all_results$MMM.csv$train_data)], linetype = "dashed", color = "black") +
  labs(x = NULL, y = NULL, color = NULL, fill = NULL) +
  scale_color_manual(values = c("Daily Returns" = "black", "ES 1-day 5%" = "blue", "ES 1-day 1%" = "red")) +
  theme_minimal() +
  scale_y_continuous(limits = c(-25, 25), breaks = seq(-20, 20, by = 5)) +
  scale_x_date(date_labels = "%m/%Y", date_breaks = "2 year") +
  theme(
    legend.position = c(0.05, 0.95),  # Position legend inside the plot area (top left)
    legend.justification = c(0, 1), 
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    axis.ticks.length = unit(0.2, "cm"),  # Add tick marks
    axis.ticks = element_line(color = "black"),  # Color of tick marks
    axis.line = element_blank(),  # Remove axis lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.6)  # Add border with uniform thickness
  )

print(p_es_realised_mmm)
ggsave("MMM_ES_Realised_GARCH.png", plot = p_es_realised_mmm, width = 13, height = 6, dpi = 600)
