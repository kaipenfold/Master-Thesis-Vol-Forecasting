##This script calculate the results for each dataset and put it into a table###
library(esback)
library(GAS)

calculate_metrics <- function(actual, predicted){
  mse <- mse(actual - mean(actual), predicted - mean(predicted))
  rmse <- rmse(actual - mean(actual), predicted - mean(predicted))
  qlike <- mean((actual / predicted) - log(actual / predicted) - 1)
  return(list(mse = mse, rmse = rmse, qlike = qlike))
}

calculate_metrics_for_dataset <- function(combined_data, model_types) {
  metrics_results <- data.frame(Model = character(), Dataset = character(), Metric = character(), Value = numeric(), stringsAsFactors = FALSE)
  
  # Filter for train and test sets
  train_data <- combined_data %>% filter(TrainTest == 'Train')
  test_data <- combined_data %>% filter(TrainTest == 'Test')
  
  for (model_type in model_types) {
    # Determine actual column based on model type
    if (model_type == "standard_garch") {
      actual_train <- train_data$RM_adj
      actual_test <- test_data$RM_adj
    } else {
      actual_train <- train_data$log_x_adj
      actual_test <- test_data$log_x_adj
    }
    
    # In-sample metrics
    predicted_train <- train_data[[paste0(model_type, '_fitted')]]
    in_sample_metrics <- calculate_metrics(actual_train, predicted_train)
    
    for (metric_name in names(in_sample_metrics)) {
      metrics_results <- rbind(metrics_results, 
                               data.frame(Model = model_type,
                                          Dataset = "AAPL.csv",
                                          Metric = paste0("In-sample ", metric_name),
                                          Value = in_sample_metrics[[metric_name]]))
    }
    
    # Out-of-sample metrics
    predicted_test <- test_data[[paste0(model_type, '_forecast_1d')]]  # Change `_forecast_1d` if needed
    out_sample_metrics <- calculate_metrics_for_model(actual_test, predicted_test)
    
    for (metric_name in names(out_sample_metrics)) {
      metrics_results <- rbind(metrics_results, 
                               data.frame(Model = model_type,
                                          Dataset = "AAPL.csv",
                                          Metric = paste0("Out-of-sample ", metric_name),
                                          Value = out_sample_metrics[[metric_name]]))
    }
  }
  
  return(metrics_results)
}


# combined_data <- combined_results_all[["AAPL.csv"]]
# metrics_results_aapl <- calculate_metrics_for_dataset(combined_data, model_types)
# print(metrics_results_aapl)


#### #### #### #### #### #### #### #### #### #### 
##### VaR TESTS #####
compute_var_metrics <- function(returns, VaR, alpha) {
  # Ensure returns and VaR are of the same length
  returns <- as.numeric(returns)
  VaR <- as.numeric(-VaR)
  
  n <- length(returns)
  if (length(VaR) != n) stop("Returns and VaR must be of the same length.")
  
  # Hit Test
  H <- ifelse(returns < VaR, 1, 0)
  
  # UC Test
  VaR_test <- VaRTest(alpha, returns, VaR, 1 - alpha)
  Expected_exceedances <- VaR_test$expected.exceed
  Actual_exceedances <- VaR_test$actual.exceed
  
  UC_LRstat <- VaR_test$uc.LRstat
  UC_critical <- VaR_test$uc.critical
  UC_LRp <- VaR_test$uc.LRp
  UC_Decision <- VaR_test$uc.Decision
  
  # CC Test
  CC_LRstat <- VaR_test$cc.LRstat
  CC_critical <- VaR_test$cc.critical
  CC_LRp <- VaR_test$cc.LRp
  CC_Decision <- VaR_test$cc.Decision
  
  # Violation Rate
  VRate <- mean(H)
  
  # Print Violation Rates
  print(paste("Violation Rate: ", round(VRate,5)))
  print(paste("Desired Level (alpha): ", alpha))
  
  # Compare Violation Rate with the desired level (alpha)
  # VRate_comparison <- ifelse(abs(VRate - alpha) < .Machine$double.eps^0.5, "equal", "not equal")
  
  
  return(list(
    H = H,
    VRate = VRate * 100 ,
    "True VRate" = alpha,
    # VRate_comparison = VRate_comparison,
    
    "Expected exceedances:" = Expected_exceedances,
    "Actual exceedances:" = Actual_exceedances,
    UC_LRstat = UC_LRstat,
    UC_critical = UC_critical,
    UC_LRp = UC_LRp,
    UC_Decision = UC_Decision,
    
    CC_LRstat = CC_LRstat,
    CC_critical = CC_critical,
    CC_LRp = CC_LRp,
    CC_Decision = CC_Decision
  ))
}












x <- compute_var_metrics(all_results$SPY.csv$test_data$returns, VaR = spy_var_forecast_1day_har_garch$VaR_0.01, alpha = 0.01)

#### #### #### #### #### #### #### #### #### #### #### #### #### 
#### EXPECTED SHORTFALL TESTS #### 
compute_es_metrics <- function(returns,VaR, ES, alpha) {
  # Ensure returns, VaR, and ES are of the same length
  n <- length(returns)
  if (length(VaR) != n || length(ES) != n) stop("Returns, VaR, and ES must be of the same length.")
  
  # V1 and V2 calculations
  delta_t <- returns - ES
  K_alpha <- which(returns < -VaR)
  T1 <- length(K_alpha)
  V1 <- sum(delta_t[K_alpha]) / T1
  
  T2 <- length(which(delta_t < quantile(delta_t, alpha)))
  tau_alpha <- which(delta_t < quantile(delta_t, alpha))
  V2 <- sum(delta_t[tau_alpha]) / T2
  
  V_alpha <- (abs(V1) + abs(V2)) / 2
  
  esr_backtest(r = all_results$AAPL.csv$test_data$returns, q = -all_forecasts$AAPL.csv$realised_har_garch_horizon_1$VaR_0.05, e = -all_forecasts$AAPL.csv$realised_har_garch_horizon_1$ES_0.05, alpha = 0.025, version = 1)
  
  FZLoss(data = all_results$MSFT.csv$test_data$returns[1:1269], VaR = -har_garch_forecast$VaR_0.01, ES = -har_garch_forecast$ES_0.01, alpha = 0.01)
  
  return(list(
    V_alpha = V_alpha,
    ESR_p_value =
    FZ_loss = 
  ))
}

banana <-  BacktestVaR(all_results$SPY.csv$test_data$returns,VaR = -spy_var_forecast_1day_har_garch$VaR_0.05, alpha = 0.05, Lags = 4)


## WORK IN PROGRES: ES-Regression Backtest
esr_backtest(r = all_results$AAPL.csv$test_data$returns, q = -all_forecasts$AAPL.csv$realised_har_garch_horizon_1$VaR_0.05, e = -all_forecasts$AAPL.csv$realised_har_garch_horizon_1$ES_0.05, alpha = 0.025, version = 1,B = 500)

##### ##### ##### ##### ##### ##### ##### 
##### Joint VaR and ES Evaluation #####

## FZ Loss Function
FZ_loss_har <- FZLoss(data = all_results$MSFT.csv$test_data$returns[1:1269], VaR = -har_garch_forecast$VaR_0.01, ES = -har_garch_forecast$ES_0.01, alpha = 0.01)
FZ_loss_realised <- FZLoss(data = all_results$MSFT.csv$test_data$returns[1:1269], VaR = -realised_garch_forecast$VaR_0.01, ES = -realised_garch_forecast$ES_0.01, alpha = 0.01)

x <- compute_es_metrics(all_results$MSFT.csv$test_data$returns[1:1269], VaR = -har_garch_forecast$VaR_0.01, ES = -har_garch_forecast$ES_0.01, alpha = 0.01)
y <- compute_es_metrics(all_results$MSFT.csv$test_data$returns[1:1269], VaR = -realised_garch_forecast$VaR_0.01, ES = -realised_garch_forecast$ES_0.01, alpha = 0.01)