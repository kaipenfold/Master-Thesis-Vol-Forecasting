########################################################################################################
#                                          FUNCTIONS (IN ORDER)
########################################################################################################

######### (1): LOADING AND PREPROCESSING DATA ######### 
load_and_preprocess <- function(min_data_path, daily_data_path, iv_data_path){
  #Reading datasets
  min_data <- read.csv(min_data_path)
  daily_data <- read.csv(daily_data_path)
  iv_data <- read.csv(iv_data_path)
  
  #Transforming Date column into Date format
  min_data$Date <- ymd_hms(min_data$Date)
  daily_data$Date <- ymd(daily_data$Date)
  iv_data$Date <- ymd(iv_data$Date)
  
  #Filtering out 9:30 am obs from min data
  min_data <- min_data[format(as.POSIXct(min_data$Date), "%H:%M:%S") != "09:30:00", ]
  
  #As we are going to lose the first observation, let's slice the data to begin from the second obs onwwards
  min_data <- min_data[-1, ]
  daily_data <- daily_data[-1, ]
  iv_data <- iv_data[-1, ]
  
  return(list(min_data = min_data, daily_data = daily_data, iv_data = iv_data))
  
}


######### (2:) CHECK DAY HAS SUFFICIENT NO. OF 5-MIN INTERVALS ######### 
check_intervals <- function(data, threshold = 66) {
  # Ensure data is a data frame
  data <- as.data.frame(data)
  
  # Count the number of 5-minute intervals per 'Day'
  daily_counts <- aggregate(log_ret ~ Day, data = data, FUN = length)
  
  # Identify days with less than the threshold number of intervals
  days_below_threshold <- daily_counts[daily_counts$log_ret < threshold, "Day"]
  
  # Print the days below the threshold that will be dropped
  if(length(days_below_threshold) > 0) {
    cat("Dropping days with intervals below the threshold:", days_below_threshold, "\n")
  } else {
    cat("No days with intervals below the threshold found.\n")
  }
  
  # Drop the days below the threshold from the dataset
  data <- data[!data$Day %in% days_below_threshold,]
  
  # Return the modified dataset as a data frame
  return(data)
}



######### (3): CALCULATING REALISED VARIANCE (RV) ######### 
calculate_realized_var <- function(data) {
  # Data here is a 5 min dataframe with log returns
  realized_var <- rRVar(xts(data$log_ret, order.by = data$Date), makeReturns = FALSE)
  colnames(realized_var) <- 'RV'
  return(tail(realized_var, -1))
}


######### (4): GLOBAL DATA HEALTH CHECK ######### 
check_data_health <- function(data_list) {
  results <- list() # To store results of checks for each dataset
  
  #Check lengths being the same
  lengths <- sapply(data_list, nrow)
  if(length(unique(lengths)) != 1) {
    stop("Error: Not all dataframes have the same length. Check the lengths of your datasets.")
  } else {
    results$length_check <- "ALL OK"
  }
  
  #Check number of NAs in the data
  na_counts <- sapply(data_list, function(df) sum(is.na(df)))
  if(any(na_counts > 0)) {
    results$na_check <- paste("Warning: There are NAs in the datasets. Counts are:", toString(na_counts))
  } else {
    results$na_check <- "ALL OK"
  }
  
  #Check to see if the time indexing is the same across all datasets
  # Assuming 'Date' column exists and is the first column
  time_index_checks <- sapply(data_list, function(df) all(df$Date == data_list[[1]]$Date))
  if(any(!time_index_checks)) {
    stop("Error: Time indexing differs across datasets. Check the 'Date' columns.")
  } else {
    results$time_index_check <- "ALL OK"
  }
  
  #Check for unexpected data types
  type_checks <- sapply(data_list, function(df) all(sapply(df, class) == sapply(data_list[[1]], class)))
  if(any(!type_checks)) {
    stop("Error: Data types differ across datasets. Inspect the classes of your dataframe columns.")
  } else {
    results$type_check <- "ALL OK"
  }
  
  return(results)
}



######### (5): TBV ######### 
calculating_TBV <- function(data) {
  #We pass the 5 minutes data into this function
  
  #Mean, std and threshold (we could potentially set up threshold outside the function...)
  data_mean_ret <- mean(data$log_ret_abs, na.rm = TRUE)
  std_ret <- sd(data$log_ret_abs, na.rm = TRUE)
  threshold <- data_mean_ret + 2* std_ret
  
  #Calculating the continuous component
  data$Continuous <- with(data, ifelse(log_ret_abs <= threshold & log_ret_abs_lagged <= threshold,
                                       log_ret_abs * log_ret_abs_lagged, NA))
  
  daily_tbv <- data %>%
    group_by(Day) %>%
    summarize(TBV = sum(Continuous, na.rm = TRUE))
  daily_tbv <- daily_tbv[2:nrow(daily_tbv),] #dropping first observation (as TBV starts on 10th, but RV starts on 11th)
  colnames(daily_tbv) <-"TBV"
  
  return(xts(daily_tbv$TBV, order.by = as.Date(daily_tbv$Day)))
  
}


######### (6): JUMP Component ######### 




######### (7): HAR Model Estimation ######### 
fit_HAR <- function(data, model_type = 'all', periods = c(1,5,22)){
  results <- list()
  
  
  if (model_type == 'all' || 'HAR' %in% model_type){
    har <- HARmodel(data$RV, periods = periods)
    results$HAR <- fitted(har)
  }
  
  if (model_type == 'all' || 'HAR_IV'%in% model_type){
    har_iv <- HARmodel(data$RV, periods = periods, externalRegressor = data$VIX)
    results$HAR_IV <- fitted(har_iv)
  }
  if (model_type == 'all' || 'HARJ' %in% model_type){
    harj <- HARmodel(data$RV, periods = periods)
    results$HARJ <- fitted(harj)
  }
  
  if (model_type == 'all' || 'HARCJ'%in% model_type){
    har_cj <- HARmodel(data$RV, periods = periods)
    results$HAR_CJ <- fitted(harcj)
  }
  
  if (model_type == 'all' || 'HARJ_IV' %in% model_type){
    harj_iv <- HARmodel(data$RV, periods = periods, externalRegressor = data$VIX)
    results$HARJ_IV <- fitted(harj_iv)
  }
  
  if (model_type == 'all' || 'HARCJ_IV'%in% model_type){
    harcj_iv <- HARmodel(data$RV, periods = periods, externalRegressor = data$VIX )
    results$HARCJ_IV <- fitted(harcj_iv)
  }
  
  
  return(results)
}


model_evaluation <- function(models) {
  results <- data.frame()
  for (model_name in names(models)) {
    
    mse <- calculate_mse(models[[model_name]])
    mafe <- calculate_mafe(models[[model_name]])
    qlike <- calculate_qlike(models[[model_name]])
    
    results <- rbind(results, data.frame(Model = model_name, MSE = mse, MAFE = mafe, Qlike = qlike))
  }
  return(results)
}
bns


