library('forecast')
library('caret')
library('caretForecast')
library('ranger')
library('xgboost')
library('kernlab')


################################################################################
### DATA PREPROCESSING #########################################################
################################################################################

### Section 2.2.3 and 2.3 in the paper. The preprocessing steps before are not
### included as they would reveal too many business internals.

# Read in the endogenous variables.
df_endog <- read.csv('input_output_data.csv')
df_endog$time <- as.Date(df_endog$time, format = '%Y-%m-%d')
names(df_endog)[names(df_endog) == 'time'] <- 'date'

# Read in the exogenous variables.
df_exog <- read.csv('exogenous.csv', stringsAsFactors = TRUE)
df_exog$holiday <- as.logical(df_exog$holiday)
names(df_exog)[names(df_exog) == 'X'] <- 'date'

# Merge the endogenous and exogenous variables to one frame.
df <- merge(df_endog, df_exog)

# Introduce weekday variable.
df$weekday <- as.factor(rep(1:7, length.out=1375))

# Shift the weather data by 10 days.
var_weather <- c('humidity', 'precipitation_height', 'snow_depth',
                 'sunshine_duration', 'temperature_air_max_2m',
                 'temperature_air_mean_2m')

df[10:nrow(df), var_weather] <- df[1:(nrow(df) - 9), var_weather]

# Remove unnecessary data points.
df <- df[29:nrow(df), ]
df <- df[df$weekday != 6 & df$weekday != 7, ]

# Introduce off_dist variable.
holidays <- which(df$sum_materials == 0)

df$off_dist <- 0
df$off_dist[holidays + 1] <- 1
df$off_dist[holidays - 1] <- 2
df$off_dist <- as.factor(df$off_dist)

# Introduce off_days variable.
df$off_days <- 0

for (i in seq(1, 960, by=5)){
  inds <- i + 0:4
  df$off_days[inds] <- length(intersect(inds, holidays))
}

df$off_days <- as.factor(df$off_days)

# Remove the holidays.
df <- df[-holidays, ]

# Store the data in time series structure.
ts_truth <- ts(df, frequency = 5)

var_endog <- c('GTK', 'Folie', 'Hohlkörper', 'MKh', 'MKw', 'NE.metall...ALU',
               'PPK', 'PE', 'PET.flaschen', 'PET.schale', 'PP', 'PS',
               'Sortierreste')

var_exog <- c('tonnes', 'humidity', 'precipitation_height', 'snow_depth',
              'sunshine_duration', 'temperature_air_max_2m',
              'temperature_air_mean_2m', 'season', 'holiday',
              'weekday', 'off_dist', 'off_days')

ts_exog <- ts_truth[ , var_exog]


################################################################################
### MODEL DEFINITION ###########################################################
################################################################################

fcast_holt_winters <- function(x, h) {
  forecast(forecast::hw(x, h=h), h=h)
}


fcast_ets <- function(x, h) {
  forecast(forecast::ets(x), h=h)
}


fcast_arima <- function(x, h, xreg=NULL, newxreg=NULL) {
  forecast(auto.arima(x, xreg=xreg), h=h, xreg=newxreg)
}


fcast_rf <- function(x, h, xreg=NULL, newxreg=NULL) {
  
  # See Table 2 in the paper.
  params <- expand.grid(mtry=c(1,4,7), splitrule='variance', min.node.size=c(1,5,10))
  
  model <- caretForecast::ARml(x, max_lag=30, caret_method='ranger',
                               tune_grid=params, cv_horizon=10,
                               xreg=xreg)
  
  forecast(model, h=h, xreg=newxreg)
}


fcast_ert <- function(x, h, xreg=NULL, newxreg=NULL) {
  
  # See Table 2 in the paper.
  params <- expand.grid(mtry=c(1,4,7), splitrule='extratrees', min.node.size=c(1,5,10))
  
  model <- caretForecast::ARml(x, max_lag=30, caret_method='ranger',
                               tune_grid=params, cv_horizon=10,
                               xreg=xreg)
  
  forecast(model, h=h, xreg=newxreg)
}


fcast_xgbTree <- function(x, h, xreg=NULL, newxreg=NULL) {
  
  # See Table 3 in the paper.
  params <- expand.grid(nrounds=150, max_depth=c(1,2,3), eta=c(0.3,0.4), gamma=0,
                        colsample_bytree=c(0.6, 0.8), min_child_weight=1,
                        subsample=c(0.5, 0.75, 1))
  
  model <- caretForecast::ARml(x, max_lag=30, caret_method='xgbTree',
                               tune_grid=params, cv_horizon=10, xreg=xreg)
  
  forecast(model, h=h, xreg=newxreg)
}


fcast_gprLin <- function(x, h, xreg=NULL, newxreg=NULL) {
  
  # See Table 4 in the paper.
  params <- expand.grid(parameter='none')
  
  model <- caretForecast::ARml(x, max_lag=30, caret_method='gaussprLinear',
                               tune_grid=params, cv_horizon=10,
                               xreg=xreg, allow_parallel=TRUE)
  
  forecast(model, h=h, xreg=newxreg)
}


fcast_gprRBF <- function(x, h, xreg=NULL, newxreg=NULL) {
  
  # See Table 4 in the paper.
  params<- expand.grid(sigma=c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5))
  
  model <- caretForecast::ARml(x, max_lag=30, caret_method='gaussprRadial',
                               tune_grid=params, cv_horizon=10,
                               xreg=xreg, allow_parallel=TRUE)
  
  forecast(model, h=h, xreg=newxreg)
}

fcast_funcs <- c(fcast_holt_winters, fcast_ets, fcast_arima, fcast_rf, fcast_ert,
                 fcast_xgbTree, fcast_gprLin, fcast_gprRBF)

fcast_funcs_names <- c('HW', 'ETS', 'ARIMA', 'RF', 'ERT', 'XGBoost',
                       'GPR linear', 'GPR RBF')


################################################################################
### MODEL EVALUATION WITH ROLLING CROSS-VALIDATION #############################
################################################################################

# Transform the residuals to one RMSE value.
res_to_rmse <- function(res, ts_port=NULL) {
  mean(sqrt(rowMeans(res[658:929, ]^2)))
}

# Transform the residuals to one sMAPE value.
res_to_smape <- function(res, ts_port) {
  
  # The actual values on the time period of interest
  # (needed to count back the predictions from the residuals).
  actual <- t(mapply(function(i) ts_port[seq(i, i+9)], 658:929))
  
  # Calculate the denominator of the sMAPE formula.
  denom <- (abs(actual) + abs(res[658:929] - actual)) / 2
  # If the denominator is 0, the prediction coincides with the actual value.
  # By setting the denominator to Inf, the corresponding data point will
  # enter the calculation as 0, which is wanted since it is a perfect prediction.
  denom[denom == 0] <- Inf
  
  # Put the pieces together to calculate the sMAPE.
  mean(rowMeans(abs(res[658:929]) / denom))
}


# Evaluate the given model on a given endogenous variable with respect to a given evaluation function.
model_eval <- function(fcast_func, endog, eval_func, xreg=NULL) {
  
  # Calculate the proportions time series for the given endogenous variable.
  ts_port <- ts_truth[ , endog] / ts_truth[ , 'sum_materials']
  
  # Perform the time series cross-validation.
  set.seed(123)
  res <- tsCV(ts_port, fcast_func, h=10, initial=657, xreg=xreg)
  
  # Evaluate the model.
  return(eval_func(res, ts_port))
}


# Evaluate the naive approach on a given endogenous variable with respect to a given evaluation function.
naive_last_eval <- function(endog, eval_func) {
  
  # Calculate the proportions time series for the given endogenous variable.
  ts_port <- ts_truth[ , endog] / ts_truth[ , 'sum_materials']
  
  # Build up a matrix that has the differences between the last value (prediction)
  # and the actual value (truth) as entries. These are the residuals of the 
  res <- matrix(NA, nrow=939, ncol=10)
  res[658:929, ] <- t(mapply(function(i) diff(ts_port)[seq(i, i+9)], 658:929))
  
  # Evaluate the naive last approach.
  return(eval_func(res, ts_port))
}


# For iterating over the forecast functions and endogenous variables.
library('foreach')

# Matrix of RMSE values without incorporating external factors.
# See Table 5 in the paper.
mat_rmse <- foreach(fcast_func=fcast_funcs, .combine='cbind') %:%
  foreach(endog=var_endog, .combine='c') %do%
  model_eval(fcast_func, endog, res_to_rmse)

# Add the naive approach to the matrix.
mat_rmse <- cbind(mat_rmse, mapply(naive_last_eval, var_endog, MoreArgs=list(eval_func=res_to_rmse)))

# Add the Mean and Percentage deviation rows to the matrix.
mat_rmse <- rbind(mat_rmse,
                  colMeans(mat_rmse),                           # Calculation of the Mean row.
                  colMeans(mat_rmse / apply(mat_rmse, 1, min))) # Calculation of the Percentage deviation row.
rownames(mat_rmse) <- c(var_endog, 'Mean', 'Percentage deviation')
colnames(mat_rmse) <- c(fcast_funcs_names, 'Naive')


# Matrix of sMAPE values without incorporating external factors.
# See Table 6 in the paper.
mat_smape <- foreach(fcast_func=fcast_funcs, .combine='cbind') %:%
  foreach(endog=var_endog, .combine='c') %do%
  model_eval(fcast_func, endog, res_to_smape)

# Add the naive approach to the matrix.
mat_smape <- cbind(mat_smape, mapply(naive_last_eval, var_endog, MoreArgs=list(eval_func=res_to_smape)))

# Add the Mean and Percentage deviation rows to the matrix.
mat_smape <- rbind(mat_smape,
                   colMeans(mat_smape),                                    # Calculation of the Mean row.
                   colMeans(mat_smape / apply(mat_smape[ , 1:8], 1, min))) # Calculation of the Percentage deviation row (excluding Naive last).
rownames(mat_smape) <- c(var_endog, 'Mean', 'Percentage deviation')
colnames(mat_smape) <- c(fcast_funcs_names, 'Naive')


# Matrix of RMSE values with incorporating external factors.
# See Table 7 in the paper.
mat_rmse_exog <- foreach(fcast_func=fcast_funcs[4:8], .combine='cbind') %:%
  foreach(endog=var_endog, xreg=ts_exog, .combine='c') %do%
  model_eval(fcast_func, endog, res_to_rmse, xreg)

# Add the Mean and Percentage deviation rows to the matrix.
mat_rmse_exog <- rbind(mat_rmse_exog,
                       colMeans(mat_rmse_exog),                                # Calculation of the Mean row.
                       colMeans(mat_rmse_exog / apply(mat_rmse_exog, 1, min))) # Calculation of the Percentage deviation row.
rownames(mat_rmse_exog) <- c(var_endog, 'Mean', 'Percentage deviation')
colnames(mat_rmse_exog) <- fcast_funcs_names[4:8]


# Matrix of sMAPE values with incorporating external factors.
# See Table 8 in the paper.
mat_smape_exog <- foreach(fcast_func=fcast_funcs[4:8], .combine='cbind') %:%
  foreach(endog=var_endog, xreg=ts_exog, .combine='c') %do%
  model_eval(fcast_func, endog, res_to_smape, xreg)

# Add the Mean and Percentage deviation rows to matrix.
mat_smape_exog <- rbind(mat_smape_exog,
                        colMeans(mat_smape_exog),                                 # Calculation of the Mean row.
                        colMeans(mat_smape_exog / apply(mat_smape_exog, 1, min))) # Calculation of the Percentage deviation row.
rownames(mat_smape_exog) <- c(var_endog, 'Mean', 'Percentage deviation')
colnames(mat_smape_exog) <- fcast_funcs_names[4:8]


# Write the matrices to csv-files.
write.csv(mat_rmse, 'rmse_endog.csv')
write.csv(mat_smape, 'smape_endog.csv')
write.csv(mat_rmse_exog, 'rmse_exog.csv')
write.csv(mat_smape_exog, 'smape_exog.csv')