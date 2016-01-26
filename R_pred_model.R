########Models using QU to predict IQU
#timeseries: [id, QU, IQU]
#id must be unique (must not be ordered!)
#username, schedule or anything else are not aggregated for all functions!

#Static
dg_model_static_create <- function(IQU) {
 return (function(id, timeseries) {
  dg_model_static_create(id, timeseries, IQU)
 })
}
dg_model_static_create <- function(id, timeseries, IQU) {
 return (IQU)
}

#Identity
dg_model_identity_create <- function() {
 return (function(id, timeseries) {
  dg_model_static_create()
 })
}
dg_model_static_create <- function(id, timeseries, IQU) {
 return (timeseries$IQU[nrow(timeseries)])
}



#baseline and windowed mean
dg_model_average_weighted_create <- function(fun_weight) {
 return (function(id, timeseries) {
  return(dg_model_average_weighted(id, timeseries, fun_weight))
 })
}

dg_model_average_weighted <- function(id, timeseries, fun_weight) {
 historictimeseries = timeseries[(timeseries$id <= id),]
 historictimeseries$weight = fun_weight(historictimeseries)
 
 result = weighted.mean(x = historicTimeseries$QU, w = historictimeseries$weight, na.rm = TRUE)
 
 return (result)
}

#weight window
dg_weight_window_create <- function(window) {
 return (function(timeseries) {
  return(dg_weight_window(timeseries, window))
 })
}
dg_weight_window <- function(timeseries, window) {
 timeseries$weight = 0
 
 timeseries$weight[timeseries$id - length(timeseries$id) > -(window)] = 1
 
 return (timeseries)
}

dg_weight_linear_create <- function(window) {
 return (function(timeseries) {
  return(dg_weight_linear(timeseries, window))
 })
}
dg_weight_linear <- function(timeseries, window) {
 timeseries$relative_id = timeseries$id - length(timeseries$id) + window
 timeseries$weight[timeseries$relative_id > 0] =  timeseries$relative_id[timeseries$relative_id > 0] / window
 timeseries$weight[timeseries$relative_id <= 0] = 0
 
 return (timeseries)
}

#TEST
if (FALSE) {
 id = 1:10
 QU = c(5, 6, 4, 6, 2, 1, 3, 4, 5, 2)
 IQU = rep(NA, 10)
 IQU[10] = 5
 IQU[5] = 3
 IQU[2] = 3
 
 timeseries = data.frame(id, QU, IQU)
 
 weight_window_3 = dg_weight_window_create(3)
 weight_window_3(timeseries)
 
 weight_linear_3 = dg_weight_linear_create(3)
 weight_linear_3(timeseries)
 
 
 dg_model_mean_window(2, timeseries, 1)
 dg_model_mean_window(10, timeseries, 10) #3.8
 
 dg_model_id(5, timeseries, 3) #3 :D
}