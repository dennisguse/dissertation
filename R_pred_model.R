########Models using QU to predict IQU
#timeseries: [id, QU, IQU]
#id must be unique (must not be ordered!)
#username, schedule or anything else are not aggregated for all functions!

#Static
model_static_create <- function(IQU) {
 return (function(id, timeseries) {
  model_static(id, timeseries, IQU)
 })
}
model_static <- function(id, timeseries, IQU) {
 return (IQU)
}

#Identity
model_identity_create <- function() {
 return (function(id, timeseries) {
  model_identity(id, timeseries)
 })
}
model_identity <- function(id, timeseries) {
 return (timeseries$IQU[id])
}



#baseline and windowed mean
model_average_weighted_create <- function(fun_weight) {
 return (function(id, timeseries) {
  return(model_average_weighted(id, timeseries, fun_weight))
 })
}
model_average_weighted <- function(id, timeseries, fun_weight) {
 historicTimeseries = timeseries[(timeseries$id <= id),]
 historicTimeseries = fun_weight(historicTimeseries)
 
 result = weighted.mean(x = historicTimeseries$QU, w = historicTimeseries$weight, na.rm = TRUE)
 
 return (result)
}

#weight window
weight_window_create <- function(window) {
 return (function(timeseries) {
  return(weight_window(timeseries, window))
 })
}
weight_window <- function(timeseries, window) {
 timeseries$weight = 0
 
 timeseries$weight[timeseries$id - max(timeseries$id) > -(window)] = 1
 
 return (timeseries)
}

weight_linear_create <- function(window) {
 return (function(timeseries) {
  return(weight_linear(timeseries, 2*window))
 })
}
weight_linear <- function(timeseries, window) {
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
 
 weight_window_3 = weight_window_create(10)
 weight_window_3(timeseries)
 
 weight_linear_3 = weight_linear_create(1)
 weight_linear_3(timeseries)
}