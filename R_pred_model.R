#id must be unique
#timeseries: [id, QU, IQU]
#username, schedule or anything else are not aggregated for all functions!

########Models using QU to predict IQU
#--mean IQU
dg_model_id_create <- function(IQU) {
  return (function(id, timeseries) {dg_model_id(id, timeseries, IQU)})
}
dg_model_id <- function(id, timeseries, IQU) {
  return (IQU)
}


#--mean
dg_model_mean_create <- function() {
  return (dg_model_mean)
}
dg_model_mean <- function(id, timeseries) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  return (mean(historictimeseries$QU, na.rm=TRUE))
}

#--mean + offset
dg_model_mean_offset_create <- function(offset) {
  return (function(id, timeseries) {dg_model_mean_offset(id, timeseries, offset)})
}
dg_model_mean_offset <- function(id, timeseries, offset) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  return (mean(historictimeseries$QU, na.rm=TRUE) + offset)
}


#--mean over prior n
dg_model_mean_window_create <- function(window) {
  return (function(id, timeseries) { return(dg_model_mean_window(id, timeseries, window)) })
}
dg_model_mean_window <- function(id, timeseries, window) {
  if (id-window < 0) { #Only do the calculation, if their is enough data for the model.
    print("Window greater than id!")
    return (NA)
  }
  
  historictimeseries = timeseries[(timeseries$id <= id) & timeseries$id >= (id - window + 1), ]
  
  return (mean(historictimeseries$QU, na.rm=TRUE))
}

#--weighted mean using linear forgetting
dg_model_mean_linear_create <- function(window) {
  return (function(id, timeseries) { return(dg_model_mean_linear(id, timeseries, window)) })
}
dg_model_mean_linear <- function(id, timeseries, window) {
  historicTimeseries = timeseries[timeseries$id <= id, ]
  
  historicTimeseries$relative_id = (historicTimeseries$id - id + window)# use relative IDs for Treppenfunktion
  
  historicTimeseries$weight = dg_models_linearWeight(window, historicTimeseries$relative_id) #Sampling points for Treppenfunktion
  result = weighted.mean(x=historicTimeseries$QU, w=historicTimeseries$weight, na.rm=TRUE)
  
#   print("-------DEBUG")
#   print(historicTimeseries)
#   print(result)
  
  return (result)
}
dg_models_linearWeight <- function(window, n) {
  y = n/window #(n-1)? also 0 index?
  y[y < 0] = 0
  y[y > 1] = 0
  return (y)
}

#--mean - alpha * min
dg_model_mean_minus_peak_create <- function(alpha) {
  return (function(id, timeseries) {dg_model_mean_minus_peak(id, timeseries, alpha)})
}
dg_model_mean_minus_peak <- function(id, timeseries, alpha) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  average = mean(historictimeseries$QU, na.rm=TRUE)
  minimal = min(historictimeseries$QU, na.rm=TRUE)
  
  result = (average - alpha * minimal)
  return (result)
}
#--mean + alpha * abs(mean - min))
dg_model_mean_plus_peak_diff_create <- function(alpha) {
  return (function(id, timeseries) {dg_model_mean_plus_peak_diff(id, timeseries, alpha)})
}
dg_model_mean_plus_peak_diff <- function(id, timeseries, alpha) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  average = mean(historictimeseries$QU, na.rm=TRUE)
  minimal = min(historictimeseries$QU, na.rm=TRUE)
  
  result = (average + alpha * abs(average - minimal))
  return (result)
}
#--(mean - alpha * abs(mean - min)) / (1 + alpha) !ATTENTION: Factor used for normalisation is stupid!
dg_model_mean_minus_peak_diff_norm_create <- function(alpha) {
  return (function(id, timeseries) {dg_model_mean_minus_peak_diff_norm(id, timeseries, alpha)})
}
dg_model_mean_minus_peak_diff_norm <- function(id, timeseries, alpha) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  average = mean(historictimeseries$QU, na.rm=TRUE)
  minimal = min(historictimeseries$QU, na.rm=TRUE)
  
  result = (average - alpha * abs(average - minimal)) / (1+alpha)
  return (result)
}
#--(mean + alpha * abs(mean - min)) / (1 + alpha)
dg_model_mean_plus_peak_diff_norm_create <- function(alpha) {
  return (function(id, timeseries) {dg_model_mean_plus_peak_diff_norm(id, timeseries, alpha)})
}
dg_model_mean_plus_peak_diff_norm <- function(id, timeseries, alpha) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  average = mean(historictimeseries$QU, na.rm=TRUE)
  minimal = min(historictimeseries$QU, na.rm=TRUE)
  
  result = (average + alpha * abs(average - minimal)) / (1+alpha)
  return (result)
}

#--weighted mean using Treppenfunktion (from id reverse)
dg_model_weighted_mean_create <- function(x1, x2, y1, y2) {
  return (function(id, timeseries) {dg_model_weighted_mean(id, timeseries, x1, x2, y1, y2)})
}
dg_model_weighted_mean <- function(id, timeseries, x1, x2, y1, y2) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  
  historictimeseries$relative_id = id-historictimeseries$id # use relative IDs for Treppenfunktion
    
  historictimeseries$weight = dg_models_treppenfunktion(x1, x2, y1, y2, historictimeseries$relative_id) #Sampling points for Treppenfunktion
  result = weighted.mean(x=historictimeseries$QU, w=historictimeseries$weight, na.rm=TRUE)

#  print("-------DEBUG")
#  print(historictimeseries)
#  print(result)
  
#  result
  return (result)
}

dg_model_mean_window_minus_peak_create <- function(window, alpha) {
  return (function(id, timeseries) { return(dg_model_mean_window_minus_peak(id, timeseries, window, alpha)) })
}
dg_model_mean_window_minus_peak <- function(id, timeseries, window, alpha) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  minimal = min(historictimeseries$QU, na.rm=TRUE)
  
  result = (dg_model_mean_window(id, timeseries, window) - alpha * minimal)
  
  return (result)
}


#Moving average things
###TODO Not implemented.
library("TTR")
dg_model_moving_average_exponential_create <- function(ratio) { #ratio is alpha
  return (function(id, timeseries) {dg_model_moving_average_exponential(id, timeseries, ratio)})
}
dg_model_moving_average_exponential <- function(id, timeseries, ratio) {
  historictimeseries = timeseries[timeseries$id <= id, ]
  
  print(historictimeseries)
  
  EMA(historictimeseries$QU, id, ratio=ratio, )
  
  historictimeseries$relative_id = id-historictimeseries$id # use relative IDs for Treppenfunktion: might be broken; this should work (historicTimeseries$id-id+n)
  
  historictimeseries$weight = dg_models_treppenfunktion(x1, x2, y1, y2, historictimeseries$relative_id) #Sampling points for Treppenfunktion
  result = weighted.mean(x=historictimeseries$QU, w=historictimeseries$weight, na.rm=TRUE)
  
  return (result)
}
dg_model_ex_mov_avg <- function(id, timeseries, ratio) {
  current = timeseries[timeseries$id == id, ]
  print(id)
  print(current)
  currentValue = 0
  if (is.nan(current$QU)) {
    currentValue = 0;
  }
  else {
    currentValue = current$QU * ratio;
  }
  
  next_timeseries = timeseries[-which(timeseries$id %nin% id), ]
  print(next_timeseries)
  if (length(next_timeseries) == 0) { 
    return (currentValue);
  } else {
    return (currentValue + dg_model_ex_mov_avg(id+0.5, next_timeseries, ratio))
  }
}


#########Using last IQU values!
#Only works for Study 3! Not for any other! (DayId is different?)
dg_model_last_IQU_create <- function(alpha=1) {
  return ((function(id, timeseries) {dg_model_last_IQU(id, timeseries, alpha)}))
}
dg_model_last_IQU <- function(id, timeseries, alpha=1) {
  if (id <= 2) return (NA)
  
  IQU_prior = timeseries[timeseries$id == (id - 3), ]$IQU
  QU_prior = timeseries[timeseries$id > (id - 3) & timeseries$id <= id, ]$QU
  
  IQU_unweighted = alpha * IQU_prior + mean(QU_prior, na.action="omit")
  IQU_estimation = IQU_unweighted / (1 + alpha)
  return (IQU_estimation)
}








#Treppefunktion f(x) %x can be a vector!
#****  y1
#    *
#     *
#      ******** y_2
#   x1 x2 
# Note x can be a vector!

dg_models_treppenfunktion <- function(x1, x2, y1, y2, x) {
  y = 0
  
  y[x < x1] = y1;
  y[x > x2] = y2;
  
  i = x1 <= x & x <= x2;
  y[i] = y2 + (y2 - y1) / (x2 - x1) * (x[i] - x2);
  
  return (y)
}

# if (FALSE) {
#   ##Test for the weighted mean function!
#   test_trepfun = dg_models_treppenfunktion(1, 8, 1, 0.7, seq(0, 14))
#   #test_trepfun = dg_models_treppenfunktion(1, 7, 0.8, 0.3, seq(0, 14))
#   print(test_trepfun)
#   
#   #test_trepfun_data = c(7, 6, 6.6, 7, 4, 6, 7, 4, 3, 2)
#   #weighted_mean = weighted.mean(x=test_trepfun_data, w=test_trepfun)
#   
#   #sum(test_trepfun*test_trepfun_data)/sum(test_trepfun)
#   
#   
#   print(sum(test_trepfun))
#   plot(test_trepfun, ylim=c(0,1), xlim=c(0, 14))
#   grid()
# }





id = 1:10
QU = c(5, 6, 4, 6, 2, 1, 3, 4, 5, 2)
IQU = rep(NA, 10)
IQU[10] = 5
IQU[5] = 3
IQU[2] = 3

timeseries = data.frame(id, QU, IQU)

dg_model_mean_linear(10, timeseries, 1) #2
dg_model_mean_linear(10, timeseries, 2) #3


dg_model_mean_window(2, timeseries, 1)
dg_model_mean_window(10, timeseries, 10) #3.8

dg_model_id(5, timeseries, 3) #3 :D
