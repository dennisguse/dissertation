library("Hmisc") # %in%

performance <- function(model, timeseries, id=NULL) {
 #Performance for all?
 if (!is.null(id)) timeseries$IQU[which(timeseries$id %nin% id)] = NA

 timeseries$IQU_approx = NA  
 
 ##SPLIT
 timeseriesSplit = split(timeseries, timeseries[, c("experiment", "service", "condition", "username")], drop=TRUE)
 
 for(username in names(timeseriesSplit)) {    
  #PREDICT
  for(IQUidx in which(!is.na(timeseriesSplit[[username]]$IQU))) {
   currentId = timeseriesSplit[[username]]$id[IQUidx]
   timeseriesSplit[[username]]$IQU_approx[IQUidx] = model(currentId, timeseriesSplit[[username]])
  }
 }
 
 #UNSPLIT
 timeseries=do.call("rbind", timeseriesSplit)
 
 correlation = cor(timeseries$IQU, timeseries$IQU_approx, use="na.or.complete") #method="pearson"

 #2. Mean root square // Euclidian distance // Minkowski distance are possible here.
 timeseries$residuals = timeseries$IQU_approx - timeseries$IQU   #1. Calculate residuals
 RMSD = sqrt(sum(timeseries$residuals^2, na.rm=T)/sum(!is.na(timeseries$residuals)))
 
 return (list(timeseries=timeseries, model=model, RMSD=RMSD, correlation=correlation))
}


performance_by_experiment_and_condition <- function(model, id=NULL, experiment_filter=experiment, condition_filter=condition) {
 d = subset(timeseries, experiment %in% experiment_filter & condition %in% condition_filter)
 result = performance(model, d, id)
 sprintf("$RMSD=%f, Cor=%f$", result$RMSD, result$correlation)
}

performanceAVG_by_experiment_and_condition <- function(model, id=NULL, experiment_filter=timeseries_avg$experiment, condition_filter=timeseries_avg$condition) {
 d = subset(timeseries_avg, timeseries_avg$experiment %in% experiment_filter & timeseries_avg$condition %in% condition_filter)
 result = performance(model, d, id)
 sprintf("$RMSD=%f, Cor=%f$", result$RMSD, result$correlation)
}

if (FALSE) {
 id = 1:10
 QU = c(5, 6, 4, 6, 2, 1, 3, 4, 5, 2)
 IQU = rep(NA, 10)
 IQU[10] = 5
 IQU[5] = 3
 
 username=rep(1, 10)
 condition=rep(1, 10)
 experiment=rep(1, 10)
 service=rep(1, 10)
 
 timeseries = data.frame(id, QU, IQU, username, condition, study, service)
 
 model_identity(10, timeseries)
 model_static(10, timeseries, 4)
 
 performance(model_identity_create(), timeseries)
 
 performance(model_average_weighted_create(dg_weight_window_create(6)), timeseries)
 
 
 performance_by_experiment_and_condition(model_identity_create(), NULL, "E1")

 timeseries_avg = aggregate(timeseries[c("QU", "IQU")], by=timeseries[c("experiment", "service", "condition", "id")], FUN=mean, na.rm=TRUE)
 timeseries_avg$username = "statistics"
 performanceAVG_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1")
 performanceAVG_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 3)
 performanceAVG_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 6)
 

 
   
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 1)
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", "2a")
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 3)
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 4)
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", "5b")
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 6)
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E1", 7)
 
 
 
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), NULL, "E2a")
 
 performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 14, "E6a")
 
 
 
 performance_by_experiment_and_condition(model_identity_create(), NULL, "E1")
}