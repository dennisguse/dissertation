library("Hmisc") # %in%

dg_model_performance <- function(model, timeseries, id=NULL) {
 #Performance for all?
 if (!is.null(id)) timeseries$IQU[which(timeseries$id %nin% id)] = NA

 timeseries$IQU_approx = NA  
 
 ##SPLIT
 timeseriesSplit = split(timeseries, timeseries[, c("study", "service", "condition", "username")], drop=TRUE)
 
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

if (FALSE) {
 id = 1:10
 QU = c(5, 6, 4, 6, 2, 1, 3, 4, 5, 2)
 IQU = rep(NA, 10)
 IQU[10] = 5
 IQU[5] = 3
 
 username=rep(1, 10)
 condition=rep(1, 10)
 study=rep(1, 10)
 service=rep(1, 10)
 
 timeseries = data.frame(id, QU, IQU, username, condition, study, service)
 
 dg_model_identity(10, timeseries)
 dg_model_static(10, timeseries, 4)
 
 dg_model_performance(dg_model_identity_create(), timeseries)
 
 dg_model_performance(dg_model_average_weighted_create(dg_weight_window_create(1)), timeseries)
 
}