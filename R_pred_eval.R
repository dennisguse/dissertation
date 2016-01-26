library("Hmisc")

#Calculates the performance of a model_impl for the given dataSet: correlation and RMSD
#The dataSet is splitted by schedule and username (if more than one)
#To the given dataSet the IQU_approx and residuals are added
dg_model_performance <- function(model, timeseries, id=NULL) {
  #0. Prepare data: split
  if (!is.null(id)) timeseries$IQU[which(timeseries$id %nin% id)] = NA

  timeseries$IQU_approx = NA  
  ## For each STUDY AND and SCHEDULE and SERVICE and USERNAME
  dataSetSplit=split(timeseries, timeseries[, c("study", "service", "schedule", "username")], drop=TRUE)
  
  for(username in names(dataSetSplit)) {    
    #1. Calculate estimation for each existing IQU
    for(IQUidx in which(!is.na(dataSetSplit[[username]]$IQU))) {
      currentId = dataSetSplit[[username]]$id[IQUidx]
      dataSetSplit[[username]]$IQU_approx[IQUidx] = model(currentId, dataSetSplit[[username]])
    }
  }
  timeseries=do.call("rbind", dataSetSplit) #Unsplitting
  
  #1. Calculate correlation
  correlation = cor(timeseries$IQU, timeseries$IQU_approx, use="na.or.complete")
  #correlation = cor(timeseries$IQU, timeseries$IQU_approx, use="pairwise.complete.obs", method="pearson")

  #2. Mean root square // Euclidian distance // Minkowski distance are possible here.
  timeseries$residuals = timeseries$IQU_approx - timeseries$IQU   #1. Calculate residuals
  
  RMSD = (mean(timeseries$residuals^2, na.rm=TRUE))^(1/2)
  RMSD = sqrt(sum(timeseries$residuals^2, na.rm=T)/sum(!is.na(timeseries$residuals)))

  return (list(dataSet=timeseries, model=model, RMSD=RMSD, correlation=correlation, IQU=timeseries$IQU, IQU_approx=timeseries$IQU_approx))
}
dg_model_performance_print <- function(model, model_name, timeseries, id=NULL) {
  result = dg_model_performance(model, timeseries, id)
  print(paste(model_name, "parameters ", id, result$RMSD, result$correlation, result$IQU_approx, sep=" ; "))
  #return (result)
  return (data.frame(model=model_name, id=id, RMSD=result$RMSD, correlation=result$correlation))
#  return (list(model=model_name, id=id, RMSD=result$RMSD, correlation=result$correlation, IQU=result$IQU, IQU_approx=result$IQU_approx))
#  return (rbind(result, cbind(correlation, RMSD)))
}


id = 1:10
QU = c(5, 6, 4, 6, 2, 1, 3, 4, 5, 2)
IQU = rep(NA, 10)
IQU[10] = 5
IQU[5] = 3

username=rep(1, 10)
schedule=rep(1, 10)
study=rep(1, 10)
service=rep(1, 10)

timeseries = data.frame(id, QU, IQU, username, schedule, study, service)

dg_model_mean_linear(10, timeseries, 1)
dg_model_mean_linear(10, timeseries, 2)
dg_model_mean_linear(10, timeseries, 3)