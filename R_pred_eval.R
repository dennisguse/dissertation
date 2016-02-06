suppressMessages(library("Hmisc")) # %in%

eval_performance <- function(model, timeseries, id=NULL) {
 #Performance for all?
 if (!is.null(id)) timeseries$IQU[which(timeseries$id %nin% id)] = NA
 if (nrow(timeseries) == 0) return (c(NA, NA))
 
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
 
 #return (list(timeseries=timeseries, model=model, RMSD=RMSD, correlation=correlation))
 return (c(RMSD, correlation))
}


#1. Computes adjusted condition 6: id=4/9,10 are replaced by average of all previous
#2. MOS per experiment and condition; are added as new conditions: X_averaged
eval_prepare <- function(timeseries) {
 #Add condition 6_adjusted: here, the 4th or 9th, and 10th episodic judgment is replaced by the average of all prior judgments.
 timeseries_c6 = subset(timeseries, condition == 6 & ((timeseries$experiment %in% c("E1", "E2a") &  id < 4) | (timeseries$experiment == "E6a" & id %in% 3:8)))
 timeseries_c6_estimate=aggregate(timeseries_c6[c("QU")], by=list(experiment=timeseries_c6$experiment, username=timeseries_c6$username), FUN=function(x) {return (round(mean(x, na.rm=T), 1))})
 
 timeseries_c6_estimate$id = 4 #E1, E2a
 timeseries_c6_estimate$id[timeseries_c6_estimate$experiment == "E6a"] = 9
 timeseries_c6_estimate_e2a = timeseries_c6_estimate[timeseries_c6_estimate$experiment == "E6a", ]
 timeseries_c6_estimate_e2a$id = 10
 timeseries_c6_estimate=rbind(timeseries_c6_estimate, timeseries_c6_estimate_e2a)
 
 timeseries_c6 = subset(timeseries, condition == 6 & experiment %in% c("E1", "E2a", "E6a"))
 timeseries_c6 = merge(timeseries_c6, timeseries_c6_estimate, by=c("experiment", "username", "id"), all = T, suffixes = c("", ".y"))
 timeseries_c6$QU[!is.na(timeseries_c6$QU.y)] = timeseries_c6$QU.y[!is.na(timeseries_c6$QU.y)]
 timeseries_c6$QU.y = NULL
 timeseries_c6$condition = "6_adjusted"
 
 timeseries_org = timeseries
 timeseries = rbind(timeseries, timeseries_c6)

 #Add average per condition as new condition
 timeseries_avg = aggregate(timeseries[c("QU", "IQU", "NPS")], by=list(experiment=timeseries$experiment, duration=timeseries$duration, service=timeseries$service, condition=timeseries$condition, id=timeseries$id, performance_level=timeseries$performance_level), FUN=function(x) {return (mean(x, na.rm=T))})
 timeseries_avg$username = "average"
 timeseries_avg$condition = paste(timeseries_avg$condition, "average", sep = "_")
 
 timeseries_avg$performance = NA
 timeseries = rbind(timeseries, timeseries_avg)

 return (timeseries)
}