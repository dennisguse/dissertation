suppressMessages(library("Hmisc")) # %in%
timeseries = read.csv("data_minimal.csv")
attach(timeseries)

#Add condition 6_adjusted: here, the 4th or 9th, and 10th episodic judgment is replaced by the average of all prior judgments.
timeseries_c6 = subset(timeseries, condition == 6 & ((experiment %in% c("E1", "E2a") &  id < 4) | (experiment == "E6a" & id %in% 3:8)))
timeseries_c6_estimate=aggregate(timeseries_c6[c("QU")], by=list(experiment=timeseries_c6$experiment, username=timeseries_c6$username), FUN=function(x) {return (round(mean(x, na.rm=T), 1))})

timeseries_c6_estimate$id = 4 #E1, E2a
timeseries_c6_estimate$id[timeseries_c6_estimate$experiment == "E6a"] = 8
timeseries_c6_estimate_e2a = timeseries_c6_estimate[timeseries_c6_estimate$experiment == "E6a", ]
timeseries_c6_estimate_e2a$id = 9
timeseries_c6_estimate=rbind(timeseries_c6_estimate, timeseries_c6_estimate_e2a)

timeseries_c6 = subset(timeseries, condition == 6 & experiment %in% c("E1", "E2a", "E6a"))
timeseries_c6 = merge(timeseries_c6, timeseries_c6_estimate, by=c("experiment", "username", "id"), all = T, suffixes = c("", ".y"))
timeseries_c6$QU[!is.na(timeseries_c6$QU.y)] = timeseries_c6$QU.y[!is.na(timeseries_c6$QU.y)]
timeseries_c6$QU.y = NULL
timeseries_c6$condition = "6_adjusted"

timeseries_org = timeseries
timeseries = rbind(timeseries, timeseries_c6)
attach(timeseries)





#Add average per condition as new condition
timeseries_avg = aggregate(timeseries[c("QU", "IQU", "NPS")], by=list(experiment=experiment, duration=duration, service=service, condition=condition, id=id, performance=performance, performance_level=performance_level), FUN=function(x) {return (mean(x, na.rm=T))})
timeseries_avg$username = "average"
timeseries_avg$condition = paste(timeseries_avg$condition, "average", sep = "_")

timeseries = rbind(timeseries, timeseries_avg)
attach(timeseries)



#timeseries=timeseries[-which(timeseries$experiment == "E1" & timeseries$condition == "5a")]
#timeseries=timeseries[-which(timeseries$experiment == "E6a" & timeseries$id < 3)]


overview=read.csv("../R_modeling.csv", stringsAsFactors = F)
overview$id = as.numeric(overview$id)
toPredict = data.frame()
for(i in 1:nrow(overview)) {
 for(parameter in 1:overview[1, ]$id) {
  current = cbind(overview[i, ], parameter)
  toPredict = rbind(toPredict, current)
 }
}
toPredict$model = "weight_window_create"

evaluate <- function(line) {
 weight_fun = do.call(line["model"], list(as.numeric(line["parameter"])))
 model = model_average_weighted_create(weight_fun)
 data = subset(timeseries, experiment %in% line["experiment"] & condition %in% line["condition"])
 performance(model, data, as.numeric(line["id"]))
}

if (FALSE) {
 a=apply(toPredict, 1, evaluate)
 c=cbind(toPredict, t(a))
 
 d=subset(c, c$experiment == "E2a")
 colnames(d) <- c("experiment", "condition", "id", "parameter", "model", "RMSD", "cor")
 p <- ggplot(d, aes(parameter, y=RMSD, colour=condition)) + geom_line()
p }
