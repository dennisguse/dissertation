timeseries = read.csv("data_minimal.csv")
attach(timeseries)


#timeseries=timeseries[-which(timeseries$experiment == "E1" & timeseries$condition == "5a")]
#timeseries=timeseries[-which(timeseries$experiment == "E6a" & timeseries$id < 3)]

evaluate <- function(line) {
 weight_fun = do.call(line["model"], list(as.numeric(line["parameter"])))
 model = model_average_weighted_create(weight_fun)
 data = subset(timeseries, experiment %in% line["experiment"] & condition %in% line["condition"])
 eval_performance(model, data, as.numeric(line["id"]))
}

if (FALSE) {
 a=apply(toPredict, 1, evaluate)
 c=cbind(toPredict, t(a))
 
 d=subset(c, c$experiment == "E2a")
 colnames(d) <- c("experiment", "condition", "id", "parameter", "model", "RMSD", "cor")
 p <- ggplot(d, aes(parameter, y=RMSD, colour=condition)) + geom_line()
p }


if (FALSE) {
 #ONE SESSION
 to_model_experiments_lab = unique(subset(timeseries, experiment %in% c("E1", "E2a", "E2b", "E3"))[, c("experiment", "condition")], MARGIN=2)
 to_model_experiments_lab_tmp = expand.grid(experiment=to_model_experiments_lab$experiment, id=c(3, 6, 9))
 to_model_experiments_lab = unique(merge(to_model_experiments_lab, to_model_experiments_lab_tmp), MARGIN=2)
 to_model_experiments_lab = subset(to_model_experiments_lab, !(to_model_experiments_lab$experiment == "E1" & to_model_experiments_lab$id == 9))
 
 #FIELD
 to_model_experiments_field= unique(subset(timeseries, experiment %in% c("E6a"))[, c("experiment", "condition")], MARGIN=2)
 to_model_experiments_field_tmp = expand.grid(experiment=to_model_experiments_field$experiment, id=c(8, 14))
 to_model_experiments_field = unique(merge(to_model_experiments_field, to_model_experiments_field_tmp), MARGIN=2)
 
 to_model = rbind(to_model_experiments_lab, to_model_experiments_field)
 
 toPredict = data.frame()
 for(i in 1:nrow(to_model)) {
  for(parameter in 1:to_model[1, ]$id) {
   current = cbind(to_model[i, ], parameter)
   toPredict = rbind(toPredict, current)
  }
 }
 toPredict$model = "weight_window_create"
}