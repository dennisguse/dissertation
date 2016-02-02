#timeseries=timeseries[-which(timeseries$experiment == "E1" & timeseries$condition == "5a")]
#timeseries=timeseries[-which(timeseries$experiment == "E6a" & timeseries$id < 3)]


# exp = "E1"
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(3)), 3, exp)
# 
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, "1")
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, "2a")
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, 3)
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, 4)
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, "5b")
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, "6")
# performance_by_experiment_and_condition(model_average_weighted_create(weight_window_create(6)), 6, exp, "7")
# 
# 
# performance()
# t(apply(timeseries, 1,  function(x) {return (c(x[["id"]], x[["QU"]]))}))


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
