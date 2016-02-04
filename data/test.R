timeseries = read.csv("data_minimal.csv")
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
