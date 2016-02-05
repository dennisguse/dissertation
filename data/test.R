timeseries = read.csv("data_minimal.csv")

timeseries=timeseries[-which(timeseries$experiment == "E1" & timeseries$condition == "5a"), ]
timeseries=timeseries[-which(timeseries$experiment == "E6a" & timeseries$id < 3), ]
timeseries=timeseries[-which(timeseries$experiment == "E2b" & timeseries$service %in% c("VoD", "bundle")), ]

timeseries = eval_prepare(timeseries)
attach(timeseries)


evaluate <- function(line) {
 weight_fun = do.call(line["model"], list(as.numeric(line["parameter"])))
 model = model_average_weighted_create(weight_fun)
 data = subset(timeseries, experiment %in% line["experiment"] & condition %in% line["condition"])
 eval_performance(model, data, as.numeric(line["id"]))
}

if (FALSE) {
 #ONE SESSION
 to_model_experiments_lab = unique(subset(timeseries, experiment %in% c("E1", "E2a", "E2b", "E3"))[, c("experiment", "condition")], MARGIN=2)
 to_model_experiments_lab_tmp = expand.grid(experiment=to_model_experiments_lab$experiment, id=c(3, 6, 9))
 to_model_experiments_lab = unique(merge(to_model_experiments_lab, to_model_experiments_lab_tmp), MARGIN=2)
 to_model_experiments_lab = subset(to_model_experiments_lab, (to_model_experiments_lab$condition %in% c("5b", "7") & to_model_experiments_lab$id == 9) | (to_model_experiments_lab$condition  %nin% c("5b", "7") & to_model_experiments_lab$id != 9)) #Only exists for C5b and C7
 
 #FIELD
 to_model_experiments_field= unique(subset(timeseries, experiment %in% c("E6a"))[, c("experiment", "condition")], MARGIN=2)
 to_model_experiments_field_tmp = expand.grid(experiment=to_model_experiments_field$experiment, id=c(8, 14))
 to_model_experiments_field = unique(merge(to_model_experiments_field, to_model_experiments_field_tmp), MARGIN=2)
 
 to_model = rbind(to_model_experiments_lab, to_model_experiments_field)
 
 #WEIGHT WINDOW
 to_model_parameter = data.frame()
 for(i in 1:nrow(to_model)) {
  for(parameter in 1:to_model[i, ]$id) {
   current = cbind(to_model[i, ], parameter, model="weight_window_create")
   to_model_parameter = rbind(to_model_parameter, current)
  }
 }
 #WEIGHT LINEAR
 for(i in 1:nrow(to_model)) {
  for(parameter in seq(1, to_model[i, ]$id*2, 2)) {
   current = cbind(to_model[i, ], parameter, model="weight_linear_create")
   to_model_parameter = rbind(to_model_parameter, current)
  }
 }
 
 
 #COMPUTE
 ptm <- proc.time()
 c=cbind(to_model_parameter, t(apply(to_model_parameter, 1, evaluate)))
 proc.time() - ptm
}

#PLOTTING
if (FALSE) {
 d=subset(c, c$experiment == "E2a")
 colnames(d) <- c("experiment", "condition", "id", "parameter", "model", "RMSD", "cor")
 p <- ggplot(d, aes(parameter, y=RMSD, colour=condition)) + geom_line()
 p }