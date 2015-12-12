timeseries = read.csv("data/data_minimal.csv")
attach(timeseries)

mean_sd <- function(d, precision=1) {
 paste(format(round(mean(d, na.rm=T), precision), nsmall=precision), " (", format(round(sd(d, na.rm=T), precision), nsmall=precision), ")", sep = "")
}


participants <- function(experiment_filter, condition_filter=unique(timeseries$condition)) {
 d = subset(timeseries$username, experiment==experiment_filter & condition %in% condition_filter)

 length(unique(d))
}
participants("E1", 3)

mos_qu_with_sd_by_condition <- function(experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id)) {
 d = subset(timeseries$QU, experiment==experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & id %in% id_filter)
 
 mean_sd(d)
}
mos_qu_with_sd_by_condition("E1", "HP", 3)
mos_qu_with_sd_by_condition("E1", "HP", c(1:3), 1)

mos_iqu_with_sd_by_condition <- function(experiment_filter, condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service)) {
 d = subset(timeseries$IQU, experiment==experiment_filter & condition %in% condition_filter & id %in% id_filter & service %in% service_filter)
 
 mean_sd(d)
}
mos_iqu_with_sd_by_condition("E1", 4, 3)
mos_qu_with_sd_by_condition("E2a", "LP", "2b", 5)

mos_iqu_with_sd_by_condition("E2b", "5", 6, "telephony")

mos_iqu_with_sd_by_condition("E2b", "0", 6, "video")
mos_iqu_with_sd_by_condition("E2b", "4", 6, "video")
