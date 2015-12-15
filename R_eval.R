attach(timeseries)
mean_sd <- function(d, precision=1) {
 paste0(format(round(mean(d, na.rm=T), precision), nsmall=precision), " (", format(round(sd(d, na.rm=T), precision), nsmall=precision), ")")
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
mos_qu_with_sd_by_condition("E1", "LP", "2b", 5)

mos_iqu_with_sd_by_condition("E2b", "5a0", 6, "telephony")
mos_iqu_with_sd_by_condition("E2b", "5a4", 6, "telephony")

mos_iqu_with_sd_by_condition("E2b", "0", 6, "video")
mos_iqu_with_sd_by_condition("E2b", "4", 6, "video")


#####STATISTICS!

kruskal <- function(formula=QU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id)) {
 d = subset(timeseries, experiment==experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & id %in% id_filter)

 r = kruskal.test(formula, d)
 if (r$p.value < 0.0001) paste0("$H(", r$parameter, ")=", round(r$statistic, 4), "$, $p<0.0001$") 
 else paste0("$H(", r$parameter, ")=", round(r$statistic, 4), "$, $p=", round(r$p.value, 4), "$") 
}
kruskal(QU~condition, "E1", "HP", unique(timeseries$condition), c(1:3))

wilcox <- function(formula=QU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), alternative="two.sided", paired=F) {
 d = subset(timeseries, experiment==experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & id %in% id_filter)

 r = wilcox.test(formula, d, exact = F, conf.int = T, alternative = alternative, paired = paired)
 if (r$alternative == "two.sided") r$alternative = ""
 else r$alternative = ", one-sided"
  
 if (r$p.value < 0.0001) paste0("$W=", round(r$statistic, 4), "$, $p<0.0001$, $\triangle=", format(round(r$estimate, 2), nsmall=2), "$", r$alternative) 
 else paste0("$W=", round(r$statistic, 4), "$, $p=", round(r$p.value, 4), "$, $\triangle=", format(round(r$estimate, 2), nsmall=2), "$", r$alternative) 
}
wilcox(QU~performance, "E1",  c("HP", "LP"))
wilcox(QU~performance, "E1",  c("HP", "LP"), alternative="greater")
wilcox(QU~performance, "E1",  c("HP", "LP"), alternative="less")

pairwise.wilcox.test(timeseries_by_study_H1_TEL_ONE$IQU, timeseries_by_study_H1_TEL_ONE$condition, exact=F, alternative = "less") 

wilcox.pairwise <- function(formula=IQU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id)) { #, alternative="two.sided", paired=F, p.adjust.method = "holm"
 d = subset(timeseries, experiment==experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & id %in% id_filter)
 
 f = all.names(formula, functions=F)
 r = pairwise.wilcox.test(d[[f[1]]], d[[f[2]]], exact=F) 
 r$p.value = format(round(r$p.value, 2), nsmall=2)
 r$p.value[r$p.value == "  NA"] = "-"

 print(sprintf("%s\\ \n", paste(colnames(r$p.value), collapse="&")))
 value=apply(r$p.value, 1, function(x){ sprintf("%s\\ \n", paste(x, collapse="&"))})
 for(row in 1:length(value)) {
  print(paste(names(value)[row], value[row], sep="&"))
 }
 
 return (r)
}
r=wilcox.pairwise(experiment_filter = "E2a")




