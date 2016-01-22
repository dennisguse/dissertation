attach(timeseries)
mean_sd <- function(d, precision=1) {
 paste0(format(round(mean(d, na.rm=T), precision), nsmall=precision), " (", format(round(sd(d, na.rm=T), precision), nsmall=precision), ")")
 #sprintf("%f (%f)", mean(d, na.rm=T), sd(d, na.rm=T))
}

#TODO Sprintf does not round, but rather cuts numbers!

participants <- function(experiment_filter, condition_filter=unique(timeseries$condition)) {
 d = subset(timeseries$username, experiment==experiment_filter & condition %in% condition_filter)

 length(unique(d))
}
participants("E1", 3)
participants("E2b", "5a4")

mos_qu_with_sd_by_condition <- function(experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service)) {
 d = subset(timeseries$QU, experiment %in% experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & id %in% id_filter & service %in% service_filter)
 
 mean_sd(d)
}
mos_qu_with_sd_by_condition("E1", "HP", c(1:3), 1)
mos_qu_with_sd_by_condition("E2b", "HP", c("5a"),  c(1:3), "telephony")
mos_qu_with_sd_by_condition("E2b", "HP", c("5a0"),  c(1:6), "VoD")
mos_qu_with_sd_by_condition("E2b", "LP", c("5a4"),  c(1:6), "VoD")

mos_iqu_with_sd_by_condition <- function(experiment_filter, condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service)) {
 d = subset(timeseries$IQU, experiment %in% experiment_filter & condition %in% condition_filter & id %in% id_filter & service %in% service_filter)
 
 mean_sd(d)
}
mos_iqu_with_sd_by_condition("E1", 4, 3)
mos_qu_with_sd_by_condition("E1", "LP", "2b", 5)

mos_iqu_with_sd_by_condition("E2b", "5a", 6, "telephony")
mos_iqu_with_sd_by_condition("E2b", "5a0", 6, "telephony")
mos_iqu_with_sd_by_condition("E2b", "5a4", 6, "telephony")

mos_iqu_with_sd_by_condition("E2b", "5a0", 6, "VoD")
mos_iqu_with_sd_by_condition("E2b", "5a4", 6, "VoD")


#####STATISTICS!
kruskal <- function(formula=QU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service), reference=F) {
 d = subset(timeseries, experiment %in% experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & service %in% service_filter)
 if (reference) {
  d$condition = as.character(d$condition)
  d[d$id == 3, ]$condition = -1
  d$condition = as.factor(d$condition)
  d$id[d$condition == -1 & d$id == "3"] = 6
 }
 d = subset(d, id %in% id_filter)
 
 r = kruskal.test(formula, d)
 if (r$p.value < 0.001) paste0("$H(", r$parameter, ")=", round(r$statistic, 4), "$, $p<0.001$") 
 else paste0("$H(", r$parameter, ")=", round(r$statistic, 4), "$, $p=", round(r$p.value, 4), "$") 
}

kruskal(QU~condition, "E1", "HP", unique(timeseries$condition), c(1:3))
kruskal(QU~condition, "E2b", "HP", c("5a0", "5a4"), c(1:3))

kruskal(IQU~condition, "E1", unique(timeseries[["performance_level"]]), c("3", "5b", "6"), c(6))
kruskal(IQU~condition, "E1", unique(timeseries[["performance_level"]]), c("3", "5b", "6"), c(6), reference=T)

wilcox <- function(formula=QU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service), alternative="two.sided", paired=F, diffOnly = F) {
 d = subset(timeseries, experiment %in% experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & service %in% service_filter & id %in% id_filter)
 
 r = wilcox.test(formula, d, exact = F, conf.int = T, alternative = alternative, paired = paired)
 if (diffOnly == T) {
  paste0("$\\triangle=", format(round(r$estimate, 2), nsmall=2), "$")
 } else {
  if (r$alternative == "two.sided") r$alternative = ""
  else r$alternative = ", one-sided"

  if (r$p.value > 0.001) sprintf("$W=%.2f$, $p=%.3f$%s", r$statistic, r$p.value, r$alternative)
  else sprintf("$W=%.2f$, $p<0.001$%s", r$statistic, r$alternative)
  }
}
wilcox(QU~performance, "E1",  c("HP", "LP"))
wilcox(QU~performance, "E1",  c("HP", "LP"), alternative="greater")
wilcox(QU~performance, "E1",  c("HP", "LP"), alternative="less")
wilcox(QU~condition, "E1", "HP", c("4", "7"), c(1:3), diffOnly=T)

wilcox(IQU~condition, "E1", "HP", c("4", "5b"), c(6))



wilcox.pairwise.print <- function(formula=IQU~condition, experiment_filter, performance_level_filter=unique(timeseries$performance_level), condition_filter=unique(timeseries$condition), id_filter=unique(timeseries$id), service_filter=unique(timeseries$service), alternative="two.sided", paired=F, reference=F) { #, alternative="two.sided", paired=F, p.adjust.method = "holm"
 d = subset(timeseries, experiment %in% experiment_filter & performance_level %in% performance_level_filter & condition %in% condition_filter & service %in% service_filter)
 if (reference) {
  d$condition = as.character(d$condition)
  d[d$id == 3, ]$condition = "000"
  d$condition = as.factor(d$condition)
  d$id[d$condition == "000" & d$id == "3"] = 6
 }
 d = subset(d, id %in% id_filter)
 
 f = all.names(formula, functions=F)
 p = pairwise.wilcox.test(d[[f[1]]], d[[f[2]]], exact=F, alternative = alternative, paired = paired)$p.value
 result = apply(p, c(1, 2), function(x) {
  if (!is.na(x) && x < 0.001) sprintf("$p<0.001$", x)
  else  sprintf("$p=%.3f$", x)
  })
 return (result)
# estimate = pairwise.wilcox.estimate(d[[f[1]]], d[[f[2]]], exact=F, alternative = alternative, paired = paired)$p.value
 
#  result = p
#  for(x in 1:nrow(p)) {
#   for(y in 1:ncol(p)) {
#    if (is.na(p[x, y])) result[x, y] = "NA"
#    else 
#    if (p[x, y] > 0.05) result[x, y] = sprintf("$p=%.3f$", p[x, y])
#    else if (p[x, y] < 0.001) result[x, y] = sprintf("$p<0.001$, $\\triangle=%.2f$", estimate[x, y])
#    else result[x, y] = sprintf("$p=%.3f$, $\\triangle=%.2f$", p[x, y], estimate[x, y])
#    }
#   }
#  return (result)
 #result = as.data.frame(outer(r, estimate, function(x, y) {return (paste(x, ": ", y))} ))
}

pairwise.wilcox.estimate <-function (x, g, p.adjust.method = p.adjust.methods, paired = FALSE, ...) {
 p.adjust.method <- match.arg(p.adjust.method)
 DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
 g <- factor(g)
 METHOD <- if (paired) 
  "Wilcoxon signed rank test"
 else "Wilcoxon rank sum test"
 compare.levels <- function(i, j) {
  xi <- x[as.integer(g) == i]
  xj <- x[as.integer(g) == j]
  wilcox.test(xi, xj, paired = paired, conf.int = T,...)$estimate #This is the original implementation of pairwise.wilcox.tet, but instead $p.value here $estimate is used!
 }
 PVAL <- pairwise.table(compare.levels, levels(g), "none")
 ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
             p.adjust.method = p.adjust.method)
 class(ans) <- "pairwise.htest"
 ans
}
wilcox.pairwise.print(QU~condition, "E1", "HP", unique(timeseries[["condition"]]))
wilcox.pairwise.print(QU~condition, "E1", "HP", unique(timeseries[["condition"]]), c(1:3))
wilcox.pairwise.print(QU~condition, "E1", "HP", unique(timeseries[["condition"]]), c(1:3))[7, 4]
#pairwise.wilcox.estimate(d[[f[1]]], d[[f[2]]], exact=F, alternative = "two.sided", paired = F)

wilcox.pairwise.print(IQU~condition, "E1", unique(timeseries[["performance_level"]]), c("3", "5b", "6"), c(6))
wilcox.pairwise.print(IQU~condition, "E1", unique(timeseries[["performance_level"]]), c("3", "5b", "6"), c(6), reference=T)
