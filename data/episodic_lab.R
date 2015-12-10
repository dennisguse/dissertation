timeseries = read.csv("data_minimal.csv")


stat_qu_by = function(t, formula=QU~condition) {
 ggplot(t, aes(QU, fill=condition)) + geom_density(alpha = 0.2)

 p=kruskal.test(formula, t)$p.value
 return (p)
}

stat_qu_by(subset(timeseries, study=="Lab-TEL"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="Lab-TEL" & performance_level == "HP")) #S, 4v5, 4v7
#pairwise.wilcox.test(subset(timeseries, study=="Lab-TEL")$QU, subset(timeseries, study=="Lab-TEL")$performance_level)
stat_qu_by(subset(timeseries, study=="Lab-TEL" & performance_level == "LP")) #N

stat_qu_by(subset(timeseries, study=="Lab-LST"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="Lab-LST" & performance_level == "HP")) #S, N pairwise
stat_qu_by(subset(timeseries, study=="Lab-LST" & performance_level == "LP")) #N


stat_qu_by(subset(timeseries, study=="Lab-AOD"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="Lab-AOD" & performance_level == "HP")) #N
stat_qu_by(subset(timeseries, study=="Lab-AOD" & performance_level == "LP")) #N


stat_qu_by(subset(timeseries, study=="Field-AOD"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="Field-AOD" & performance_level == "HP")) #S
pairwise.wilcox.test(subset(timeseries, study=="Field-AOD" & performance_level == "HP")$QU, subset(timeseries, study=="Field-AOD" & performance_level == "HP")$condition) #1v5, 3v5, 3v9, 4v5, 5v6, 5v9
stat_qu_by(subset(timeseries, study=="Field-AOD" & performance_level == "LP")) #S
pairwise.wilcox.test(subset(timeseries, study=="Field-AOD" & performance_level == "HP")$QU, subset(timeseries, study=="Field-AOD" & performance_level == "HP")$condition)

stat_qu_by(subset(timeseries, study=="skype"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="skype" & performance_level == "HP")) #N
stat_qu_by(subset(timeseries, study=="skype" & performance_level == "LP")) #N

stat_qu_by(subset(timeseries, study=="Field-TEL"), QU~performance_level)       #S
stat_qu_by(subset(timeseries, study=="Field-TEL" & performance_level == "HP")) #N
stat_qu_by(subset(timeseries, study=="Field-TEL" & performance_level == "LP")) #N

stat_qu_by(subset(timeseries, study=="Lab-LST-Video"), QU~performance_level)      
stat_qu_by(subset(timeseries, study=="Lab-LST-Video" & performance_level == "HP")) 
stat_qu_by(subset(timeseries, study=="Lab-LST-Video" & performance_level == "LP")) 

