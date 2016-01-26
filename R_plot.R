#http://www.r-bloggers.com/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/
#install.packages("extrafont")
#font_import()
#library(extrafont) 

suppressMessages(library(ggplot2))

ggplot_timeseries_create <- function(timeseries) {
 p <- ggplot(timeseries, aes(id, y=id)) + coord_cartesian(ylim=c(-0.3, 6.3), xlim=c(min(timeseries$id)-0.5, max(timeseries$id)+0.5)) 
 
 #  p <- p + ylab("QoE") + xlab("Episode") 
 #p <- p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
 p <- p + theme(axis.title.y = element_blank())
 p <- p + theme(axis.text.x=element_text(colour="black"))
 
 #  p <- p + scale_y_continuous(labels=c("extrem schlecht (0)","schlecht (1)","dürftig (2)", "ordentlich (3)", "gut (4)", "ausgezeichnet (5)", "ideal (6)"), breaks=0:6)
 p <- p + scale_y_continuous(labels=c("Extremely\nbad (0)","Bad (1)", "Poor (2)", "Fair (3)", "Good (4)", "Excellent (5)", "Ideal (6)"), breaks=0:6)
 p <- p + theme(axis.text.y=element_text(colour="black"))
 
 p <- p + theme(legend.position="none")
 
 #Shift x-axis (if needed)
 #  timeseries$id = timeseries$id + 1
 
 return (p)
}

