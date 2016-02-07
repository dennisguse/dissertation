#http://www.r-bloggers.com/change-fonts-in-ggplot2-and-create-xkcd-style-graphs/
#XXXinstall.packages("extrafont")
#XXXfont_import()
#XXXsuppressMessages(library(extrafont))

suppressMessages(library(ggplot2))

ggplot_timeseries_create <- function(timeseries) {
 p <- ggplot(timeseries, aes(id, y=id)) + coord_cartesian(ylim=c(-0.3, 6.3), xlim=c(min(timeseries$id)-0.5, max(timeseries$id)+0.5)) 
 

 #  p <- p + scale_y_continuous(labels=c("extrem schlecht (0)","schlecht (1)","dürftig (2)", "ordentlich (3)", "gut (4)", "ausgezeichnet (5)", "ideal (6)"), breaks=0:6)
 p <- p + scale_y_continuous(labels=c("Extremely\nbad (0)","Bad (1)", "Poor (2)", "Fair (3)", "Good (4)", "Excellent (5)", "Ideal (6)"), breaks=0:6)

 p <- p + theme(axis.text.x=element_text(colour="black"))
 p <- p + theme(axis.text.y=element_text(colour="black"))
 
 p <- p + theme(text=element_text(family="Palatino"))
 p <- p + theme(strip.background = element_rect(colour="white", fill="white"), strip.text=element_text(size=15)) 
 
 p <- p + theme(panel.background=element_rect(fill="white", color="white"))
 # p <- p + theme(plot.background=element_rect(fill="white", color="white"))
 #p <- p + theme(panel.border=element_rect(color="white"))
 p <- p + theme(panel.grid.major=element_line(color="darkgray",size=.25))
 p <- p + theme(panel.grid.minor=element_line(color="gray",size=.25))
  
 
 p <- p + xlab("Usage Episode")
 p <- p + ylab("Episodic Judgment")
 
 p <- p + theme(legend.position="none")

# p <- p + theme(plot.margin=unit(c(1, 1, 10,  10), "lines"))
 
 #Shift x-axis (if needed)
 #  timeseries$id = timeseries$id + 1
 
 return (p)
}

ggplot_model_create <- function(data, ylim=2.4) {
 p <- ggplot(data) + coord_cartesian(ylim=c(0, ylim), xlim=c(min(data$parameter)-0.4, max(data$parameter)+0.4))
 p <- p + scale_x_continuous(breaks = 1:max(data$parameter))
 
 p <- p + theme(axis.text.x=element_text(colour="black"))
 p <- p + theme(axis.text.y=element_text(colour="black"))
 
 p <- p + theme(text=element_text(family="Palatino"))
 p <- p + theme(strip.background = element_rect(colour="white", fill="white"), strip.text=element_text(size=15)) 
  
 p <- p + theme(panel.background=element_rect(fill="white", color="white"))
# p <- p + theme(plot.background=element_rect(fill="white", color="white"))
 #p <- p + theme(panel.border=element_rect(color="white"))
 p <- p + theme(panel.grid.major=element_line(color="darkgray",size=.25))
 p <- p + theme(panel.grid.minor=element_line(color="gray",size=.25))

 p <- p + ylab("RMSD") + xlab("w")
 p <- p + guides(fill=guide_legend(title="Condition"))
 p <- p + theme(legend.position="bottom", legend.key = element_rect(fill = "white"))
 
  
 return (p)
}