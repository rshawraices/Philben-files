#remaking fig 1 for bioavail paper - depth vs. GHG production

##REFERENCES##
#basic ggplot2 help: https://r-statistics.co/ggplot2-cheatsheet.html#Basic%20plot%20setup, https://ggplot2.tidyverse.org/
#flipping the axes: https://stackoverflow.com/questions/26838005/putting-x-axis-at-top-of-ggplot2-chart
#sub/superscripts in ggplot2: https://www.geeksforgeeks.org/r-language/how-to-display-a-variable-with-subscript-ggplot2-graph-in-r/
#reordering multiple legends/positioning: https://www.databrewer.co/R/visualization/12-ggplot2-reorder-multiple-legends,
    # https://stackoverflow.com/questions/52060601/ggplot-multiple-legends-arrangement
    # https://stackoverflow.com/questions/68497565/different-legend-positions-on-plot-with-multiple-legends
    # https://cran.r-project.org/web/packages/ggguides/vignettes/multiple-legends.html
#panel border: https://stackoverflow.com/questions/26191833/add-panel-border-to-ggplot2
#scale_continuous: https://www.statology.org/ggplot2-scale_y_continuous/
    #tick marks w/ scale_continuous:https://stackoverflow.com/questions/56247205/r-ggplot2-add-ticks-on-top-and-right-sides-of-all-facets
#tick marks: https://stackoverflow.com/questions/26367296/how-do-i-make-my-axis-ticks-face-inwards-in-ggplot2
  # https://r-charts.com/ggplot2/axis/#ticks
#legend symbol and point sizing: https://www.geeksforgeeks.org/r-language/how-to-change-the-legend-shape-using-ggplot2-in-r/,
    # https://stackoverflow.com/questions/15059093/ggplot2-adjust-the-symbol-size-in-legends,
    # https://www.geeksforgeeks.org/r-language/control-size-of-ggplot2-legend-items-in-r/
#overlaying multiple plots: https://stackoverflow.com/questions/40297206/overlaying-two-plots-using-ggplot2-in-r
#margins between multiple plots: https://stackoverflow.com/questions/15556068/removing-all-the-space-between-two-ggplots-combined-with-grid-arrange


#---------------------------------------------------------------------------------------------------------
install.packages("ggplot2","tidyverse")
install.packages("lemon")
library(ggplot2)
library(tidyverse)
library(gridExtra) #for having multiple plots in one object
library(lemon)     #for working with plot tickmarks

fig1data <- read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/bioavail_fig1_revisions.txt")
fig1data$Core <- as.factor(fig1data$Core) #make core into factor so that the symbols work later

#is.factor(fig1data$Core)
View(fig1data)

#---------------------------------------------------------------------------------------------------------
##Aerobic CO2##
fig1a <- ggplot(data=fig1data,
                aes(x=Aerobic.CO2,y=Depth,colour = Microtopography, shape=Core))+
         geom_point(size=3)+
         labs(x="Aerobic CO"[2]~ " Production (μmol/gdw/day)",y="Depth (m)")+
         expand_limits(x = 0, y = 0)+
         scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),    #putting tick marks on opp side w/o labels
                            position = "top", breaks=seq(0,450,75), limits=c(0,450),expand=c(0,0)) +
         scale_y_continuous(sec.axis=dup_axis(name=NULL, labels = NULL),  #putting tick marks on opp side w/o labels
                            breaks=seq(0.5,3,.5),limits=c(0,3), expand=c(0,0))+
         coord_trans(y = "reverse")+
         guides(color = guide_legend(override.aes = list(size = 5)))+
         theme(panel.background = element_rect(fill = NA),
               panel.grid.major = element_line(colour = NA),
               panel.grid.minor = element_line(colour = NA),
               panel.border=element_rect(colour="black",fill=NA,linewidth=1),
               text=element_text(family = "serif"),
               legend.position = "none", #legend.position.inside=c(.94,0.1),   ##<- hiding the legend
              # legend.background=element_rect(linewidth=.5,colour="black"),
               #legend.title = element_text(face="bold",size=12),
               #legend.text = element_text(size=11),
               axis.title = element_text(size=15),
               axis.text = element_text(size=14),
               axis.ticks.length = unit(-0.15,"cm"),
               plot.margin=unit(c(1,0.4,1,0.7),"cm"))               # makes space between combined plots so no labels get cut off
        # options(repr.plot.width = 5, repr.plot.height =2)
              
fig1a

#---------------------------------------------------------------------------------------------------------
##Anaerobic CO2 ##
fig1b <- ggplot(data=fig1data,
                aes(x=Anaerobic.CO2,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="Anaerobic CO"[2]~ " Production (μmol/gdw/day)", y=NULL)+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,3.5,0.5), limits=c(0,3.5),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0.5,3,.5),limits=c(0,3), expand=c(0,0))+
  coord_trans(y = "reverse")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "none",# legend.position.inside=c(.94,0.1),    ##<- hiding the legend
        #legend.background=element_rect(linewidth=.5,colour="black"),
        #legend.title = element_text(face="bold",size=12),
        #legend.text = element_text(size=11),
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.5,1,0.5),"cm"))

fig1b

#---------------------------------------------------------------------------------------------------------
## CH4 Prod ##
fig1c <- ggplot(data=fig1data,
                aes(x=CH4,y=Depth,colour = Microtopography, shape= Core))+
  geom_point(size=3)+
  labs(x="CH"[4]~ " Production (nmol/gdw/day)", y=NULL)+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,800,200), limits=c(0,800), expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(.5,3,.5),limits=c(0,3), expand=c(0,0))+
  coord_trans(y = "reverse")+
  guides(shape = guide_legend(order=1),
         color = guide_legend(override.aes = list(size = 3)) #makes the symbols larger
         )+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "inside", legend.position.inside=c(.7,0.42),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=14),
        legend.text = element_text(size=12),
        legend.box.just = "right",                                #centers the top legend over bottom one
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,1,1,0.1),"cm"))
       # axis.ticks.bottom = element_line(linetype="solid",colour="black"),
      #  axis.ticks.left=element_line(linetype="solid", colour="black"))

fig1c

#---------------------------------------------------------------------------------------------------------
##Combo plot ##

fig1 <- grid.arrange(fig1a, fig1b, fig1c, ncol=3)

ggsave("fig1.png", plot=fig1,
       path= "C:/Users/rache/Downloads/work stuff/research/Philben",
       units="in",
       width=12.5,
       height=5.5)

#export with width=1100 and height = 500

