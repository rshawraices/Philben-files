#remaking multiple regressions fig for bioavail paper revisions - 2/24/2026
#using my fig 1 revisions codes as a starting point 

##REFERENCES##
#overlaying 2 functions: https://stackoverflow.com/questions/40297206/overlaying-two-plots-using-ggplot2-in-r
#inset: https://stackoverflow.com/questions/5219671/it-is-possible-to-create-inset-graphs
#captions and subtitles: https://r-charts.com/ggplot2/titles/
#titles in ggplot: https://r-graph-gallery.com/289-control-ggplot2-title.html
#calculating rmse in excel: https://www.statology.org/root-mean-square-error-excel/
           
   
#---------------------------------------------------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)  #for having multiple plots in one object
library(cowplot)    #for making inset plot

fig4data <- read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/test_fig4.txt") #test_fig4 file just has different values for the line since there were gaps for some reason, the actual data is the same as the revisions_fig4.txt file
View(fig4data)

#---------------------------------------------------------------------------------------------------------
##Aerobic CO2##

fig4a <- ggplot(data=fig4data,
                aes(x=Exp..Aerob.CO2,y=Pred..Aerob.CO2,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x="Experimental Aerobic CO"[2]~" ",y=expression("Predicted Aerobic CO" [2]~ " (μmol gdw"^-1~" day"^-1~")"),
       caption= expression("(μmol gdw"^-1~"day"^-1~")"))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,450,75), limits=c(0,450),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,450,75),limits=c(0,450), expand=c(0,0)) +
 # coord_trans(y = "reverse")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "inside", legend.position.inside=c(.275,0.813),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=13),
        legend.text = element_text(size=12),
        legend.box.just = "left",
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.3,1,0.5),"cm"),
        plot.caption.position = "plot",                                ##this is the units line
        plot.caption = element_text(hjust = 0.6, size=14))+                     ##centering the units
 geom_line(aes(x=line.1, y=line),color="black") #adds the line
  

fig4a

#---------------------------------------------------------------------------------------------------------
##Anaerobic CO2##
fig4b <- ggplot(data=fig4data,
                aes(x=Exp..Anaerob.CO2,y=Pred..Anaerob.CO2,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x="Experimental Anaerobic CO"[2]~ " ",y=expression("Predicted Anaerobic CO" [2]~ " (μmol gdw"^-1~" day"^-1~")"),
      caption=expression("(μmol gdw"^-1~" day"^-1~")"))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,2.5,.5), limits=c(0,2.5),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,2.5,.5),limits=c(0,2.5), expand=c(0,0)) +
  # coord_trans(y = "reverse")+
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
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.25,1,0.25),"cm"),
        plot.caption.position = "plot",                                ##this is the units line
        plot.caption = element_text(hjust = 0.6, size=14))+ 
  geom_line(aes(x=line, y=line.1),color="black")


fig4b

#---------------------------------------------------------------------------------------------------------
##CH4##
fig4c <- ggplot(data=fig4data,
                aes(x=Exp..CH4,y=Pred..CH4,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x="Experimental CH"[4]~ " ",y=expression("Predicted CH" [4]~ " (nmol gdw"^-1~" day"^-1~")"),
       caption= expression("(nmol gdw"^-1~" day"^-1~")"))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,480,80), limits=c(0,480),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,480,80),limits=c(0,480), expand=c(0,0))+
  # coord_trans(y = "reverse")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position= "none", # legend.position.inside = c(.32, 0.80), #legend.position.inside=c(.94,0.1),   ##<- hiding the legend
       # legend.background=element_rect(linewidth=.5,colour="black"),
        #legend.title = element_text(face="bold",size=14),
        #legend.text = element_text(size=12),
        #legend.box.just = "left",
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.5,1,0.3),"cm"),
        plot.caption.position = "plot",                                ##this is the units line
        plot.caption = element_text(hjust = 0.62, size=14))+ 
  geom_line(aes(x=line, y=line.1),color="black")

fig4c

#now making the inset plot

inset.plot <- fig4c + 
              labs(x=NULL, y=NULL, caption=NULL) + 
              theme(legend.position = "none",
                    plot.margin=unit(c(0,0,0,0),"cm"))+
              scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                                breaks=seq(0,80,40), limits=c(0,80),expand=c(0,0)) +
              scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                                breaks=seq(0,80,40),limits=c(0,80), expand=c(0,0))
#put them together
fig4c_inset <-
  ggdraw() +
  draw_plot(fig4c) +
  draw_plot(inset.plot, x = 0.185, y = .61, width = .38, height = .28)

fig4c_inset

#---------------------------------------------------------------------------------------------------------
#all together
fig4 <- grid.arrange(fig4a, fig4b, fig4c_inset, ncol=3)

ggsave("fig4final.png", plot=fig4,
       path= "C:/Users/rache/Downloads/work stuff/research/Philben",
       units="in",
       width=12,
       height=5)

#export with width=1100 and height = 500