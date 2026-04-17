## Supplemental depth profile figures ##
# using the bioavailpaper_revisions_fig1 code as the source code #

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

