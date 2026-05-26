## Supplemental depth profile figures ##
# using the bioavailpaper_revisions_fig1 code as the source code #

#---------------------------------------------------------------------------------------------------------
install.packages("ggplot2","tidyverse")
install.packages("lemon")
library(ggplot2)
library(tidyverse)
library(gridExtra) #for having multiple plots in one object
library(lemon)     #for working with plot tickmarks

suppfigdata <- read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/bioavail_suppfigs.txt")
suppfigdata$Core <- as.factor(suppfigdata$Core) #make core into factor so that the symbols work later

#is.factor(fig1data$Core)
View(suppfigdata)

#---------------------------------------------------------------------------------------------------------
## %C as Sugars ##
csugars <- ggplot(data=suppfigdata,
                aes(x=X.CasSugars,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% C as Sugars",y="Depth (m)")+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),    #putting tick marks on opp side w/o labels
                     position = "top", breaks=seq(0,15,5), limits=c(0,15),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels = NULL),  #putting tick marks on opp side w/o labels
                     breaks=seq(0.5,3,0.5),limits=c(0,3), expand=c(0,0))+
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
        plot.margin=unit(c(0.3,0.4,0.3,0.7),"cm"))               # makes space between combined plots so no labels get cut off
# options(repr.plot.width = 5, repr.plot.height =2)

csugars

#---------------------------------------------------------------------------------------------------------
## % C as AA ##
cthaa <- ggplot(data=suppfigdata,
                aes(x=X..CasAA,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% C as THAA", y=NULL)+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,15,5), limits=c(0,15),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0.5,3,0.5),limits=c(0,3), expand=c(0,0))+
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
        plot.margin=unit(c(0.3,0.5,0.3,0.5),"cm"))

cthaa

#---------------------------------------------------------------------------------------------------------
## % N as AA ##
nthaa <- ggplot(data=suppfigdata,
                aes(x=X..NasAA,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% N as THAA", y=NULL)+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,70,10), limits=c(0,70),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0.5,3,0.5),limits=c(0,3), expand=c(0,0))+
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
        plot.margin=unit(c(0.3,0.5,0.3,0.5),"cm"))

nthaa

#---------------------------------------------------------------------------------------------------------
## C:N ##
cn <- ggplot(data=suppfigdata,
                aes(x=C.N, y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="C:N", y= "Depth (m)")+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,25,5), limits=c(0,25),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0.5,3,0.5),limits=c(0,3), expand=c(0,0))+
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
        plot.margin=unit(c(0.3,0.5,0.5,0.5),"cm"))

cn

#---------------------------------------------------------------------------------------------------------
## % Rhamnose ##
rhamnose <- ggplot(data=suppfigdata,
                aes(x=X.Rhamnose,y=Depth,colour = Microtopography, shape= Core))+
  geom_point(size=3)+
  labs(x="% Rhamnose", y=NULL)+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,20,4), limits=c(0,20), expand=c(0,0)) +
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
        legend.position = "right", #legend.position.inside=c(.7,0.42),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=14),
        legend.text = element_text(size=12),
        legend.box.just = "right",                                #centers the top legend over bottom one
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.3,1,0.5,0.1),"cm"))
# axis.ticks.bottom = element_line(linetype="solid",colour="black"),
#  axis.ticks.left=element_line(linetype="solid", colour="black"))

rhamnose


#---------------------------------------------------------------------------------------------------------
##Combo plot ##


suppfig <- grid.arrange(grobs=list(csugars, cthaa, nthaa, cn, rhamnose), 
             layout_matrix = matrix(c(1,1,2,2,3,3,NA,4,4,5,5,NA),nrow=2, byrow=T),
             widths=c(0.5,0.5,0.5,0.5,0.5,0.5),
             heights=c(1,1))
 

suppfig

ggsave("suppfig.png", plot=suppfig,
       path= "C:/Users/rache/Downloads/work stuff/research/Philben",
       units="in",
       width=12.5,
       height=5.5)

