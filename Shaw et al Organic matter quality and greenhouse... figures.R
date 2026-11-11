### Figure 1
library(ggplot2)
library(tidyverse)
library(gridExtra) 
library(lemon)    

fig1data <- read.delim("file path")
fig1data$Core <- as.factor(fig1data$Core)

# Fig. 1a - Aerobic CO2 & Depth

fig1a <- ggplot(data=fig1data,
                aes(x=Aerobic.CO2,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x=(expression(atop("Aerobic CO"[2]~ " Production", paste("(μmol gdw"^-1~" day"^-1~")")))),
       y=("Depth (m)"),
       tag=expression("a."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),    
                     position = "top", breaks=seq(0,450,75), limits=c(0,450),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels = NULL), 
                     breaks=seq(0.5,3,.5),limits=c(0,3), expand=c(0,0))+
  coord_trans(y = "reverse")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "none",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14),
        plot.margin=unit(c(1,0.4,1,0.7),"cm"))

fig1a

# Fig. 1b - Anaerobic CO2 & Depth

fig1b <- ggplot(data=fig1data,
                aes(x=Anaerobic.CO2,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x=expression(atop("Anaerobic CO"[2]~ " Production", paste("(μmol gdw"^-1~" day"^-1~")"))),
       y=NULL,
       tag=expression("b."))+
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
        legend.position = "none",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14),
        plot.margin=unit(c(1,0.5,1,0.5),"cm"))

fig1b

# Fig. 1c - CH4 & Depth

library(ggtext) #adding the legends to this panel

fig1c <- ggplot(data=fig1data,
                aes(x=CH4,y=Depth,colour = Microtopography, shape= Core))+
  geom_point(size=3)+
  labs(x=(expression(atop("CH"[4]~" Production", paste("(nmol gdw"^-1~" day"^-1~")")))),
       y=NULL,
       tag=expression("c."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,800,200), limits=c(0,800), expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(.5,3,.5),limits=c(0,3), expand=c(0,0))+
  coord_trans(y = "reverse")+
  guides(shape = guide_legend(order=1),
         color = guide_legend(override.aes = list(size = 3))
  )+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "inside", legend.position.inside=c(.7,0.45),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=14),
        legend.text = element_text(size=12),
        legend.box.just = "right",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14),
        plot.margin=unit(c(1,1,1,0.1),"cm"))

fig1c

# Fig. 1 combined plot

fig1 <- grid.arrange(fig1a, fig1b, fig1c, ncol=3)

ggsave("fig1.pdf", plot=fig1,
       path= "figure path",
       units="in",
       width=12.5,
       height=5.5,
       device="pdf",
       dpi=600)


### Figure 2

