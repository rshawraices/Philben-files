### Figure 1
library(ggplot2)
library(tidyverse)
library(gridExtra) 
library(lemon)    

fig1data <- read.delim(".txt file path")
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




### Figures 2 & 3 - PCA Plots

library(factoextra)
library(MASS)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(ggrepel)

totalPCA1<-read.delim(".txt file path")

#changing column names
colnames(totalPCA1)[which(names(totalPCA1) == "C.N")] <- "C:N"
colnames(totalPCA1)[which(names(totalPCA1) == "X.CasSugars")] <- "%CSugars"
colnames(totalPCA1)[which(names(totalPCA1) == "X.Glucose")] <- "%Glucose"
colnames(totalPCA1)[which(names(totalPCA1) == "X.Rhamnose")] <- "%Rhamnose"
colnames(totalPCA1)[which(names(totalPCA1) == "TotalInorganicN")] <- "TotalInorgN"
colnames(totalPCA1)[which(names(totalPCA1) == "X..NasAA")] <- "THAA%N"
colnames(totalPCA1)[which(names(totalPCA1) == "AADegIndex")] <- "AAIndex"
colnames(totalPCA1)[which(names(totalPCA1) == "X..CasAA")] <- "THAA%C"
colnames(totalPCA1)[which(names(totalPCA1) == "Mol.Hydroxyproline")] <- "%Hyp"
colnames(totalPCA1)[which(names(totalPCA1) == "Mol.Glycine..w.o.Norvaline.")] <- "%Glycine"
colnames(totalPCA1)[which(names(totalPCA1) == "CNormYieldHydroxyproline")] <- "HypC"
colnames(totalPCA1)[which(names(totalPCA1) == "Anaerobic.CO2")] <- "AnaCO2"
colnames(totalPCA1)[which(names(totalPCA1) == "Aerobic.CO2")] <- "AerCO2"

analysis<-prcomp(totalPCA1, center=TRUE, scale=TRUE)
analysis

#making a dataframe from the prcomp results
scores <- as.data.frame(analysis$x)
loadings <- as.data.frame(analysis$rotation)

loadings

# Fig. 2 - PCA loadings plot

fig2 <- ggplot(data = loadings, aes(x=PC1, y=PC2)) +
  labs(x="PC1 (25.3%)", y="PC2 (17.5%)")+
  scale_x_continuous(breaks=seq(-0.3,0.6,0.3), limits=c(-0.6,0.6), expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-0.6,0.6,0.3), limits=c(-0.6,0.6), expand=c(0,0))+
  geom_segment(data = loadings, aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow = arrow(length = unit(0.2, "cm")), color = "black", linewidth=0.3) +
  geom_text_repel(data = loadings, aes(x = PC1, y = PC2, label=rownames(loadings)),
                  color = "black", vjust = -0.5, family="serif", fontface="bold",
                  size=4.23333, direction="both", 
                  nudge_x=c(0,0,0,0,0,0,0,0,-0.05,0,0,0.07,0,0,0,0),
                  nudge_y=c(0,0,0.02,0,0.01,0,0.04,0,0.06,0,0,0.02,0,0,0.01,0.01),
                  min.segment.length=1)+
  geom_vline(xintercept = 0, linewidth=0.3)+
  geom_hline(yintercept = 0, linewidth=0.3)+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"))

fig2

ggsave("fig2.pdf", plot=fig2,
       path= "figure path",
       units="in",
       width=5,
       height=5,
       device="pdf",
       dpi=600)




# Fig. 3 - PCA scores plot

fig3data<-read.delim(".txt file path")

# Fig. 3a - Depth PCA scores

fig3a <- ggplot(data=fig3data, aes(x=PC1.Scores, y=PC2.Scores,color=Depth.cat,shape=Depth.cat))+
  geom_point(size=2.5)+
  labs(x="PC1 (25.3%)", y="PC2 (17.5%)",color="Depth", shape="Depth",
       tag = expression("a."))+
  scale_x_continuous(breaks=seq(-3.0,4.5,1.5), limits=c(-4.5,4.5), expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-4.5,4.5,1.5), limits=c(-4.5,4.5), expand=c(0,0))+
  geom_vline(xintercept = 0, linewidth=0.5)+
  geom_hline(yintercept = 0, linewidth=0.5)+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.text = element_text(size=12),
        legend.title=element_text(size=14,face="bold", hjust=0.5),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.box.just="center",
        legend.position="inside", legend.position.inside = c(0.875,0.84),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

fig3a

# Fig. 3b - Microtopography PCA scores

fig3b <- ggplot(data=fig3data, aes(x=PC1.Scores, y=PC2.Scores,color=Microtopography,shape=Microtopography))+
  geom_point(size=2.5)+
  labs(x="PC1 (25.3%)", y="PC2 (17.5%)",
       tag = expression("b."))+
  scale_x_continuous(breaks=seq(-3.0,4.5,1.5), limits=c(-4.5,4.5), expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-4.5,4.5,1.5), limits=c(-4.5,4.5), expand=c(0,0))+
  geom_vline(xintercept = 0, linewidth=0.5)+
  geom_hline(yintercept = 0, linewidth=0.5)+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        legend.text = element_text(size=12),
        legend.title=element_text(size=14,face="bold", hjust=0.5),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.box.just="center",
        legend.position="inside", legend.position.inside = c(0.83,0.84),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))+
  scale_color_viridis_d(option = "viridis")

fig3b


# Fig. 3 combined plot

fig3combo <- grid.arrange(fig3a,fig3b,ncol=2)

ggsave("fig3.pdf", plot=fig3combo,
       path= "figure path",
       units="in",
       width=12.5,
       height=5,
       device="pdf",
       dpi=600)



### Figure 4

library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(cowplot)

fig4data <- read.delim(".txt file path")

# Fig. 4a - Predicted vs. experimental aerobic CO2 
