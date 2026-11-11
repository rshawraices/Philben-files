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



### Figure 4 - Predicted vs. observed greenhouse gas production

library(ggplot2)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(cowplot)

fig4data <- read.delim(".txt file path")

# Fig. 4a - Predicted vs. experimental aerobic CO2 

fig4a <- ggplot(data=fig4data,
                aes(x=Exp..Aerob.CO2,y=Pred..Aerob.CO2,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x=expression(atop("Experimental Aerobic CO"[2]~" ",paste("(μmol gdw"^-1~"day"^-1~")"))),
       y=expression(atop("Predicted Aerobic CO"[2]~ " ", paste("(μmol gdw"^-1~" day"^-1~")"))),
       #  caption= expression("R"^2~"=0.66"),
       tag=expression("a."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(75,450,75), limits=c(0,450),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,450,75),limits=c(0,450), expand=c(0,0)) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "inside", legend.position.inside=c(.28,0.813),
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=11.5),
        legend.text = element_text(size=10.5),
        legend.box.just = "left",
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.3,1,0.4),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))+
  geom_text(x=330,y=80,
            label=expression(atop("R"^2~"=0.66",paste("RMSE=47.83"))), 
            color="black", family="serif", size=12, size.unit="pt")+
  geom_line(aes(x=line.1, y=line),color="black")


fig4a

# Fig. 4b - Predicted vs. experimental anaerobic CO2

fig4b <- ggplot(data=fig4data,
                aes(x=Exp..Anaerob.CO2,y=Pred..Anaerob.CO2,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x=expression(atop("Experimental Anaerobic CO"[2]~ " ",paste("(μmol gdw"^-1~" day"^-1~")"))),
       y=expression(atop("Predicted Anaerobic CO" [2]~ " ", paste("(μmol gdw"^-1~" day"^-1~")"))),
       tag=expression("b."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0.5,2.5,.5), limits=c(0,2.5),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,2.5,.5),limits=c(0,2.5), expand=c(0,0)) +
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "none",
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.3,1,0.3),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))+
  geom_text(x=2.0,y=0.45,
            label=expression(atop("R"^2~"=0.39",paste("RMSE=0.26"))), 
            color="black", family="serif", size=12, size.unit="pt")+
  geom_line(aes(x=line, y=line.1),color="black")


fig4b

# Fig. 4c - Predicted vs. experimental CH4

fig4c <- ggplot(data=fig4data,
                aes(x=Exp..CH4,y=Pred..CH4,colour = Microtopography, shape = Microtopography))+
  geom_point(size=3)+
  labs(x=expression(atop("Experimental CH"[4]~ " ", paste("(nmol gdw"^-1~" day"^-1~")"))),
       y=expression(atop("Predicted CH" [4]~ " ", paste("(nmol gdw"^-1~" day"^-1~")"))),
       tag=expression("c."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(80,480,80), limits=c(0,480),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,480,80),limits=c(0,480), expand=c(0,0))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position= "none",
        axis.title = element_text(size=14),
        axis.text = element_text(size=12),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(1,0.7,1,0),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))+
  geom_text(x=375,y=140,
            label=expression(atop("R"^2~"=0.49",paste("RMSE=98.72"))), 
            color="black", family="serif", size=12, size.unit="pt")+
  geom_line(aes(x=line, y=line.1),color="black")

fig4c

# make the inset plot

inset.plot <- fig4c + 
  labs(x=NULL, y=NULL, caption=NULL,tag=NULL) + 
  theme(legend.position = "none",
        plot.margin=unit(c(0,0,0,0),"cm"))+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(40,80,40), limits=c(0,80),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(0,80,40),limits=c(0,80), expand=c(0,0))

# add inset plot to Fig. 4c
fig4c_inset <-
  ggdraw() +
  draw_plot(fig4c) +
  draw_plot(inset.plot, x = 0.25, y = .61, width = .36, height = .26)

fig4c_inset

# Fig. 4 combined plot

fig4 <- grid.arrange(fig4a, fig4b, fig4c_inset, ncol=3)

ggsave("fig4.pdf", plot=fig4,
       path= "figure path",
       units="in",
       width=12,
       height=5,
       device="pdf",
       dpi=600)






### Supplemental Figure 1 - depth profiles

library(ggplot2)
library(tidyverse)
library(gridExtra)
library(lemon)

suppfigdata <- read.delim(".txt file path")
suppfigdata$Core <- as.factor(suppfigdata$Core) #make core into factor so that the symbols work later


# Supp. Fig. 1a - %C as sugars & depth

csugars <- ggplot(data=suppfigdata,
                  aes(x=X.CasSugars,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% C as Sugars",y="Depth (m)",
       tag=expression("a."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,15,5), limits=c(0,15),expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels = NULL),
                     breaks=seq(0.5,3,0.5),limits=c(0,3), expand=c(0,0))+
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
        plot.margin=unit(c(0.3,0.2,0.3,0.7),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

csugars

# Supp. Fig. 1b - % C as AA & depth

cthaa <- ggplot(data=suppfigdata,
                aes(x=X..CasAA,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% C as THAA", y=NULL,
       tag=expression("b."))+
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
        legend.position = "none",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.3,0.6,0.3,0.7),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

cthaa

# Supp. Fig. 1c - % N as AA & depth

nthaa <- ggplot(data=suppfigdata,
                aes(x=X..NasAA,y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="% N as THAA", y=NULL,
       tag=expression("c."))+
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
        legend.position = "none",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.3,0.7,0.3,0.1),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

nthaa

# Supp. Fig. 1d - C:N & depth

cn <- ggplot(data=suppfigdata,
             aes(x=C.N, y=Depth,colour = Microtopography, shape=Core))+
  geom_point(size=3)+
  labs(x="C:N", y= "Depth (m)",
       tag=expression("d."))+
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
        legend.position = "none",# legend.position.inside=c(.94,0.1),
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.3,0.2,0.5,0),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

cn

# Supp. Fig. 1e - % Rhamnose & depth

rhamnose <- ggplot(data=suppfigdata,
                   aes(x=X.Rhamnose,y=Depth,colour = Microtopography, shape= Core))+
  geom_point(size=3)+
  labs(x="% Rhamnose", y=NULL,
       tag=expression("e."))+
  expand_limits(x = 0, y = 0)+
  scale_x_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     position = "top", breaks=seq(0,20,4), limits=c(0,20), expand=c(0,0)) +
  scale_y_continuous(sec.axis=dup_axis(name=NULL, labels=NULL),
                     breaks=seq(.5,3,.5),limits=c(0,3), expand=c(0,0))+
  coord_trans(y = "reverse")+
  guides(shape = guide_legend(order=1),
         color = guide_legend(override.aes = list(size = 3),nrow=3,)
  )+
  theme(panel.background = element_rect(fill = NA),
        panel.grid.major = element_line(colour = NA),
        panel.grid.minor = element_line(colour = NA),
        panel.border=element_rect(colour="black",fill=NA,linewidth=1),
        text=element_text(family = "serif"),
        legend.position = "inside", legend.position.inside=c(.45,0.45),
        legend.direction = "horizontal",
        legend.title.position = "top", 
        legend.background=element_rect(linewidth=.5,colour="black"),
        legend.title = element_text(face="bold",size=14, hjust=0.5),
        legend.text = element_text(size=12),
        legend.box.just = "center",
        axis.title = element_text(size=15),
        axis.text = element_text(size=14),
        axis.ticks.length = unit(-0.15,"cm"),
        plot.margin=unit(c(0.3,0.6,0.5,0.6),"cm"),
        plot.tag.position = "topleft",
        plot.tag = element_text(size=14))

rhamnose

#running rhamnose without the legend for the combo plot

rhamnosenoleg <- rhamnose+theme(legend.position = "none")
rhamnosenoleg

#pulling the legend out of rhamnose and making it its own object

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(rhamnose)

mylegend

# Supp. Fig. 1 Combined plot

suppfig <- grid.arrange(grobs=list(csugars, cthaa, nthaa, cn, rhamnosenoleg,mylegend), 
                        layout_matrix = matrix(c(1,1,2,2,3,3,
                                                 NA,4,4,5,5,6),nrow=2, byrow=T),
                        widths=c(0.5,0.5,0.5,0.5,0.5,0.4),
                        heights=c(1,1))

suppfig

ggsave("suppfig.pdf", plot=suppfig,
       path= "figure path",
       units="in",
       width=12.5,
       height=9,
       device="pdf",
       dpi=600)

