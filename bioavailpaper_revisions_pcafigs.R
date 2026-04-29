## remaking pca figs for bioavail paper revisions - figs 2-3

##REFERENCES##
#loadings plot: https://statisticsglobe.com/loading-plot-in-r#video-further-resources-summary
#biplots: https://www.geeksforgeeks.org/r-language/how-to-create-a-biplot-in-r/
          # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/biplot
          # https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/biplot.princomp
#general PCA: https://www.geeksforgeeks.org/r-language/how-to-make-pca-plot-with-r/
#changing column titles for a dataframe: https://www.geeksforgeeks.org/r-language/change-column-name-of-a-given-dataframe-in-r/


#----------------------------------------------------------------------------------------------------------------------------
##not super sure which are necessary for final code
install.packages("ggfortify")
install.packages("factoextra") 
install.packages("MASS")

library(factoextra)
library(MASS)
library(ggplot2)
library(ggfortify)
library(ggpubr)

totalPCA1<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/pca3.20.24.txt") #this is my old PCA code
View(totalPCA1)

#changing column names# 
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



analysis<-prcomp(totalPCA1, center=TRUE, scale=TRUE) #rank has it only output the first 2 pcas
analysis
biplot(analysis, xlim=c(-0.4,0.4), ylim=c(-0.4,0.4))

?biplot
?prcomp

#results from 'analysis':         
#                      PC1         PC2
#pH             0.27972687  0.22328745
#C:N           -0.39997146 -0.13340020
#TotalOrgAcids -0.04972301  0.33290129
#TotalInorgN    0.34364558 -0.04322101
#%CSugars      -0.25996580 -0.10271434
#%Glucose       0.43207479  0.01108056
#%Rhamnose      0.32307650  0.03731928
#THAA%N        -0.05710087 -0.45518750
#AAIndex        0.02021676  0.10154339
#THAA%C         0.25827026 -0.31651734
#%Hyp           0.03419910 -0.43138236
#%Glycine       0.03870026  0.16486751
#HypC           0.14052314 -0.49082138
#AnaCO2        -0.27689146 -0.10546571
#AerCO2        -0.27239403  0.14281376
#CH4           -0.19725010 -0.06483542

#---playing around with trying to fix the axes ---#
pcadf <- read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/pcas4.29.2026.txt")
View(pcadf)

ggscatter(pcadf, x="PC1", y="PC2", label = "X", repel=TRUE)
?ggscatter


fviz_pca(pcadf, geom.ind=FALSE, geom.var=c("text", "arrow"))

fviz(pcadf, geom=c("text","arrow"))

#------------------------------------------------------------------------------------------------------------------------
#loadings plot
fig2prep <- fviz_pca_var(analysis, col.circle = NA, col.var = "black",
                         axes.linetype="solid", labelsize=5,
                         font.family = "serif", geom=c("arrow","text"),
                        # font.subtitle="bold",
                       #  label.rectangle=TRUE,
                        # font.label = c("black", 5, "bold"),
                       #  font.label= "bold",          #this is supposed to make the labels bold but there seems to be an issue with the wrapper to ggscatter and I can't find a way to resolve it
                        # face="bold",
                      # font.label = list(face = "bold"),
                         repel = TRUE, arrowsize=0.4)+
            labs(x="PC1 (25.3%)", y = "PC2 (17.5%)", title=NULL)+
            theme(panel.grid.major = element_line(colour = NA),
                  panel.grid.minor = element_line(colour = NA),
                  panel.border=element_rect(colour="black", fill=NA, linewidth=1),
                  axis.title = element_text(size=14),
                  axis.text = element_text(size=12),
                  text=element_text(family = "serif"),
                  plot.background = element_rect(fill = "white", linewidth=NA))
        

fig2prep

ggsave("fig2.png", plot=fig2prep,
       path= "C:/Users/rache/Downloads/work stuff/research/Philben",
       units="in",
       width=12.5,
       height=5.5)

##BOOKMARK ----- trying to get loadings labels in times new roman, gotta fix labels, get them not to be overlapped by arrows, etc etc

?fviz_pca_var

#-- old work ---------------------------------------------------------------------------------------------------
fig2data <- read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/bioavail_fig2_revisions.txt")
View(fig2data)
fig2data$PC1

#fig2 <- ggplot(data=fig2data,
aes(x=PC1, y=PC2))+
  geom_point()

#fig2b <- autoplot(fig2, data=fig2data, loadings=TRUE, loadings.label=TRUE, loadings.label.size=3)
fig2b


#fig2 <- biplot(x=fig2data$PC1, y=fig2data$PC2, var.axes=TRUE)
#fig2 <- ggplot(data=fig2data,
aes(x=PC1, y=PC2, colour=Sampls))+
  #fig2 <- autoplot(analysis, data=fig2data, loadings = TRUE, loadings.label=TRUE, loadings.label.size=3)
  ?autoplot
