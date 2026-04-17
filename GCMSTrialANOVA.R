# GCMS Trial Results - ANOVA comparing 2 22g replicates vs 3 15 g replicates
# reference: https://www.scribbr.com/statistics/anova-in-r/
install.packages("ggplot2","ggpubr","tidyverse","broom","AICmodavg")
R.Version()
library(devtools)
install.packages("AICcmodavg")
one.way<-aov(yield~Treatment)
library(AICcmodavg) #tests model fit
summary(T48A)
one.way<-aov(Peak.min.gdrysoil~Treatment,data=T48A)
summary(one.way)
two.way<-aov(Peak.min.gdrysoil~Treatment+Time.Sampled,data=T48A)
summary(two.way)
model.set<-list(one.way,two.way)
model.names<-c("one.way","two.way")
aictab(model.set,modnames=model.names)
#model with lowest AICc is best fit for data --> two.way in this case
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))
tukey.one.way<-TukeyHSD(one.way,ordered=FALSE,conf.level=0.95)
tukey.one.way
plot(one.way)
?TukeyHSD
#77 hour data
ow77<-aov(Peak.min.gdrysoil~Treatment,data=T77A)
summary(ow77)
dw77<-aov(Peak.min.gdrysoil~Treatment+Time.Sampled,data=T77A)
summary(dw77)
model.set<-list(ow77,dw77)
model.set
model.names<-c("ow77","dw77")
aictab(model.set,modnames=model.names)
tukey.ow77<-TukeyHSD(ow77,ordered=FALSE,conf.level=0.95)
tukey.ow77
# soooo with all the data points there is no sig dif in the trials... probably because of the weirdness with the humidity
