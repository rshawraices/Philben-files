# Stats for Site __ Core __ comparing Respirometer and GCMS protocols 
#import dataset from Excel, from Statistics tab#
#name dataset X(site#)_(core#)B --> ex. X60881_1B 
#specify cells for desired compiled core data before importing the dataset #
one.way<-aov(CO2.ugCO2.gdrysoil~Treatment,data=X60881_1B)
one.way.2<-aov(CO2.ugCO2.gdrysoil~Sample,data=X60881_1B)
two.way.1<-aov(CO2.ugCO2.gdrysoil~Treatment+Time.hr,data=X60881_1B)
two.way.2<-aov(CO2.ugCO2.gdrysoil~Treatment+Time.hr+Sample,data=X60881_1B)
library(AICcmodavg)
model.set<-list(one.way,two.way.1,two.way.2)
model.names<-c("one.way","two.way.1","two.way.2")
aictab(model.set,modnames=model.names) # --> tells you which is better (lowest AICc #)
# two.way.2 is usually best
summary(two.way.2)
# p<0.05 is significant (conf. interval 95%)
tukey.one.way<-TukeyHSD(one.way,ordered=FALSE,conf.level=0.95)
tukey.one.way
summary(one.way)
plot(two.way.2)
# reference site: https://www.scribbr.com/statistics/anova-in-r/