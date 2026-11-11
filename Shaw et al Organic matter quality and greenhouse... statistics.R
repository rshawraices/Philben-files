### Stepwise multiple regressions


### Aerobic CO2 Regression

library(dplyr)

aerobSRdata<-read.delim(".txt file path") #using log-transformed GHG data for this regression
aerobSRdata$CH4<-NULL
aerobSRdata$Anaerobic.CO2<-NULL
aerobSRdata$Aerobic.CO2<-NULL
aerobSRdata$Anaerobic.CO2..log.<-NULL
aerobSRdata$CH4..log.<-NULL
aerobSRdata$Sample<- NULL

initial_model <- lm(Aerobic.CO2..log. ~ ., data = aerobSRdata)
initial_model

stepwise_aerCO2_<- step(initial_model, direction = "both")
summary(stepwise_aerCO2)



### Anaerobic CO2 Regression

anaerobSRdata<-read.delim(".txt file path") #using log-transformed GHG data for this regression
anaerobSRdata$CH4<-NULL
anaerobSRdata$Aerobic.CO2<-NULL
anaerobSRdata$Anaerobic.CO2<- NULL
anaerobSRdata$CH4..log.<-NULL
anaerobSRdata$Aerobic.CO2..log.<-NULL
anaerobSRdata$Sample<-NULL


initial_modelana <- lm(Anaerobic.CO2..log. ~ ., data = anaerobSRdata)
initial_modelana

stepwise_anaerCO2 <- step(initial_modelana, direction = "both")
summary(stepwise_anaerCO2)



### CH4 Regression

CH4SRdata<-read.delim(".txt file path") #using log-transformed GHG data for this regression
CH4SRdata$CH4<-NULL
CH4SRdata$Aerobic.CO2<-NULL
CH4SRdata$Anaerobic.CO2<- NULL
CH4SRdata$Anaerobic.CO2..log.<-NULL
CH4SRdata$Aerobic.CO2..log.<-NULL
CH4SRdata$Sample<-NULL

initial_modelCH4 <- lm(CH4..log. ~ ., data = CH4SRdata)
initial_modelCH4

stepwise_CH4 <- step(initial_modelCH4, direction = "both")
summary(stepwise_CH4)





### ANOVAs and Tukey's post-hoc test for depth and microtopography effects on GHG

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

GHGdata<-read.csv(".csv file path", header = TRUE, colClasses = c("numeric", "factor","numeric","numeric","numeric","factor"))

two.way.anaCO2 <- aov(Anaerobic_CO2 ~ Depth+Microtopography, data = GHGdata)
summary(two.way.anaCO2)

two.way.aerCO2 <- aov(Aerobic_CO2 ~ Depth*Microtopography, data = GHGdata)
summary(two.way.aerCO2)

two.way.methane <- aov(CH4 ~ Depth*Microtopography, data = GHGdata)
summary(two.way.methane)


# Tukey's post-hoc test

TukeyHSD(two.way.anaCO2)
TukeyHSD(two.way.aerCO2)
TukeyHSD(two.way.methane)


