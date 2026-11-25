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
library(dbplyr)

GHGdata<-read.delim(".txt file path", header = TRUE, colClasses = c("character", "factor","numeric","numeric","numeric","numeric"))

#need to bin the depth values into categories for ANOVA 
GHGdata <- GHGdata %>% mutate(Depthcat = case_when(GHGdata$Depth >= 0  & GHGdata$Depth <= 1 ~ 'shallow',
                                             Depth >= 1.01  & Depth <= 2 ~ 'intermediate',
                                             Depth >= 2.01  & Depth <= 3 ~ 'deep'))

two.way.anaCO2 <- aov(AnaCO2 ~ Depthcat+Microtopography, data = GHGdata)
summary(two.way.anaCO2)

two.way.aerCO2 <- aov(AerCO2 ~ Depthcat*Microtopography, data = GHGdata)
summary(two.way.aerCO2)

two.way.methane <- aov(CH4 ~ Depthcat*Microtopography, data = GHGdata)
summary(two.way.methane)


# Tukey's post-hoc test

TukeyHSD(two.way.anaCO2)
TukeyHSD(two.way.aerCO2)
TukeyHSD(two.way.methane)


# looking at differences between GHG production within specific microtopographies

GHGdata_sedge <- subset(GHGdata, Microtopography=="SedgeMeadow")
GHGdata_humm <- subset(GHGdata, Microtopography=="Hummock")
GHGdata_holl <- subset(GHGdata, Microtopography=="Hollow")

one.way.anaCO2.sedge <- aov(AnaCO2 ~ Depthcat, data = GHGdata_sedge)
one.way.anaCO2.holl <- aov(AnaCO2 ~ Depthcat, data = GHGdata_holl)
one.way.anaCO2.humm <- aov(AnaCO2 ~ Depthcat, data = GHGdata_humm)
summary(one.way.anaCO2.sedge)
summary(one.way.anaCO2.holl)
summary(one.way.anaCO2.humm)


one.way.aerCO2.sedge <- aov(AerCO2 ~ Depthcat, data = GHGdata_sedge)
one.way.aerCO2.holl <- aov(AerCO2 ~ Depthcat, data = GHGdata_holl)
one.way.aerCO2.humm <- aov(AerCO2 ~ Depthcat, data = GHGdata_humm)
summary(one.way.aerCO2.sedge)
summary(one.way.aerCO2.holl)
summary(one.way.aerCO2.humm)

one.way.methane.sedge <- aov(CH4 ~ Depthcat, data = GHGdata_sedge)
one.way.methane.holl <- aov(CH4 ~ Depthcat, data = GHGdata_holl)
one.way.methane.humm <- aov(CH4 ~ Depthcat, data = GHGdata_humm)
summary(one.way.methane.sedge)
summary(one.way.methane.holl)
summary(one.way.methane.humm)
