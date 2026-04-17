## making a stepwise regression model for fig 4 ##
## per article linked below will do a stepwise selection (combines forward selection and backward elimination) regression ##

##REFERENCES##
#stepwise regression 
  #stats theory sources: https://quantifyinghealth.com/stepwise-selection/
  #coding sources: https://www.statology.org/stepwise-regression-r/,
      #https://www.geeksforgeeks.org/r-machine-learning/stepwise-regression-in-r/,
      #


library(dplyr)   #for getting rid of certain columns in dataset

#-----------------------------------------------------------------------------------------------------------------
# Aerobic CO2 
#aerobSRdata2 has depth in it, aerobSRdata3 has depth and logged GHG data
#going to do a stepwise selection with the PCA dataset cuz has all the data in it

aerobSRdata<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/pca3.20.24.txt") #this is my old PCA code
aerobSRdata$CH4<-NULL
aerobSRdata$Anaerobic.CO2<-NULL

aerobSRdata

#initial model
initial_model <- lm(Aerobic.CO2 ~ ., data = aerobSRdata)

# Perform stepwise selection
stepwise_aerCO2 <- step(initial_model, direction = "both")

# Print the summary of the selected model
summary(stepwise_aerCO2)

####with depth##

aerobSRdata2<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/stepwise_withdepth_data.txt") 
aerobSRdata2$CH4<-NULL
aerobSRdata2$Anaerobic.CO2<-NULL

aerobSRdata2

#initial model
initial_model2 <- lm(Aerobic.CO2 ~ ., data = aerobSRdata2)

# Perform stepwise selection
stepwise_aerCO2_2 <- step(initial_model2, direction = "both") #to run backward elimination, set direction = "backward"

# Print the summary of the selected model
summary(stepwise_aerCO2_2)

##log GHG ##

aerobSRdata3<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/revisions_logGHG.txt") 
aerobSRdata3$CH4<-NULL
aerobSRdata3$Anaerobic.CO2<-NULL
aerobSRdata3$Aerobic.CO2<-NULL
aerobSRdata3$Anaerobic.CO2..log.<-NULL
aerobSRdata3$CH4..log.<-NULL
aerobSRdata3$Sample<- NULL

View(aerobSRdata3)


initial_model3 <- lm(Aerobic.CO2..log. ~ ., data = aerobSRdata3)

initial_model3

stepwise_aerCO2_3 <- step(initial_model3, direction = "both") #to run backward elimination, set direction = "backward"

summary(stepwise_aerCO2_3)

#-----------------------------------------------------------------------------------------------------------------
# Anaerobic CO2 

anaerobSRdata<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/pca3.20.24.txt") #this is my old PCA code
anaerobSRdata$CH4<-NULL
anaerobSRdata$Aerobic.CO2<-NULL

initial_modelana <- lm(Anaerobic.CO2 ~ ., data = anaerobSRdata)

stepwise_anaerCO2 <- step(initial_modelana, direction = "both")

summary(stepwise_anaerCO2)


##with depth##

anaerobSRdata2<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/stepwise_withdepth_data.txt") 
anaerobSRdata2$CH4<-NULL
anaerobSRdata2$Aerobic.CO2<-NULL

initial_modelana2 <- lm(Anaerobic.CO2 ~ ., data = anaerobSRdata2)

stepwise_anaerCO2_2 <- step(initial_modelana2, direction = "both")   #to run backward elimination, set direction = "backward"

summary(stepwise_anaerCO2_2)

##log GHG ##

anaerobSRdata3<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/revisions_logGHG.txt") 
anaerobSRdata3$CH4<-NULL
anaerobSRdata3$Aerobic.CO2<-NULL
anaerobSRdata3$Anaerobic.CO2<- NULL
anaerobSRdata3$CH4..log.<-NULL
anaerobSRdata3$Aerobic.CO2..log.<-NULL
anaerobSRdata3$Sample<-NULL


initial_modelana3 <- lm(Anaerobic.CO2..log. ~ ., data = anaerobSRdata3)

stepwise_anaerCO2_3 <- step(initial_modelana3, direction = "both")   #to run backward elimination, set direction = "backward"

summary(stepwise_anaerCO2_3)

#-----------------------------------------------------------------------------------------------------------------
# CH4 

CH4SRdata<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/pca3.20.24.txt") #this is my old PCA code
CH4SRdata$Anaerobic.CO2<-NULL
CH4SRdata$Aerobic.CO2<-NULL

initial_modelCH4 <- lm(CH4 ~ ., data = CH4SRdata)

stepwise_CH4 <- step(initial_modelCH4, direction = "both")

summary(stepwise_CH4)


##with depth##

CH4SRdata2<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/stepwise_withdepth_data.txt")
CH4SRdata2$Anaerobic.CO2<-NULL
CH4SRdata2$Aerobic.CO2<-NULL

initial_modelCH4_2 <- lm(CH4 ~ ., data = CH4SRdata2)

stepwise_CH4_2 <- step(initial_modelCH4_2, direction = "both")  #to run backward elimination, set direction = "backward"

summary(stepwise_CH4_2)

##with log GHG##

CH4SRdata3<-read.delim("C:/Users/rache/Downloads/work stuff/research/Philben/revisions_logGHG_CH4.txt")
CH4SRdata3$Anaerobic.CO2..log.<-NULL
CH4SRdata3$Aerobic.CO2..log.<-NULL
CH4SRdata3$Sample<-NULL

View(CH4SRdata3)

initial_modelCH4_3 <- lm(CH4..log. ~ ., data = CH4SRdata3)

stepwise_CH4_3 <- step(initial_modelCH4_3, direction = "both")  #to run backward elimination, set direction = "backward"

summary(stepwise_CH4_3)

