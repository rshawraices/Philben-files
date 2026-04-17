#ref link: https://www.scribbr.com/statistics/anova-in-r/
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
install.packages(c("rtools"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
anaerobicCO2<-read.csv("C:/Users/rache/Downloads/Bioavailability_ANOVAs.csv",header = TRUE, colClasses = c("numeric", "factor","numeric","numeric","numeric","factor"))
summary(anaerobicCO2)
two.way <- aov(Anaerobic_CO2 ~ Depth+Microtopography, data = anaerobicCO2)
summary(two.way)
one.way.anadepth<-aov(Anaerobic_CO2 ~ Depth, data = anaerobicCO2)
one.way.anadepth
one.way.anamicro<-aov(Anaerobic_CO2 ~ Microtopography, data = anaerobicCO2)
one.way.anamicro
two.way.aerCO2 <- aov(Aerobic_CO2 ~ Depth*Microtopography, data = anaerobicCO2)
summary(two.way.aerCO2)
two.way.methane <- aov(CH4 ~ Depth*Microtopography, data = anaerobicCO2)
summary(two.way.methane)
?aov
# do a tukey's post-hoc test: https://fastercapital.com/content/Post-hoc-tests--Uncovering-Hidden-Patterns-in-Two-Way-ANOVA.html#How-to-Conduct-Post-hoc-Tests-in-Two-Way-ANOVA- #
#ref.2 https://stats.libretexts.org/Bookshelves/Applied_Statistics/Learning_Statistics_with_R_-_A_tutorial_for_Psychology_Students_and_other_Beginners_(Navarro)/14%3A_Comparing_Several_Means_(One-way_ANOVA)/14.06%3A_Multiple_Comparisons_and_Post_Hoc_Tests #
# ^ I think I used a YT video for the actual code#

# one below doesn't work, from when I was trying to do a bonferroni test #
pairwise.t.test(anaerobicCO2$Aerobic_CO2, anaerobicCO2$Microtopography, p.adjust.method = "bonferroni", paired=TRUE)
?pairwise.t.test
# the one below is actually what I want lol #
?TukeyHSD
TukeyHSD(two.way.aerCO2, which = "Microtopography")

