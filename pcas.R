totalPCA1<-read.delim("C:/Users/rache/Downloads/School/research/totalPCA2.txt")
View(totalPCA2)
analysis3<-prcomp(totalPCA2, center=TRUE, scale=TRUE)
analysis3
biplot(analysis3)