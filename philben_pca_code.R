totalPCA1<-read.delim("C:/Users/rache/Downloads/School/research/pca3.20.24.txt")
View(totalPCA1)
analysis<-prcomp(totalPCA1, center=TRUE, scale=TRUE)
analysis
biplot(analysis)
?prcomp
