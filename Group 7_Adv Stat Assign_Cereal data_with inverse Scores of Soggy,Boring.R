setwd("C:\\Maneesh_29 Jul 2018\\0008 - Business Analytics\\03 - BABI course\\03 - Advanced Stats\\Group Assignment")

cerealdata <- read.csv("Dataset_Cereal.csv")
str(cerealdata)
dim(cerealdata)
summary (cerealdata)

cerealdata [cerealdata==6] <- 5
summary(cerealdata)

cerealdata[,c(12,25)] <- 6 - cerealdata[,c(12,25)]
summary(cerealdata)

library(nFactors)
library(psych)
library(corrplot)
library(car)



##Create a matrix out of the dataframe in order to be used in the Model
cerealdata2 <- as.matrix(cerealdata[, 2:26])
cerealdata2

cor(cerealdata2)
corrplot(cor(cerealdata2))

#Eigen values formula being ("Correlation r") minus (Lambda*I)
# get eigenvalues
ev <- eigen(cor(cerealdata2))
ev
print(ev,digits=3)
eigenvalues <- ev$values
eigenvalues

nS <- nScree(x=eigenvalues)
plotnScree(nS)

##Run factor analysis model with number of factors limit to 5 and no rotation
# Specify the number of factors to be extracted
Factor <- factanal(~cerealdata2, 5, rotation="none")
Factor
print(Factor, digits=2, cutoff= 0.30, sort= TRUE)

Factorloadings <- Factor$loadings[,1:5]
Factorloadings


##Run factor analysis model with number of factors limit to 4 and no rotation
# Specify the number of factors to be extracted
# varimax and promax rotations are possible
Factor <- factanal(~cerealdata2, 4, rotation="none") 
Factor
print(Factor, digits=2, cutoff= 0.28, sort= TRUE)


# ##Run factanal again with varimax rotation just for fun
Factor1 <- factanal(~cerealdata2, 4, rotation="varimax")
Factor1

print(Factor1, digits=2, cutoff= 0.28, sort= TRUE)

Factorloadings <- Factor1$loadings[,1:4]
Factorloadings

# plot(Factorloadings, type= "n")
# text(Factorloadings, labels=names(cerealdata2), Factorloadings=Factor1$loadings[,1:4])

F1 <- cerealdata2[,c("Filling", "Natural","Fibre", "Satisfying", "Energy", "Health", "Regular","Quality", "Nutritious")]
F2 <- cerealdata2[,c("Sweet", "Salt","Calories", "Sugar", "Process")]
F3 <- cerealdata2[,c("Fun", "Plain","Treat", "Boring", "Soggy", "Crisp", "Fruit")]
F4 <- cerealdata2[,c("Kids", "Family","Easy", "Economical")]

F1
F2
F3
F4

cerealdata$F1.Score <- apply(F1,1,mean)
cerealdata$F2.Score <- apply(F2,1,mean)
cerealdata$F3.Score <- apply(F3,1,mean)
cerealdata$F4.Score <- apply(F4,1,mean)

View(cerealdata)

colnames(cerealdata)[27:30] <- c("Health&Quality", "Process&Taste", "ExcitementLevel", "Economical&Availability")
cerealdata_factor <- cerealdata[,c("Cereals","Health&Quality", "Process&Taste", "ExcitementLevel", "Economical&Availability")]
View(cerealdata_factor)

str(cerealdata_factor)


Pivotcerealdata <- aggregate(cerealdata_factor[,2:5], list(cerealdata_factor[,1]),mean)
format(Pivotcerealdata,digits = 2)
str(Pivotcerealdata)
View(Pivotcerealdata)


library(ggplot2)
boxplot(as.matrix(Pivotcerealdata[,2:5]))


plot1 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Health&Quality`,fill=Group.1))+geom_boxplot()
plot1
plot2 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Process&Tase`,fill=Group.1))+geom_boxplot()
plot2
plot3 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$TextureOFFood,fill=Group.1))+geom_boxplot()
plot3
plot4 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Economical&Availability`,fill=Group.1))+geom_boxplot()
plot4

plot1 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Health&Quality`))+geom_boxplot()
plot1


plot2 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Process&Tase`))+geom_boxplot()
plot2
plot3 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$ExcitementLevel))+geom_boxplot()
plot3
plot4 <- ggplot(Pivotcerealdata, aes(x=Group.1, y= Pivotcerealdata$`Economical&Availability`))+geom_boxplot()
plot4





