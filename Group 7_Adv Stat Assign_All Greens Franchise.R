##Upload the csv file into R
data2 <- read.csv(file.choose())
attach(data2)

#uploading the library
library(psych)
library(lmtest)
library(zoo)
library(corrplot)
library(car)

##Study the structure of data
str(data2)
head(data2)
tail(data2)
summary(data2)

#Multi variate normality
par(mfrow=c(3,2))
qqnorm(X1,pch=1,frame=FALSE)
qqline(X1,col="steelblue",lwd=2)
qqnorm(X2,pch=1,frame=FALSE)
qqline(X2,col="steelblue",lwd=2)
qqnorm(X3,pch=1,frame=FALSE)
qqline(X3,col="steelblue",lwd=2)
qqnorm(X4,pch=1,frame=FALSE)
qqline(X4,col="steelblue",lwd=2)
qqnorm(X5,pch=1,frame=FALSE)
qqline(X5,col="steelblue",lwd=2)
qqnorm(X6,pch=1,frame=FALSE)
qqline(X6,col="steelblue",lwd=2)

#Autocorrelation rule
#Durbin watson Rule
SLM=lm(X1~X2+X3+X4+X5+X6,data=data2)
dwtest(SLM)
#Homoscedasticity
gqtest(SLM)
#Multi-collinearity:
cor(data2)

corrplot(cor(data2))

#regression analysis of complete dataset
reg1 <- lm(X1 ~ ., data= data2)
summary(reg1)

#VIF
#install.packages("car")

vif(reg1)


##removal of redundant variable to perform regression:
reg2 <- lm(X1 ~ .-X3, data= data2)
summary(reg2)

#VIF(2):Checking VIF for the reg2
vif(reg2)


#Prediction

Prediction=predict(reg1)
Actual=X1
plot(Actual,col="red",xlab="Data point")
lines(Actual,col="red")
plot(Prediction,col="blue",xlab="Data point")
lines(Prediction,col="blue")
lines(Actual,col="red")


#Prediction with model 2
Prediction=predict(reg2)
Actual=X1
plot(Actual,col="red",xlab="Data point")
lines(Actual,col="red")
plot(Prediction,col="blue",xlab="Data point")
lines(Prediction,col="blue")
lines(Actual,col="red")