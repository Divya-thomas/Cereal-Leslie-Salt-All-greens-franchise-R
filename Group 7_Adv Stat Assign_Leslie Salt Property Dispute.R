setwd("C:\\Rwd")

install.packages("xlsx", dependencies = TRUE)
install.packages("car")

library(xlsx)
library(corrplot)
library(car)
library(corrplot)
library(psych)
library(lmtest)
library(zoo)

data <- read.xlsx("Dataset_LeslieSalt-1.xlsx",1)


#We have factor variables County and Flood in non-factor format. We transform them
data$County <- factor(data$County,  levels=c("0","1"), labels=c("San Mateo", "Santa Clara" ))
data$Flood <- factor(data$Flood,  levels=c("0","1"), labels=c("No", "Yes" ))


str(data[c("County","Flood")])

#We take a look at Price's spread using box plot
boxplot(data$Price)

#There is an outlier (a value of 37.20. This outlier could impact our model. Even if we remove this, we have sufficient sample data
data <- data[-26,]
boxplot(data$Price)

install.packages("corrplot")

#Let's take a look at how the variables are correlated with each other. We will omit the factor variables to do this because 
# it will be difficult to compare categorical and numeric data correlation.
dataMatrrix <- as.matrix(dplyr::select_if(data, is.numeric))
corrplot(cor(dataMatrrix), method = "circle",  type="full",order = "hclust", tl.col = "black")


#The correlation plot explains some good correlation between price and Elevation and Price and Date. 
#It indicates negative correlation between price and Size, Sewer

#Let's try to create a model with the entire dataset
#Model 1 trial

MyModel1 = lm(Price ~., data = data)
summary(MyModel1)

#Reg1 model has an adj R squared of 0.74%. Which indicates that considering all variables together explains about 74% of variance
#However, p-values of County1, Size, Sewer and Distance are >0.05. We will have to drop these variables and retry our model


#Model 2 trial
MyModel2 = lm(Price ~.-County-Size-Sewer-Distance, data = data)
summary(MyModel2)
#When we removed those variables the adj R squared reduced drastically to 0.63 compared to previous model.
#We will retry another model with some variables included

#Model 3 trial
MyModel3 = lm(Price ~.-Distance -Size, data = data)
MyModel3
summary(MyModel3)
#Adj R squared - 0.73 with most variables included except Size and Distance and p-values of all being <0.05
#Looks good. We go with this


#Characterestics of a well behaved residual vs Fit plot and their take aways

#The appropriateness of the linear model
#Closer the dots to the line better the model's ability to predict. We can't say that's largely appropriate here.
#But this seems to be the best model we have.

#We derive the following points:
# 1. Residuals seem to bounce randomly aroundly the 0 line causing 
#no set patterns (Typically like cone or U Shaped patterns)
#Suggests that assumption of linear relashionship is within means


#2. Points to form a horizontal band
# This shows that the variance of the error terms are more or less equal
#This shows the lack of violation of Homoscedesticity
# (Appropriate for alinear regression model)


#3. Also in the event any points are behaving as outliers, this charts shows it to us.
fit3 <- fitted(MyModel3)
res3 <- residuals(MyModel3)
propertyReg <- cbind(data, fit3, res3)
with(propertyReg, plot(fit3,res3, pch=19, cex=0.6))
abline(a=0,b=0)


#VIF - Variation inflation factor -as a thumb rule if the VIF is greater than 10 it suggests there is multi collinerity
# If the VIF is greater than 100 it certainly demands us to fix this as it could greatly affect results in regression
vif(MyModel3)
#VIF is less than 10 hence there is no multi collinearity

#Durbin Watson Test to test Auto Correlation
#DW ranges between 0 and 4. a Value of 2 indicates there is no Auto correlation.
#0 indicated positive auto correlation. 4 indicates negative auto correlation
#Thumb rule 1.5 to 2.5 safe to conclude no Auto correlation
dwtest(MyModel3)
#DW value 2 indicates there is no Auto correlation. Thumb rule 1.5 to 2.5 safe to conclude no Auto correlation

# Homoscedasticity tested using Goldfelt Quant test
#Null hypothesis : Data satisfies the condiction of homoscedasticity
#Alternate hypothesis states data is not Homoscedastic
gqtest(MyModel3)



#Problem Statement : The Leslie property contained 246.8 acres and was located right on the San Francisco Bay. 
#The land had been used for salt evaporation and had an elevation of exactly sea level. 
#However, the property was diked so that the waters from the bay park were kept out.
LeslieProperty <- data.frame(Price = 0, County = "Santa Clara", Size = 246.8, Elevation = 0, 
                             Sewer = 0, Date = c(1,2,3), Flood = "No" , Distance = 0)
LeslieProperty$PredictedPrice <- predict(MyModel3,LeslieProperty) * 1000

mydataset <- data.frame(LeslieProperty)
View(mydataset)

#