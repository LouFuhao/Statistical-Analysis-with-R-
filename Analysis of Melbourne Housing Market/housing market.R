rm(list=ls())
install.packages("caTools")
library(caTools)
install.packages('car')
library(car)
install.packages("qqplotr")
library(qqplotr)
install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)
library(tidyverse)
library(dplyr)
install.packages("MASS")
library(MASS)

melbdata <- read.csv(file.choose())
melbdata <-na.omit(melbdata)
str(melbdata$Type)
#transfore categorical variable Type, preparing for regression
melbdata$Type <- as.factor(melbdata$Type)
contrasts(melbdata$Type) <- contr.treatment(3,base = 3)
melbdata$Type
h_v_u <- c(1,0,0)
t_v_u <-c(0,1,0)
contrasts(melbdata$Type) <-cbind(h_v_u,t_v_u)

#the reason to remove Bedroom2 and Rooms (index)
detectvariables <-lm(Price~Type + Rooms + Distance + Bedroom2 + Bathroom + Car+ Landsize 
            + BuildingArea,data=melbdata)

model2$coefficients
plot(detectvariables)
dwt(detectvariables)
vif(detectvariables)

#select variables, excluding bedroom2 and rooms
# (type,distance,bathroom,car,landsize,building area)

# before using hierarchical selection method (assumption check)


allvariables<-lm(Price~Type + Distance + Bathroom + Car+ Landsize 
           + BuildingArea,data=melbdata)
#all predictor variables must be quantitative or categorical yes.
dwt(allvariables) #independent
#multicollinearity
vif(allvariables)
1/vif(allvariables)
mean(vif(allvaribales))

melbdata$studentized.residuals<-rstudent(allvariabels)
hist(melbdata$studentized.residuals)
plot(allvariables) #(diagnosing problems homoscedasticity + linearity)
#normal qq (normality) #fitted values (linearity)
nvctest(allvariables)
str(melbdata)
melbdata$standardized.residuals <- rstandard(allvariables)
melbdata
possibleoutliers <- subset(melbdata,melbdata$standatdized.residuals >-1.96 &
                     melbdata$standardized.residuals <1.96)
# 231/6830 is about 3 percent. We don't need to worry too much 
#about possible outliers.


melbdata$cooks <- cooks.distance(allvariables)
plot(sort(melbdata$cooks,decreasing=TRUE))
max(melbdata$cooks)
# From the graph we can see that, the influential cases are really few. And 
# this could also be solved by adding new corrleated variables, so I don't 
# want to remove it.



myvars <-c("Price","Type","Landsize","BuildingArea","Bathroom","Car","Distance")
newdata<-melbdata[myvars]


basemodel <-lm(Price ~ Type+Distance+Landsize,data=newdata)
plot(basemodel)
summary(model6) #R^2 0.2564
#base
testmodel1<-lm(Price~Type + Distance + Landsize
                + Bathroom,data=newdata) #R^2 0.4256
plot(testmodel1)
summary(testmodel1)

testmodel2<-lm(Price~Type + Distance + Landsize
           + Bathroom+Car,data=newdata)
plot(testmodel2)
summary(testmodel2) #R^2 0.4334

anova(basemodel,testmodel1,testmodel2)
