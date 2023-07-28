library(dplyr)
library(car)
library(ggplot2)
library(tidyverse)
rm(list=ls())


bank_data <- read.csv(file.choose())
str(bank_data)



bank_data1 <-subset(bank_data,marital == "married"|marital =="single")
bank_data2 <-subset(bank_data1,select=c(marital,age,housing))
is.na(bank_data2)
ggplot(bank_data2,aes(y=housing,x=age)) +geom_jitter(width=0.2,height=0.2)+
  facet_grid(~marital)
  
bank_data2$housing <- as.factor(bank_data2$housing)
model1 <-glm(housing ~ marital,data=bank_data2,family = binomial())
summary(model1)
model2 <-glm(housing ~ marital+age,data=bank_data2,
             family = binomial())

summary(model2)
# test for multicollinearity
vif(model2)
1/vif(model2)


# test for linearity
bank_data2$logage <-log(bank_data2$age)*bank_data2$age
bank_data2 <- bank_data2 %>% drop_na(logbalance)
test1 <- glm(housing~marital+age+logage,data=bank_data2,
             family =binomial() )
summary(test1)
#independence of errors
plot(model1)
plot(model2)

exp(confint(model1))
exp(confint(model2))
