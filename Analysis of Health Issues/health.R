rm(list=ls())
dataset<-read.csv("/Users/loufuhao/Desktop/Data.csv")
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages("ggcorrplot")
library(ggcorrplot)
install.packages("ggmosaic")
library(ggmosaic)
library(ggm)
library(boot)
knitr::opts_chunk$set(echo = TRUE)
head(dataset)
str(dataset)
summary(dataset)
variablesselected <- select(dataset,part,health,stud_h)
summary(variablesselected)
health1 <- as.factor(variablesselected$health)
part1 <- as.factor(variablesselected$part)
str(variablesselected)

ggplot(data=variablesselected) +
  geom_mosaic(aes(x=product(health1),fill=health1)) +
  labs(title='health')


ggplot(data=variablesselected) +
  geom_mosaic(aes(x=product(part1,health1),fill=health1)) +
  labs(title='part | helath')



ggplot(variablesselected,aes(health1,stud_h,color=health1)) +
  geom_jitter(height = 0) +
  ggtitle("Study vs Health")

cor(variablesselected$part,variablesselected$health,method="kendall")
cor(variablesselected$stud_h,variablesselected$health,method="spearman")

pc <- pcor(c("stud_h","health","part"),var(variablesselected))
           
pcor.test(pc,1,886)

bootTau <-function(variablesselected,i)cor(variablesselected$health[i],
                                           variablesselected$stud_h[i],
                                           method="kendall")
boot_kendall <-boot(variablesselected,bootTau,886)
boot_kendall
boot.ci(boot_kendall)
plot(boot_kendall)
