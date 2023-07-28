rm(list=ls())
library(tidyverse)
library(dplyr)
knitr::opts_chunk$set(echo = TRUE)
mydata <- read.csv("/Users/loufuhao/Desktop/salaries_clean.csv")

mydata <- replace(mydata,is.na(mydata),0)
mydata <-replace(mydata,mydata=='',0)
mydata4 <- mutate(mydata,totalmoney = annual_base_pay+signing_bonus+
                    annual_bonus)

mydata1 <- select(mydata4,
                  location_name,
                  job_title_category,
                  total_experience_years,
                  totalmoney)

mydata2 <-filter(mydata1,location_name %in% c('seattle') &
                   job_title_category =='Software' &
                   total_experience_years >=0)

mydata3 <-filter(mydata1,location_name %in% 
                   c('san francisco') &
                   job_title_category =='Software' &
                   total_experience_years >=0)

mydata3 <- mydata3[-(41:73),]

mydata5 <-filter(mydata1,location_name %in% c('seattle') &
                   job_title_category =='Engineering' &
                   total_experience_years >=0)

mydata6 <-filter(mydata1,location_name %in% 
                   c('san francisco') &
                   job_title_category =='Engineering' &
                   total_experience_years >=0)
mydata6 <- mydata6[-(8:17),]

summary(mydata2)
summary(mydata3)



ggplot() + 
  geom_point(data=mydata2, aes(x=total_experience_years, y=totalmoney),
             color='green') + 
  geom_point(data=mydata3, aes(x=total_experience_years, y=totalmoney), 
             color='red')    
ggplot() + 
  geom_point(data=mydata5, aes(x=total_experience_years, y=totalmoney),
             color='green') + 
  geom_point(data=mydata6, aes(x=total_experience_years, y=totalmoney), 
             color='red')  
mydata7 <- mydata2[-(8:58),]
ggplot() + 
  geom_point(data=mydata5, aes(x=total_experience_years, y=totalmoney),
             color='green') + 
  geom_point(data=mydata7, aes(x=total_experience_years, y=totalmoney), 
             color='red')  
mydata8 <-mydata3[-(8:58),]
ggplot() + 
  geom_point(data=mydata8, aes(x=total_experience_years, y=totalmoney),
             color='green') + 
  geom_point(data=mydata6, aes(x=total_experience_years, y=totalmoney), 
             color='red')  
summary(mydata2)

total <- rbind(mydata2,mydata3)

total_summary <- total %>%
  group_by(location_name) %>%
  summarise(observations=n(),avg = mean(totalmoney),se=sd(totalmoney)
            /sqrt(n()))

ggplot(total_summary,aes(x=avg,y=location_name)) +
  geom_point()+
  geom_label(aes(label=avg),nudge_y = 0.1)+
  geom_errorbarh(aes(xmax=avg+1.96*se,xmin=avg-1.96*se))+
  labs(x="Avg Total Money",y="",title="CI for total money wrt Software")+
  theme_bw()

total1 <- rbind(mydata5,mydata6)
total1<- total1[-(15:20),]

total_summary1 <- total1 %>%
  group_by(location_name) %>%
  summarize(avg1=mean(totalmoney), se=sd(totalmoney)/sqrt(n()), 
            t=qt(0.975, n()-1))
ggplot(total_summary1, aes(x=avg1, y=location_name))+
  geom_point() + 
  geom_errorbarh(aes(xmin=avg1-se*t, xmax=avg1+se*t))+
  geom_label(aes(label=round(avg1,3), nudge_y = 0.1)) + 
  labs(x='Avg Total Money', y="", title="Avg Total Money wrt Engineering")+
  theme_bw()
