#This is a script
#Created by Amadeus R
#On August 25th, 2021

rm(list=ls())

library(tidyverse)

#Function####

DIF<-function(x){
  temp<-(x-lag(x))
  return(temp)
  
}

library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(new_tests=DIF(Tests), 
         new_cases=DIF(Cases),
         new_deaths=DIF(Deaths))

delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(temp)
}

covidIL<-covidIL %>%
  mutate(pc_tests=delta(new_tests),
         pc_deaths=delta(new_deaths),
         pc_cases=delta(new_cases))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")



plot(covidIL$Date, covidIL$pc_tests)

plot(covidIL$Date, covidIL$pc_tests,
     main = "Daily Percent Change in Tests",
     xlab = "",
     ylab = "",
     type = "l",
     col="blue")



