#This is a script
#Created by Amadeus R
#On August 25th, 2021

rm(list=ls())

library(tidyverse)

#Function####

delta<-function(x){
  temp<-((x-lag(x))/lag(x))
  return(temp)
}
  
library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)
  
  covidIL<-ILCovid19 %>%
    mutate(pc_test = delta(Tests), 
           pc_cases = delta(Cases),
           pc_deaths = delta(Deaths))
  
  covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA
    
  covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")
  
  plot(covidIL$Date, covidIL$pc_cases)
  
  plot(covidIL$Date, covidIL$pc_cases,
       main = "Percent Cases",
       xlab = "",
       ylab = "",
       type = "l",
       col="red")
  
  
  
  
   
  
  