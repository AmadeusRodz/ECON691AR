#This is a script
#Created by Amadeus R
#On August 25th, 2021

rm(list=ls())

library(tidyverse)

#Function####

DIF<-function(x){
  Cases<-x-lag(x)
  return(Cases)
}


library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(pc_test = DIF(Tests), 
         pc_cases = DIF(Cases),
         pc_deaths = DIF(Deaths))

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_cases)

plot(covidIL$Date, covidIL$pc_cases,
     main = "Daily Percent Change in New Cases",
     xlab = "",
     ylab = "",
     type = "l",
     col="blue")
