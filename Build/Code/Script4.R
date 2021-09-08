rm(list=ls())

library(tidyverse)

#Function####

DIFF<-function(x){
  New_Deaths<-x-lag(x)
  return(New_Deaths)
}

library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate( pc_cases = DIFF(Cases),
          pc_tests=DIFF(Tests),
          pc_deaths= DIFF(Deaths)
        
)
delta<-function(x){
  Percent_Change_New_Cases<-DIFF(Deaths)/lag(x)
  
  
  
  
}

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_deaths)

plot(covidIL$Date, covidIL$pc_deaths,
     main = "Daily Percent Change in Deaths",
     xlab = "",
     ylab = "",
     type = "l",
     col="orange")




