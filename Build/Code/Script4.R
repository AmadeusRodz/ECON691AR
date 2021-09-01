rm(list=ls())

library(tidyverse)

#Function####

DIF<-function(x){
  Deaths<-x-lag(x)
  return(Deaths)
}

library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate( pc_cases = DIF(Cases),
          pc_tests=DIF(Tests),
          pc_deaths= DIF(Deaths)
        
)

covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_deaths)

plot(covidIL$Date, covidIL$pc_deaths,
     main = "Daily Percent Change in Deaths",
     xlab = "",
     ylab = "",
     type = "l",
     col="orange")




