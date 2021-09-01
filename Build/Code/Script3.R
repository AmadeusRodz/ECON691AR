rm(list=ls())

library(tidyverse)

#Function####

DIF<-function(x){
  Tests<-x-lag(x)
  return(Tests)
}

library(readr)
ILCovid19 <- read_csv("~/Downloads/ILCovid19(1).csv", show_col_types = FALSE)

covidIL<-ILCovid19 %>%
  mutate(pc_test= DIF(Tests),
         pc_cases= DIF(Cases),
         pc_deaths= DIF(Deaths)

)


covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)]<-NA

covidIL$Date<-as.Date(covidIL$Date, format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_test)

plot(covidIL$Date, covidIL$pc_test,
     main = "Daily Percent Change in Tests",
     xlab = "",
     ylab = "",
     type = "l",
     col="green")




