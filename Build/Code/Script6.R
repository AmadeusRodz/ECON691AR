#ARscriptSeptember8th

rm(list=ls())

library(tidycensus)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)


covid<-GET("https://data.cdc.gov/resource/n8mc-b4w4.json")

covid.2<-fromJSON(rawToChar(covid$content))

#Generate data from Census API
#Pre-defining variables to be used in loop

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003","B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081" )

states<-c("kentucky","indiana","illinois","missouri","wisconsin","iowa") 
fips<-c(21,18,17,29,55,19)

#API Command
k<-1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars, 
               state = i,
               year  =  2016, 
               geometry  =  TRUE)
  
  temp<-acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  
  
  
  #assign(paste0(states[k],"census"),temp)
  ifelse(k==1,census<-temp,census<-rbind(census,temp))
  
  temp$area<-st_area(temp)
  map <- temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  #assign(paste0(states[k],"map"),map) 
  ifelse(k==1,MAP<-map,MAP<-rbind(MAP,map))
  
  k<-k+1
  rm(temp, map)
}



rm(fips, i, k, states, vars, acs)
save.image(file="./Build/Output/census.RData")





