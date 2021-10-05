rm(list=ls())

library(tidyverse)
library(rvest)

#createloop

states<-c("illinois","kansas","missouri","arkansas","iowa")
fips<-c(17, 20, 29, 05, 19)

for(i in states){
  URL<-paste0("https://www.nytimes.com/elections/2016/results/", i)
  webpage<-read_html(URL)
  tables<-webpage %>%
    html_nodes("table")
  
  results<-as.data.frame(html_table(tables[2],header=TRUE,fill=TRUE))
  temp<-results %>%
    rename("County"="Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton"= Clinton/(Clinton + Trump),
           "pctTrump"= Trump/(Clinton + Trump),
           "State"= i)
  
  assign(i,temp)
}

votes<-rbind(illinois,iowa,missouri,kansas,arkansas)

#API data

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")
k<-1

for(i in fips){
  acs <- get_acs(geography = "county",
                 variables = vars,
                 state= i,
                 year= 2016,
                 geometry = TRUE)
  
  temp<-acs %>%
    mutate(variable2=case_when(variable=="B01001_001" ~ "TotPop",
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
    select("GEOID","NAME",starts_with("per"),"geometry")%>%
    mutate(state = states[k])
  
    ifelse(k==1, census1<-temp, census1<-rbind(census1, temp))
    
    temp$area<-st_area(temp)
    map<-temp %>%
      summarise(area = sum(area))%>%
      mutate(state = states[k])
    
    ifelse(k==1, MAP1<-map, MAP1<-rbind(MAP1, map))
    k<-k+1
    rm(temp, map)
}

k<-1

for(i in fips){
  acs <- get_acs(geography = "county",
                 variables = vars,
                 state= i,
                 year= 2019,
                 geometry = TRUE)
  
  temp<-acs %>%
    mutate(variable2=case_when(variable=="B01001_001" ~ "TotPop",
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
    select("GEOID","NAME",starts_with("per"),"geometry")%>%
    mutate(state = states[k])
  
  ifelse(k==1, census2<-temp, census2<-rbind(census2, temp))
  
  temp$area<-st_area(temp)
  map<-temp %>%
    summarise(area = sum(area))%>%
    mutate(state = states[k])
  
  ifelse(k==1, MAP2<-map, MAP2<-rbind(MAP2, map))
  k<-k+1
  rm(temp, map)
}

census3<-cbind(census1, census2)

census3<-census3 %>%
  mutate(new.perMale =  census2$perMale - census1$perMale,
         new.perWhite = census2$perWhite - census1$perWhite,
         new.perBlack = census2$perBlack - census1$perBlack,
         new.perCit = census2$perBlack - census1$perCit,
         new.perStay = census2$perStay - census1$perStay,
         new.perSameCounty = census2$perSameCounty - census1$perSameCounty,
         new.perSameSt = census2$perSameSt - census1$perSameSt,
         new.perOthState = census2$perOthState - census1$perOthState,
         new.perAbroad = census2$perAbroad - census1$perAbroad)

library(ggplot2)

census3<-census3 %>%
  mutate(County = as.data.frame(str_split_fixed(NAME, ",",2))[,1],
         state = as.data.frame(str_split_fixed(NAME, ",",2)) [,2],
         county = trimws(gsub("County","", County)))

ia.acs<-census3 %>%
  subset(state="Iowa")


ia.acs<-ia.acs[order(ia.acs$NAME),]
iowa<-iowa[order(iowa$County),]

ia.acs<-merge(ia.acs, iowa, by="County", all = TRUE)


library(cowplot)
p1<-ggplot(ia.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="red",high="blue",limits=c(0,1),aes(name="Percent
Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())


p2<-ggplot(ia.acs)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="red",high="white",limits=c(0,1),aes(name="Percent
White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
plot_grid(p1,p2) 

