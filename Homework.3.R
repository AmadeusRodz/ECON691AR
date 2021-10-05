rm(list=ls())

library(tidyverse)
library(tidycensus)
library(rvest)
library(ggplot2)
library(sf)

delta<-function(x,y){
  temp<-(y-(x+.000001))/(x+.000001)
  return(temp)
}

st<-c("ILLINOIS","KANSAS","MISSOURI","ARKANSAS","IOWA")
states<-c("illinois","kansas","missouri","arkansas","iowa")
fips<-c(17, 20, 29, 05, 19)

countypres_2000.2020 <- read.csv("~/Downloads/dataverse_files/countypres_2000-2020.csv")

cp20<-filter(countypres_2000.2020, year=="2020", state %in% st)
cp16<-filter(countypres_2000.2020, year=="2016", state %in% st)

D_VOTES<-merge(cp20, cp16, by.x = c("state", "county_name"), by.y =c("state", "county_name") )

D_VOTES<-D_VOTES %>%
  mutate(pcChange = delta(D_VOTES$candidatevotes.x, D_VOTES$candidatevotes.y))
  
D_VOTES$county_name[which(D_VOTES$county_name=="ST. LOUIS")]<-"ST. LOUIS COUNTY"

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

census2$NAME<-as.data.frame(str_split_fixed(census2$NAME, ",", 2))[,1]
census2$NAME<-trimws(gsub(" County","",census2$NAME))

census2<-census2 %>%
  mutate(county_name = as.data.frame(str_split_fixed(NAME , ",", 2))[,1],
        county_name = trimws(gsub(" County","", county_name)))

census2$county_name<-toupper(census2$county_name)
census2$state<-toupper(census2$state)


census2$NAME<-toupper(census2$NAME)

MapDTA<-merge(census2, D_VOTES, by.x = c("NAME","state"), by.y = c("county_name","state"), all=TRUE)
MapDTA<-filter(MapDTA, !(NAME=="KANSAS CITY"), all=TRUE)
  
MapRep1<-filter(MapDTA, party.y=="REPUBLICAN", party.x=="REPUBLICAN")

MapDem1<-filter(MapDTA, party.y=="DEMOCRAT", party.x=="DEMOCRAT")


library(ggplot2)

library(cowplot)
p1<-ggplot(MapRep1)+
  geom_sf(aes(fill = pcChange))+
  scale_fill_gradient(low="white",high="red",limits=c(-100,100),aes(name="Percent Rep"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
       axis.text.y=element_blank(),
       axis.line = element_blank(),
        axis.ticks = element_blank())
   


p2<-ggplot(MapDem1)+
  geom_sf(aes(fill = pcChange))+
  scale_fill_gradient(low="white",high="blue",limits=c(-100,100),aes(name="Percent Dem"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
     
plot_grid(p1,p2) 


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

census3<-census3 %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
                  county = trimws(gsub(" County","", county)))
         
census3$state<-toupper(census3$state)
census3$county<-toupper(census3$county)

core<-merge(census3, D_VOTES, by.x = c("state", "county"), by.y = c("state", "county_name"), all = TRUE)

core$area<-st_area(core) #Command for maps in ggplot later

Reg.Rep<-filter(core, party.y=="REPUBLICAN", party.x=="REPUBLICAN")
Reg.Dem<-filter(core, party.y=="DEMOCRAT", party.x=="DEMOCRAT")


mod1<-lm(pcChange~perWhite+perMale, data=Reg.Rep)

mod2<-lm(pcChange~perWhite+perMale, data=Reg.Dem)

mod3<-lm(pcChange~new.perWhite+new.perMale, data=Reg.Rep)

mod4<-lm(pcChange~new.perWhite+new.perMale, data=Reg.Dem)

mod5<-lm(pcChange~new.perWhite+new.perMale-1, data=Reg.Rep)

mod6<-lm(pcChange~new.perWhite+new.perMale-1, data=Reg.Dem)


library(stargazer)

stargazer(core, type="latex", out="./Build/Output/SumStat.latex")
stargazer(mod1, mod2, type="latex", out="./Build/Output/regress.latex")
stargazer(mod3, mod4, type="latex", out="./Build/Output/regress1.latex")
stargazer(mod5, mod6, type="latex", out="./Build/Output/regress2.latex")
