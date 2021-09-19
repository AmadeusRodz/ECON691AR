rm(list=ls())

library(tidyverse)
library(rvest)

#createloop

states<-c("illinois","kansas","missouri","arkansas","iowa")

for(i in states){
  URL<-paste0("https://www.nytimes.com/elections/2016/results/", i)
  webpage<-read_html(URL)
  tables<-webpage %>%
    html_nodes("table")
  
  results<-as.data.frame(html_table(tables[2],header=TRUE,fill=TRUE))
  
  results2<-tables[2] %>%
    html_table(fill=TRUE, header=TRUE) %>%
    as.data.frame()
  head(results)
  
  temp<-results %>%
    rename("County"="Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
           "Trump" = as.numeric(gsub(",","",Trump)),
           "pctClinton"= Clinton/(Clinton + Trump),
           "pctTrump"= Trump/(Clinton + Trump),
           "State"= i)
  
  assign(i,temp)
}

rm(i, URL, states, tables, results, webpage, results2, temp)

votes<-rbind(illinois,iowa,missouri,kansas,arkansas)

save(votes,file="./ECON691AR/Build/Output/votes.RData")

save.image()

#APIdata

library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

data<-load_variables(2016, "acs5")
acs <- get_acs(geography = "county",
               variables = vars,
               state= 17,
               year= 2016,
               geometry = TRUE)

census1<-acs %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

save(census1 ,file="./ECON691AR/Build/Output/census1.RData")

library(tidycensus)

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

data<-load_variables(2019, "acs5")
acs <- get_acs(geography = "county",
               variables = vars,
               state= 17,
               year= 2019,
               geometry = TRUE)

census2<-acs %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")

save(census2,file="./ECON691AR/Build/Output/census2.RData")

rm(list=ls())

library(tidyverse)

load("./Build/Output/census1.RData") 
load("./Build/Output/census2.RData")  

census1_1<-census1 %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)))

#st.Louis counties for fips 29 missouri

census1$County[which(census1$County=="DeWitt")]<-"De Witt"
census1$County[which(census1$County=="JoDaviess")]<-"Jo Daviess" 
census1$County[which(census1$County=="LaClede")]<-"Laclede" 
census1$County[which(census1$County=="LaRue")]<-"Larue" 
census1$County[which(census1$County=="St. Louis City")]<-"St. Louis city" 
census1$county[which(census1$county=="St. Louis")]<-"St. Louis County"

census2_2<-census2 %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)))

census2$County[which(census2$County=="DeWitt")]<-"De Witt"
census2$County[which(census2$County=="JoDaviess")]<-"Jo Daviess" 
census2$County[which(census2$County=="LaClede")]<-"Laclede" 
census2$County[which(census2$County=="LaRue")]<-"Larue" 
census2$County[which(census2$County=="St. Louis City")]<-"St. Louis city" 
census2$county[which(census2$county=="St. Louis")]<-"St. Louis County"

core<-merge(census1_1, census2_2, by.x=c("state", "county"), by.y=c("State", "County"),all=TRUE)







library(ggplot2)
ggplot(il.acs) +
  geom_sf(aes(fill = perMale))

il.acs$num<-seq(1:102)
ggplot(il.acs, aes(x=num))+
  geom_line(aes(y=perMale), color="red")+
  geom_point(aes(y=perWhite), color="blue")

ggplot(il.acs, aes(x=num))+
  geom_line(aes(y=perMale, color="red"))+
  geom_point(aes(y=perWhite, color="blue"))+
  xlab("County")+
  ylab("Percentage")+
  ggtitle("Percent of Population by County in Illinois")+
  scale_color_manual(name="Percent of",
                     breaks = c("red","blue"),
                     values = c("red","blue"),
                     labels = c("Male","White"))

ggplot(il.acs, aes(x=num))+
  geom_line(aes(y=perMale, color="red"))+
  geom_point(aes(y=perWhite, color="blue"))+
  xlab("County")+
  ylab("Percentage")+
  ggtitle("Percent of Population by County in Illinois")+
  scale_color_manual(name="Percent of",
                     breaks = c("red","blue"),
                     values = c("red","blue"),
                     labels = c("Male","White"))


il.acs$County<-trimws(gsub("County, Illinois","",il.acs$Name))

il.acs<-il.acs[order(acs3$County),]

illinois<-illinois[order(illinois$County),]

il.acs$County==illinois$County

il.acs$County[19:20]
illinois$County[19:20]
il.acs$County[19]<-illinois$County[20]
il.acs$County[43]<-illinois$County[43]

Reduce(function(x, y) merge(x, y, all=TRUE), list(df1, df2, df3))

il.acs<-merge(il.acs,illinois,by="County",all=TRUE)
b<-il.acs[is.na(il.acs$perMale),]

ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton"))


library(cowplot)
p1<-ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent
Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p2<-ggplot(il.acs)+
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent
White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
plot_grid(p1,p2) 