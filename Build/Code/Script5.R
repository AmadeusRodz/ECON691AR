#thisisascriptforclass
#amadeus-sept1

rm(list=ls())

library(tidyverse)
library(rvest)

states<-c("illinois","indiana","kansas","missouri","arkansas","iowa")

for(i in states){

url<-paste0("https://www.nytimes.com/elections/2016/results/", i)


webpage<-read_html(url)

tables<-webpage %>%
  html_nodes("table")


results<-as.data.frame(html_table(tables[2], header=TRUE, fill=TRUE))

#results2<-tables[2] %>%
  #html_table(fill=TRUE, header=TRUE) %>%
  #as.data.frame()


 temp<-results %>%
  rename("County"="Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",","",Clinton)),
       "Trump" = as.numeric(gsub(",","",Trump)),
       "pctClinton"= ((Clinton)/(Clinton + Trump)),
       "pctTrump"= (Trump)/(Clinton + Trump),
       "State"= i)


assign(i,temp)

}

votes<-rbind(illinois,iowa,missouri,kansas,arkansas)


save(votes, file="./Build/Output/votes.RData")
save.image()




#APIdata
library(tidycensus)

census_api_key("1b57247c194141b3222ff8b855ba395161a4dde5",install=TRUE )

data<-load_variables(2016, "acs5")

acs <- get_acs(geography = "county",
             variables= vars,
             state=17,
             year=2016,
             geometry=TRUE
             )

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", 
        "B02001_003","B05001_001","B05001_006","B07001_001", 
        "B07001_017","B07001_033","B07001_049","B07001_065","B07001_081")

il.acs<-acs %>%
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
  select("GEOID","NAME",starts_with("per"),"geometry")



