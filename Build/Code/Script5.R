#thisisascriptforclass
#amadeus-sept1

rm(list=ls())

library(tidyverse)
library(rvest)

states<-c("illinois","indiana","kentucky","missouri","wisconsin","iowa")

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


