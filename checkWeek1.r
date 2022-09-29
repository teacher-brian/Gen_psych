library(rvest)
library(tidyverse)
library(clipr)
library(lubridate)
email<- read_clip()  #form notify everyone email before sending
email1<- read_clip()  #form notify everyone email before sending
users<- read.csv("slackUsers.csv")
roster <- read.csv("d1.csv")
roster %>% separate(Name,into = c('last','first'),sep = ',')

email<- as.data.frame(strsplit(email,", "))
email1<- as.data.frame(strsplit(email1,", "))
colnames(email1) <- "email"
colnames(email ) <- "email"
email<- rbind(email,email1)
missing<- email%>% left_join(users) %>% select(email, fullname) %>% filter(is.na(fullname))

gsub(x=paste(shQuote(missing$email), collapse = ", "),"'","" )%>% write_clip()

week1 <- read_clip()
week1

posted<- grep("[0-9]{1,2}:|joined",week1,value = T)  %>%
  as.data.frame(row.names = NULL) %>%
  separate (col=".",into=c("name","time"),sep = "  ") %>%
  mutate(check=lead(name)) %>%
  mutate(check=ifelse(grepl("joined",check,ignore.case=T),1,0)) %>%
  filter(!grepl('joined',name,ignore.case=T) ) %>%
  filter (check!=1) %>%
  select(name) %>%  unique(.)



users %>% anti_join(posted, by = c("displayname"="name")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  mutate(message= paste0("Hi ",displayname,", Week 2 is getting close to being half over, and I'm not seeing a post to week 1. I might be wrong because I'm using program to filter for people who haven't posted yet, but I don't think you've posted to week 1 yet.  Do you need any help?  The #logistics channel is available."))
