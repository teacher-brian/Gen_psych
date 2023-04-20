
####note that the roster work needs to capture last column for 'withdraw'



library(rvest)
library(tidyverse)
library(clipr)
library(lubridate)
email<- read_clip()  #from notify everyone email before sending
email1<- read_clip()  #from notify everyone email before sending
users<- read.csv(file="/media/brian/dater_bridge2/work/rosters/slack-spr23generalpsych-members.csv")
#roster.d1 <- read.csv("/media/brian/dater_bridge2/work/rosters/2023-4-20_GeneralPsychology_PSYC& 100 - H2 (24624).csv")

roster.h2 <- read.csv("/media/brian/dater_bridge2/work/rosters/2023-4-20_GeneralPsychology_PSYC& 100 - H2 (24624).csv")
roster.h2<- roster.h2[complete.cases(roster.h2$ID),]
roster <- rbind(#roster.d1,
                roster.h2)
rownames(roster) <- NULL

roster<- roster %>%
  filter(Status=='Enrolled') %>%
  separate(Name,into = c('last','first'),sep = ',')

email<- as.data.frame(strsplit(email,", "))
email1<- as.data.frame(strsplit(email1,", "))
colnames(email1) <- "email"
colnames(email ) <- "email"
email<- rbind(email,email1)
missing<- email%>% left_join(users) %>% select(email, fullname) %>% filter(is.na(fullname)) #missing from slack

gsub(x=paste(shQuote(missing$email), collapse = ", "),"'","" )%>% write_clip()

week1 <- read_clip()

week1.bak <- week1

#grep("[a]|\\s",week1,value = T)  %>% #romy gets filtered out
  grep("[a-z\\)]$|\\s[0-9]{1,2}:.*[AP]M$|joined",week1,value = T,ignore.case = T)  %>%
  grep("Google Doc|G Suite Document|repl|days|iew|http|rian|Only|one doc",
       .,invert = T,value = T)  %>%  as.data.frame() -> week_post

colnames(week_post) <- "data"

head(week_post)



posted <- week_post %>%
  mutate(time=dplyr::lead(data,1),joined=dplyr::lead(data,2)) %>%
  mutate(time= gsub("^[a-zA-Z]",NA,time)) %>%
  mutate(data= gsub("^[ 0-9]",NA,data)) %>%
  filter(grepl("joined",joined,ignore.case=T,)==F) %>%
  filter(grepl("joined",data,ignore.case=T,)==F) %>%
  select(data,time) %>% mutate(time=trimws(time)) %>% filter(grepl("^[0-9]",time)) %>% select(data) %>%

  unique(.) %>% arrange(data)

posted <- posted %>% filter(posted$data!='eden')
# joins users from slack to roster
users  %>% mutate_all(as.character) %>%
  mutate(displayname=ifelse(nchar(displayname)<1,fullname,displayname)) %>%
  anti_join(posted, by = c("displayname"="data")) %>%
  filter(status =="Member") %>% separate(fullname,into=c("first",'last')," ") %>%   left_join(roster) %>% arrange(last)


# does anti-join to find roster names not in week 1

users  %>% mutate_all(as.character) %>% mutate(displayname=ifelse(nchar(displayname)<1,fullname,displayname)) %>% anti_join(posted, by = c("displayname"="data")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  #select(email) %>%
  mutate(message= paste0("\n\n\nHi @",displayname,", This is generic message to all students who appear to not have posted their week 2 assignment.  Week 3 is ending, and I don't think you've posted to week 2. I might be wrong because I'm using a program to filter for people who haven't posted yet, but I don't think you've posted to week 2.  Do you need any help?  I know a few students are trying to get caught up, and a few of you have reached out. But at this point I'm a little worried you may fall behind.  It's not too late, but time keeps moving forward.  Reach out for help\n\n\n"))  %>% write_clip()



