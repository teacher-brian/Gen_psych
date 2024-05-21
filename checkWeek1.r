
####note that the roster work needs to capture last column for 'withdraw'



library(rvest)
library(tidyverse)
library(clipr)
library(lubridate)
email<- read_clip()  #from notify everyone email before sending
email1<- read_clip()  #from notify everyone email before sending
#users<- read.csv(file="/media/brian/dater_bridge2/work/rosters/slack-spr23generalpsych-members.csv")
users<- read.csv(file='slack-generalpsychs-xkn6199-members.csv')


#roster.d1 <- read.csv("/media/brian/dater_bridge2/work/rosters/2023-4-20_GeneralPsychology_PSYC& 100 - H2 (24624).csv")

roster.h2 <- readxl::read_xlsx("H-2.xlsx")
roster.h2 <- roster.h2[,-1]

roster.d1 <- readxl::read_xlsx("D-1.xlsx")
roster.d1 <- roster.d1[,-1]

roster.h2<- roster.h2[complete.cases(roster.h2$ID),]
roster.d1<- roster.d1[complete.cases(roster.d1$ID),]
roster.d1$`Status Note` <- NA

roster <- rbind(roster.d1,
                roster.h2)
rownames(roster) <- NULL


colnames(roster) <- c("id","Name","grade","units","program","level","Status")

roster<- roster %>%
  filter(is.na(Status)) %>%
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

#posted <- posted %>% filter(posted$data!='eden')
# joins users from slack to roster
users  %>% mutate_all(as.character) %>%
  mutate(displayname=ifelse(nchar(displayname)<1,fullname,displayname)) %>%
  anti_join(posted, by = c("displayname"="data")) %>%
  filter(status =="Member") %>% separate(fullname,into=c("first",'last')," ") %>%   left_join(roster) %>% arrange(last)


# does anti-join to find roster names not in week 1

users  %>% mutate_all(as.character) %>% mutate(displayname=ifelse(nchar(displayname)<1,fullname,displayname)) %>% anti_join(posted, by = c("displayname"="data")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  #select(email) %>%# mutate(email=paste0(email,";")) %>%
  mutate(message= paste0("\n\n\nHi @",displayname,", This is a message to all students who appear to not have posted their week 5 assignment.  Week 8 is half over and so it's time to seriously consider dropping the course.\n\n\nYes, that is correct.  It's time to drop the course.\n\n The reason is that this Friday, May 24, is the last day for you to officially withdraw.  A 'W' on your transcript does not impact your gpa. \n\n\n  But because you are so far behind, it is unreasonable to expect that you'll get feedback from me quickly enough to help your work get to a level that would qualify as passing for the course.\n\nIn other words, it is very likely that you will not get a passing grade.\n\n\n"))



