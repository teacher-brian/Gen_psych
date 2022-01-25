

library(tidyverse)
library(lubridate)
library(googlesheets4)
library(RSelenium)
library(rvest)
library(clipr)

s<- read_sheet('https://docs.google.com/spreadsheets/d/1skEgkolZpr4r_iMaBM0qMxtuNAKATgyB0X2QDSpzor8/edit?resourcekey#gid=271104061')


(p<- s[!(duplicated(s$`What's your Last name?`) & duplicated(s$`What's your first name?`)),] %>%
    mutate(f_name=tools::toTitleCase(`What's your first name?`), l_name=tools::toTitleCase(`What's your Last name?`)) %>%
    select(f_name,l_name,Timestamp) %>%
    arrange(-desc(Timestamp)) %>% filter(Timestamp>"2022-01-01")) %>% arrange(l_name) %>%  print(.,n=100)


d1 <- read_clip()#11 columns
(length(d1)-1)/11
columns<- d1[1] %>% str_split(pattern = '\t') %>% unlist() %>% .[-1]

d1 <- d1[-1]

v2 <- read_clip()  #10 columns
(length(v2)-1)/10
columns.v2<- v2[1] %>% str_split(pattern = '\t') %>% unlist() %>% .[-1]

v2 <- v2[-1]

# maybe add a new colum with a repetatin number 1-11
d <- d1 %>% unlist %>% as.data.frame() %>%
  mutate(col_n = rep(1:11,length.out=length(.))) %>%
#group_by(col_n) %>%
    rename('title'=1,) %>%
#mutate(row = row_number())%>%
  pivot_wider(
              names_from=col_n,
              values_from = title
               )  %>% unchop(everything())
colnames(d) <- c("Notify","blank","Photo","ID","Name","Basis", "Units","Program","area","Level", "Status" )

d %>% select(4,5,11)




v <- v2 %>% unlist %>% as.data.frame() %>%
  mutate(col_n = rep(1:10,length.out=length(.))) %>%
  #group_by(col_n) %>%
  rename('title'=1,) %>%
  #mutate(row = row_number())%>%
  pivot_wider(
    names_from=col_n,
    values_from = title
  )  %>% unchop(everything())
colnames(v) <- c("Notify","blank","Photo","ID","Name","Basis", "Units","Program","area","Level" )

rbind(d %>% filter(Status!="Withdrawn") %>% select(4,5),v %>% select(4,5)) -> reflect

reflect %>% separate(Name,into = c("l_name",'f_name'),sep=',') %>% left_join(p,by='l_name')  %>% filter(is.na(Timestamp)) %>% mutate(message=paste0("\n\n","Hello ",f_name.x,",","\n\n","This is a hard message to send.",'\n\n'," You're receiving it to let you know that I haven't heard from you and that you haven't done the first self-reflection that was due Sunday, Jan 23rd 11:59pm. In an effort to keep you on track, I imposed this deadline so that issues could be handled before the end of the quarter.  As of right now, you're a day late and I'm willing to Grant an additional day but at this point it's looking like you will not be able to get better than a B for the quarter. It's still expected that you do the work and that you are not late for the next 3 week deadline. If you miss that deadline you won't be able to get better than a C.","\n\n","Let me know if you can get it done in the next day.","\n\n")) %>% print(.,n=100) %>% write_clip()


