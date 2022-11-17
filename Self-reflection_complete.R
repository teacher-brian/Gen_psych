

library(tidyverse)
library(lubridate)
library(googlesheets4)
library(RSelenium)
library(rvest)
library(clipr)
library(rvest)
library(kableExtra)

s<- read_sheet('https://docs.google.com/spreadsheets/d/1skEgkolZpr4r_iMaBM0qMxtuNAKATgyB0X2QDSpzor8/edit?resourcekey#gid=271104061')
#rename columns

variables <- c('Timestamp','l_name','f_name','class','aspects','effort','grade','concerns','thoughts?','reflect_num','workflow')

colnames(s) <- variables

#%>% s[!(duplicated(s$l_name) & duplicated(s$f_name)),]

(p<- s %>% filter(Timestamp > "2022-10-01") %>%
    #select(f_name,l_name,Timestamp) %>%
    arrange(-desc(Timestamp)) %>% mutate(l_name=toupper(l_name))) %>% arrange(Timestamp) %>%  print(.,n=100)



d1 <- read_clip()#11 columns or 10 columns?
(length(d1)-1)/11
columns<- d1[1] %>% str_split(pattern = '\t') %>% unlist() %>% .[-1]

d1 <- d1[-1]


v2 <- read_clip()  #11 columns
(length(v2)-1)/11
columns.v2<- v2[1] %>% str_split(pattern = '\t') %>% unlist() %>% .[-1]
v2 <- v2[-1]

v4 <- read_clip()  #11 columns
(length(v4)-1)/11
columns.v4<- v4[1] %>% str_split(pattern = '\t') %>% unlist() %>% .[-1]

v4 <- v4[-1]



# maybe add a new colum with a repetatin number 1-11
d <-
  d1 %>% unlist %>% as.data.frame() %>%
  mutate(col_n = rep(1:11,length.out=length(.))) %>%  #if 11 columns, rep 1:11

group_by(col_n) %>%
    rename('title'=1,) %>%
mutate(row = row_number())%>%
  pivot_wider(
              names_from=col_n,
              values_from = title
               )  %>% unchop(everything())
d <- d %>% mutate(section="d1") %>% #note if there are 10 or 11 columns
  rename("row"=1,"Notify"=2,"blank"=3,"Photo"=4,"ID"=5,"Name"=6,"Basis"=7, "Units"=8,"Program"=9,"area"=10,"Level"=11, "Status"=12,"section"=13 )

d %>% select(5,6,12,13)




v <- v2 %>% unlist %>% as.data.frame() %>%
  mutate(col_n = rep(1:11,length.out=length(.))) %>%
  group_by(col_n) %>%
  rename('title'=1,) %>%
  mutate(row = row_number())%>%
  pivot_wider(
    names_from=col_n,
    values_from = title
  )  %>% unchop(everything())
v <- v %>%mutate(section="v2") %>% #note if there are 10 or 11 columns
  rename("row"=1,"Notify"=2,"blank"=3,"Photo"=4,"ID"=5,"Name"=6,"Basis"=7, "Units"=8,"Program"=9,"area"=10,"Level"=11, "Status"=12,"section"=13 )


va <- v4 %>% unlist %>% as.data.frame() %>%
  mutate(col_n = rep(1:11,length.out=length(.))) %>%
  group_by(col_n) %>%
  rename('title'=1,) %>%
  mutate(row = row_number())%>%
  pivot_wider(
    names_from=col_n,
    values_from = title
  )  %>% unchop(everything())
va <- va %>% mutate(section="va") %>%
rename("row"=1,"Notify"=2,"blank"=3,"Photo"=4,"ID"=5,"Name"=6,"Basis"=7, "Units"=8,"Program"=9,"area"=10,"Level"=11, "Status"=12,"section"=13 )




w <- "Withdrawn"


##  Currently withdrawn students  ###

rbind(#v %>% filter(Status==w) %>% select(5,6,12,13),
      va %>% filter(Status==w) %>% select(5,6,12,13)
      #d %>% filter(Status==w) %>% select(5,6,12,13)
      )

##  Currently enrolled students to be joined with reflection data###

reflect <- rbind(#d %>%
                  # filter(Status!=w) %>%
                   #select(5,6,12,13),
                 #v %>%
                   #filter(Status!=w) %>%
                   #select(5,6,12,13)
                 va %>%
                   filter(Status!=w) %>%
                   select(5,6,12,13)
)

  ####  Currently enrolled students who have NOT done reflection for week 3  ###

reflect %>%  # Roster data
  separate(Name,into = c("l_name",'f_name'),sep=',') %>%
  mutate(l_name=toupper(l_name)) %>%
  left_join(p,by='l_name')  %>%    # joins with self reflection survey
  filter(is.na(reflect_num)|reflect_num == "Week 9 Self reflection") %>%
  select(1:5,13) %>%
  mutate(message=paste0("\n\n","Hello ",f_name.x,",","\n\n","This is a hard message to send.",'\n\n'," You're receiving it to let you know that you haven't done the first self-reflection that was due at the end of week 5, more or less. What makes this very hard is that tomorrow, November 18th, is the last day of the quarter to withdraw from the class, which is why I'm alerting you today. ","\n\n","A withdrawal leaves a 'w' on your transcript, but it doesn't affect your GPA.  After Friday, 11/18/22, you will not be able to withdraw, and so my worry is that you might not have enough time to catch up with quality work before the end of the quarter.","\n\n", "**I'm not saying you should withdraw.  Several students are probably working on week 5 and the self reflection this week or weekend.  But I am saying that your choice to continue or withdraw must be made by tomorrow.**  There are still 3-4 weeks in the quarter and for many students, they'll be able to finish up with good enough quality for a passing grade (which is anything above a 1.0).","\n\n","But I suspect that a few students might want to withdraw.  You can contact me with questions; I'll do my best to reply before tomorrow's deadline.\n\n")) %>% print(.,n=100) %>% write_clip()


  ####  Currently enrolled students who have NOT done reflection for week 6  ###

reflect %>%  # Roster data
  separate(Name,into = c("l_name",'f_name'),sep=',') %>%
  mutate(l_name=toupper(l_name)) %>%
  left_join(
    filter(p,reflect_num=="Week 5 Self Reflection"),  #If this code not present, then past reflections will be counted, which will hide those not completing week 6
    by='l_name') %>%
  select(l_name,f_name.x,grade,section,reflect_num,Status) %>%
  filter(Status !=w) %>%
  filter(is.na(reflect_num)) %>%   #remove "!" to find non-completes
  mutate(message=paste0("\n\n","Hello ",f_name.x,",","\n\n","This is a hard message to send.",'\n\n'," You're receiving it to let you know that I haven't heard from you and that you haven't done the second self-reflection that was due Sunday, Feb 13, 11:59pm. In an effort to keep you on track, I imposed this deadline so that issues could be handled before the end of the quarter.  As of right now, you're a few days late and I'm willing to Grant an additional day but at this point it's looking you'll be dropped a grade. It's still expected that you do the work and that you are not late for the next deadline. ","\n\n","Let me know if you can get this work done in the next day.","\n\n")) %>% write_clip()


  #filter(grade=="C")


  filter(grepl("something",grade,ignore.case = T)) # looks for people needing attention



  reflect %>%  # Roster data
    separate(Name,into = c("l_name",'f_name'),sep=',') %>%
    mutate(l_name=toupper(l_name)) %>%
    left_join(p,by='l_name') %>%
    filter(reflect_num=="Week 3 Self reflection"|reflect_num=="Week 6 Self Reflection"|reflect_num=="Week 9 Self Reflection") %>%
    select(l_name,f_name.x,grade,section,reflect_num,Status) %>%
    group_by(l_name) %>%
    summarise(n=n()) %>% filter(n<2) %>%
    #mutate(l_name.x=toupper(l_name.x)) %>%
    left_join(p,by=c('l_name'= 'l_name')) %>%
    select(f_name,l_name,grade,reflect_num)



  # week 9

  reflect  %>%  # Roster data
    separate(Name,into = c("l_name",'f_name'),sep=',') %>%
    mutate(l_name=toupper(l_name)) %>%
    left_join(
      filter(p,reflect_num=="Week 9 Self Reflection"),  #If this code not present, then past reflections will be counted, which will hide those not completing week 9
      by='l_name') %>%
    select(l_name,f_name.x,grade,section,reflect_num,Status) %>%
    filter(Status !=w) %>%
    filter(is.na(reflect_num)) %>% #remove "!" to find non-completes
    arrange(desc(l_name)) %>%

        mutate(message=paste0("\n\n","Hello ",f_name.x,",","\n\n","This is a hard message to send.",'\n\n'," You're receiving it to let you know that I haven't heard from you and that you haven't done the third self-reflection that was due Sunday, March 6, 11:59pm. In an effort to keep you on track, I imposed this deadline so that issues could be handled before the end of the quarter. At this point it's looking you'll be dropped a grade. It's still expected that you do the work and that you are not late for the next deadline, which is the end of the quarter.  There won't be any extensions then. ")) #%>%   write_clip()


fs <-   read_sheet('https://docs.google.com/spreadsheets/d/1ISNMf5zsysLM8nZZ7D3NsdGuekssT48SDOS9U4EJXew/edit?resourcekey#gid=1821652734')



variables.f <- c('Timestamp',	'l_name',	'f_name',	'slack_name',	'successes',	'helpers',	'questions',	'grade',	'letter',	'openToFeedback',	'adjustFeedback',	'clearly',	'otherThoughts',	'qualityClass',	'qualityOther',	'improveCrit',	'improveCritOther',	'improveWriting',	'improveWritingOther',	'enjoyWork',	'enjoyWorkOther',	'anxiety',	'anxietyOther',	'control',	'controlOther',	'whichClass',	'lastAssignFocusGeneral',	'lastAssignFocusAbnormal',	'whichAssignHelped')

colnames(fs) <- variables.f


(fp <- fs %>% filter(Timestamp > "2022-06-1") %>%
    #select(f_name,l_name,Timestamp) %>%
    arrange(-desc(Timestamp)) %>% mutate(l_name=toupper(l_name))) %>% arrange(Timestamp) %>%  print(.,n=100)



# who has not done final self reflection

reflect %>%  # Roster data
  separate(Name,into = c("l_name",'f_name'),sep=',') %>%
  mutate(l_name=toupper(l_name)) %>%
  left_join(fp,by='l_name') %>%
  select(section,ID,l_name,f_name.y,Status,grade) %>%
  separate(col=grade,into = c('grade','gpa'),sep="\\.\\.\\.\\.") %>%
  arrange(section,l_name) %>% print(.,n=100) %>%

filter(Status != "Withdrawn") %>%
  filter(is.na(f_name.y))%>%
  mutate(message="You are receiving this message because I do not have a final reflection from you.  The last week of the schedule has a link.  I can not give you a grade until you have completed this. Until then, You'll have what is called an incomplete.  We can change it later if necessary.  But If you can get this done ASAP, I might be able to avoid it before my deadline.\n\n It is possible this message is an error. If you filled out the final reflection using a last name spelled differently from how the college has you on the roster, then my program will claim you did not fill out the final reflection.\n\n For convenience, here is the final reflection: https://forms.gle/E6RBAtjGcNm7BTjs7\n\n") %>% select (-c(4:7)) %>% arrange(section,l_name)
#%>% write_clip()


fp %>% select(l_name,f_name,grade,whichClass) %>% group_by(whichClass) %>%
  tally()

#Merged data

reflect %>%  # Roster data
  separate(Name,into = c("l_name",'f_name'),sep=',') %>%
  mutate(l_name=toupper(l_name)) %>%
  left_join(fp,by='l_name')->md


md %>% select(Timestamp,l_name,f_name.y,section,grade,Status,whichClass) %>%  filter(Status!="Withdrawn") %>% arrange(desc(Timestamp)) %>% group_by(whichClass) %>% separate(col=grade,into = c('grade','gpa'),sep="\\.\\.\\.\\.") %>%
  mutate(section=factor(section),
         grade=factor(grade),
         gpa=as.numeric(gpa),
         whichClass=factor(whichClass)) %>%
  ggplot(aes(x=Timestamp,y=gpa))+geom_point()+geom_smooth(method='loess')+facet_grid(~whichClass)



md %>% filter(Status!="Withdrawn") %>% arrange(desc(Timestamp)) %>%
  group_by(whichClass) %>%
  separate(col=grade,into = c('grade','gpa'),sep="\\.\\.\\.\\.") %>%
  mutate(section=factor(section),
         grade=factor(grade),
         gpa=as.numeric(gpa),
         whichClass=factor(whichClass)) %>%
  select(l_name,f_name.y,section, gpa,successes) %>%
  filter(gpa>3.7) %>% select(l_name,section,successes)




md %>% filter(Status!="Withdrawn") %>%
  filter(f_name.y=="James") %>%
  select(section,successes,letter,clearly,otherThoughts,qualityClass) %>% kbl()
