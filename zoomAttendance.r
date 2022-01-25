library(tidyverse)
library(lubridate)
library(readxl)
library(rvest)

getwd()
list.files(path="../data/zoomfall21")

list.files( "../data/zoomfall21/", pattern="participants*")
list.files( "../data/zoomfall21/", pattern="participants*\\_91",recursive = T,)

temp <-  list.files( "../data/zoomfall21/", pattern="participants*\\_91",recursive = T)
myfiles <-  lapply(paste0("../data/zoomfall21/",temp), read.csv)


abnorm<- bind_rows(myfiles)

str(abnorm)
# need to split out date and time

abnorm$Join.Time <- (mdy_hms(abnorm$Join.Time))
abnorm$Leave.Time <-(mdy_hms(abnorm$Leave.Time))

colnames(abnorm) <- c("name","email","join","leave","dur",'guest','record.consent')
abnorm$name <- as.factor(abnorm$name)

#must merge names (brian's ipad etc)
unique(abnorm$name)

abnorm[grep("Maghrbi",abnorm$name),"name"] <- "Ibrahim Maghrbi"
abnorm[grep("michael",abnorm$name,ignore.case=T),"name"] <- "Michael Paulsen"
abnorm[grep("kj|Kayden",abnorm$name,ignore.case=T),"name"] <- "Kayden B (They/Them) Duwamish land"
abnorm[grep("destiny",abnorm$name,ignore.case=T),"name"] <- "Destiny Jeffery"

abnorm[grep("sien",abnorm$name,ignore.case=T),"name"] <- "sien (they/them)"

abnorm[grep("jada",abnorm$name,ignore.case=T),"name"] <- "Jada Mobley"
abnorm[grep("Nadah|Ayni",abnorm$name,ignore.case=T),"name"] <- "Ayni Mohamed"
abnorm[grep("Anna Lisa|Amory",abnorm$name,ignore.case=T),"name"] <- "Anna Lisa (Amory)"

abnorm$name<- factor(abnorm$name)

abnorm %>% arrange(join) %>% mutate(d=(date(join)),t=hms(date(join))) %>%
  select(name,d,dur,t) %>% group_by(name,d) %>% summarise(n=n(),duration=sum(dur)) %>% filter(duration>2) %>% #print(.,n=100)
  ggplot(aes(x=d,y=duration,group=name))+geom_line()+facet_wrap(~name)



abg<- read.csv("../data/zoomfall21/abnormal.csv")

abg<- abg[complete.cases(abg),] %>% select(ID,Name,grade=Roster.Grade,w=Official.Grade)

stringr::spl

abg<- abg[!is.na(abg$ID),]
abg %>% extract(Name,into=c('ln','fn'),"(.*),(.*)") %>% mutate(gpa=as.numeric(Roster.Grade)) %>% select(ID,fn,ln,gpa)




#Intro



getwd()
list.files(path="../data/zoomfall21")

list.files( "../data/zoomfall21/", pattern="participants*")
list.files( "../data/zoomfall21/", pattern="participants*\\_95",recursive = T,)

temp <-  list.files( "../data/zoomfall21/", pattern="participants*\\_95",recursive = T)
myfiles <-  lapply(paste0("../data/zoomfall21/",temp), read.csv)


gen<- bind_rows(myfiles)

str(gen)
# need to split out date and time

gen$Join.Time <- (mdy_hms(gen$Join.Time))
gen$Leave.Time <-(mdy_hms(gen$Leave.Time))

colnames(gen) <- c("name","email","join","leave","dur",'guest','record.consent')
gen$name <- as.factor(gen$name)

#must merge names (brian's ipad etc)
unique(gen$name)

gen[grep("pa |912358",gen$name,ignore.case=T),"name"] <- "Pa (912358)"
gen[grep("Kamila",gen$name,ignore.case=T),"name"] <- "Kamila V they/them"
gen[grep("zoe",gen$name,ignore.case=T),"name"] <- "Zoe Thompson"
gen[grep("tsega",gen$name,ignore.case=T),"name"] <- "Tsega Zemikael"
gen[grep("erin",gen$name,ignore.case=T),"name"] <- "Erin Amundson (erinamundson)"
gen[grep("duwanna",gen$name,ignore.case=T),"name"] <- "Duwanna Reese (she\\her)"
gen[grep("morgan",gen$name,ignore.case=T),"name"] <- "Morgan K (Morgan Killgore)"
gen[grep("yotsana",gen$name,ignore.case=T),"name"] <- "Yotsana (Abhijit)"
gen[grep("charley|miella",gen$name,ignore.case=T),"name"] <- "Charley G. (they/he)"
gen[grep("anna|amory",gen$name,ignore.case=T),"name"] <- "Anna Lisa"
gen[grep("shirley",gen$name,ignore.case=T),"name"] <- "Shirley Leon robles"
gen[grep("naod",gen$name,ignore.case=T),"name"] <- "Naod Fikadu"
gen[grep("benji",gen$name,ignore.case=T),"name"] <- "Benji Walker"
gen[grep("rachel",gen$name,ignore.case=T),"name"] <- "Rachel Taylor"
gen$name<- factor(gen$name)

gen %>% arrange(join) %>% mutate(d=(date(join)),t=hms(date(join))) %>%
  select(name,d,dur,t) %>% group_by(name) %>% summarise(n=n(),duration=sum(dur))

%>% filter(duration>2) %>% #print(.,n=100)
  ggplot(aes(x=d,y=duration,group=name))+geom_line()+facet_wrap(~name)



#geng<- read.csv("../data/zoomfall21/abnormal.csv")
geng1<- read_xls("../data/zoomfall21/v2.xls",)
geng1 <- cbind(geng1,class="v2")
geng2<- read_xls("../data/zoomfall21/d3.xls")
geng2 <- cbind(geng2,class="d3")
geng<- rbind(geng1,geng2)
str(geng)

geng <- geng %>% mutate(`Official Grade`=case_when(`Official Grade`=='W'~'W',
                                is.na(`Official Grade`)~'E'))

geng<- geng[!is.na(geng$ID),]

geng<-geng %>% select(ID,Name,grade="Roster Grade",w="Official Grade") %>%
  extract(Name,into=c('ln','fn'),"(.*),(.*)") %>%
  mutate(gpa=as.numeric(grade))



