library(tidyverse)
library(data.table)
library(readxl)
library(clipr)
sr<- read_clip()  #copy last and first names columns from self reflection spreadsheet
#How read the google sheet but without reading all data...data.table?


#read d1
getwd()
dir('~/Downloads', pattern = '2022-12-13.*')

d1 <- read_excel("/home/brian/Downloads/2022-12-13_D1.xls")
d1 <- d1 %>% select(ID,Name,`Status Note`)
d1 <- d1[!is.na(d1$ID),]
d1$section <- 'd1'
#read h2
h2 <- read_excel("/home/brian/Downloads/2022-12-13_H2.xls")
h2 <- h2 %>% select(ID,Name,`Status Note`)
h2 <- h2[!is.na(h2$ID),]
h2$section <- 'h2'
#read ab
a5 <- read_excel("/home/brian/Downloads/2022-12-13_H5_ab.xls")
a5 <- a5 %>% select(ID,Name,`Status Note`)
a5 <- a5[!is.na(a5$ID),]
a5$section <- 'a5'
#strsplit name

#h2

h2 <- h2 %>% separate(Name,into = c('l','f')) %>%
   mutate(across(1:4,toupper))


#d1

d1 <- d1 %>%
  separate(Name,into = c('l','f')) %>%
  mutate(across(1:4,toupper))

# a5
a5 <- a5 %>%
  separate(Name,into = c('l','f')) %>%
  mutate(across(1:4,toupper))


sr<- data.frame(str_split_fixed(sr,'\t',2),stringsAsFactors=F)
colnames(sr) <- c('l','f')
str(sr)
sr<- apply(sr,2,as.character)
sr %>% mutate(across(1:2,str_trim)) -> sr
sr %>% mutate(across(1:2,toupper)) -> sr
genRost <- rbind(h2,d1,a5) %>% filter(is.na(`Status Note`))

anti_join(genRost,sr,by='l')
left_join(genRost,sr,by='l') %>% filter(f.y!='<NA>') %>%
  arrange(l,section) %>% print(n=nrow(.))

"
d1

sifbarbanti@yahoo.com
kiki.patbrown@gmail.com
kasimaria@gmail.com
cameronjclements@gmail.com
addiegill13@gmail.com
rubbihernandez11@gmail.com
morgan@killgorenw.com
lewisad219@yahoo.com
vmarroquin14@gmail.com
edgarmtz2314@yahoo.com
kelemmuche1@gmail.com
alarcon.keylaa@gmail.com
aimeeshi1@hotmail.com
reillyjack0104@gmail.com


h2
jlbarrin@gmail.com
annikacorgi25@gmail.com
edmondjenna@gmail.com
lqdinh@protonmail.com
naod.fikadu@gmail.com
miellacgracie@gmail.com
ahoopshizzle@gmail.com
serena@javid.org
pajawneh@gmail.com
shleonrobles623@gmail.com
annalisa.lindberg@hotmail.com
colinapanton@gmail.com
Duwannal@icloud.com
sagerinehart@gmail.com
zrthompson04@gmail.com
yotsanatripathi@gmail.com
milavahidi@gmail.com
benjiwalker64@gmail.com
woldeab98@gmail.com

"