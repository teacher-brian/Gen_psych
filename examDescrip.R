library(clipr)
library(tidyverse)

# obtain dataa

# e1 <- read_clip_tbl(header=T)
# #colnames(e1) <- "exam1"
# e1$class <- "9am"
#
# e2 <- read_clip_tbl(header=T)
# e2$class <- "11am"
#
# e3 <- read_clip_tbl(header=T)
# e3$class <- "1pm"
# colnames(e2) <- colnames(e1)
# colnames(e3) <- colnames(e1)
#
#
# exam1Win26<- e1
#
# exam1Win26 <- rbind(e1[e1$class!="1pm",],e3)
#

 # write.csv(exam1Win26,"exam1Win26.csv",row.names = F)

e1 <- read.csv("exam1Win26.csv")


# temp1<- e1[,c(1:4,7)] %>% pivot_longer(cols=c(exam.1,sa1),names_to = "exam", values_to = "score") %>%
#   rename(examVers=exam1version)
#
# temp2<- e1[,c(1,2,5,6)] %>% pivot_longer(cols=Exam2,names_to = "exam", values_to = "score") %>%
#   rename(examVers=Exam2Version)
#
# e1<- rbind(temp1,temp2) %>% arrange(class,id)


e1 %>%
  group_by(class,exam,examVers) %>%
  summarise(avg=mean(score,na.rm=T),sd=sd(score,na.rm=T),n())



