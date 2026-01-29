library(clipr)
library(tidyverse)

# obtain dataa

e1 <- read_clip_tbl(header=T)
#colnames(e1) <- "exam1"
e1$class <- "9am"

e2 <- read_clip_tbl(header=F)
e2$class <- "11am"

e3 <- read_clip_tbl(header=F)
e3$class <- "1pm"
colnames(e2) <- colnames(e1)
colnames(e3) <- colnames(e1)


exam1Win26<- rbind(e1,e3)

exam1Win26 <- rbind(e1[e1$class!="1pm",],e3)


 # write.csv(exam1Win26,"exam1Win26.csv",row.names = F)

e1 <- read.csv("exam1Win26.csv")

mean(exam1Win26$exam.1,na.rm=T)
quantile(exam1Win26$exam.1)
hist(exam1Win26$exam.1,breaks=13)
summary(lm(data=exam1Win26,exam.1~class))


e1 <- exam1Win26

e1 %>% group_by(class) %>% mutate(across(exam.1:sa1,~scale(.))) %>%
  summarize(corr=cor(e1$exam.1,e1$sa1,use="pairwise.complete.obs"))

# cor(e1$exam.1[e1$class=="1pm"],e1$sa1[e1$class=="1pm"],use = "pairwise.complete.obs")


sims <- 1e4

e1mc<- sample(e1$exam1,sims,replace = T)
hist(e1mc)
mean(e1mc)





e1 %>% group_by(class) %>%
  summarise(mean=mean(exam.1,na.rm=T),
            sd=sd(exam.1,na.rm=T),
            med=median(exam.1,na.rm=T),
            n=n())
