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



exam1Win26<- rbind(e1,e2,e3)
 # write.csv(exam1Win26,"exam1Win26.csv",row.names = F)

# exam1Win26 <- read.csv("exam1Win26.csv")

mean(exam1Win26$exam.1,na.rm=T)
quantile(exam1Win26$exam.1)
hist(exam1Win26$exam.1,breaks=13)
summary(lm(data=exam1Win26,exam.1~class))



scale()



sims <- 1e4

e1mc<- sample(e1$exam1,sims,replace = T)
hist(e1mc)
mean(e1mc)
