library(clipr)
e1 <- read_clip_tbl(header=F)
colnames(e1) <- "exam1"
e1$class <- "9am"
hist(e1$exam1,breaks = 8)
mean(e1$exam1)
quantile(e1$exam1)
e2 <- read_clip_tbl(header=F)
e2$class <- "elam"
colnames(e2) <- c("exam1","class")


hist(e2$elam,breaks=10)
mean(e2$elam)
median(e2$elam)
quantile(e2$elam)


e3 <- read_clip_tbl(header=F)

colnames(e3) <- "exam1"
e3$class <- "1pm"
quantile(e3$exam1)
mean(e3$exam1)
hist(e3$exam1,breaks=10)
e2

exam1Win26<- rbind(e1,e2,e3)

mean(exam1Win26$exam1)
quantile(exam1Win26$exam1)
hist(exam1Win26$exam1,breaks=13)
summary(lm(data=exam1Win26,exam1~class))

