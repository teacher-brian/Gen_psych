library(clipr)
library(tidyverse)

# obtain dataa

# e1 <- read_clip_tbl(header=T)
# #colnames(e1) <- "exam1"
# e1$class <- "9am"
#
# e2 <- read_clip_tbl(header=F)
# e2$class <- "11am"
#
# e3 <- read_clip_tbl(header=F)
# e3$class <- "1pm"
# colnames(e2) <- colnames(e1)
# colnames(e3) <- colnames(e1)
#
#
# exam1Win26<- rbind(e1,e3)
#
# exam1Win26 <- rbind(e1[e1$class!="1pm",],e3)
#

 # write.csv(exam1Win26,"exam1Win26.csv",row.names = F)

e1 <- read.csv("exam1Win26.csv")

mean(e1$exam.1,na.rm=T)
quantile(e1$exam.1,na.rm=T)
hist(e1$exam.1,breaks=13)
summary(lm(data=e1,sa1~exam.1*class))


# e1 <- exam1Win26

e1 %>%
  filter(complete.cases(.)) %>%
group_by(class) %>%
  mutate(across(exam.1:sa1,~scale(.))) %>%
  summarize(corr=cor(exam.1,sa1))

e1 %>%
  group_by(class) %>% ggplot(aes(x=exam.1, y=sa1,group=class))+geom_point()


e1 %>%
  group_by(class) %>% ggplot(aes(x=exam.1,color=class))+geom_histogram(orientation = 'x')#+facet_grid(~class)

sims <- 1e4

e1mc<- sample(e1$exam1,sims,replace = T)
hist(e1mc)
mean(e1mc)





e1 %>% group_by(class) %>%
  summarise(mean=mean(exam.1,na.rm=T),
            sd=sd(exam.1,na.rm=T),
            med=median(exam.1,na.rm=T),
            n=n())



fisher_z <- function(r) 0.5 * log((1 + r) / (1 - r))

r <- c(0.554, 0.782, 0.307)
n <- c(32, 27, 27)  # class sizes

z <- fisher_z(r)
se <- 1 / sqrt(n - 3)

# compare 11am vs 1pm
z_diff <- (z[1] - z[2]) / sqrt(se[1]^2 + se[2]^2)
p_value <- 2 * (1 - pnorm(abs(z_diff)))


# compare 11am vs 9a
z_diff <- (z[1] - z[3]) / sqrt(se[1]^2 + se[3]^2)
p_value <- 2 * (1 - pnorm(abs(z_diff)))


# compare 1pm vs 9a
z_diff <- (z[2] - z[3]) / sqrt(se[2]^2 + se[3]^2)
p_value <- 2 * (1 - pnorm(abs(z_diff)))


ci_z <- c(NA,NA,NA)
for (i in 1:3){
  ci_z[i] <- z[i] + c(-1,1) * qnorm(.975) * se[i]
}

ci_r <- tanh(ci_z)

plot(x=r,ylim=c(-.2,1.5))
points(r+ci_r,col='red')
points(r-ci_r,col='red')




m1 <- lm(sa1 ~ exam.1 * class, data = e1)
m0 <- lm(sa1 ~ exam.1 + class, data = e1)

anova(m0, m1)
