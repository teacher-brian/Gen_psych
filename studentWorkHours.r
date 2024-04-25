library(googlesheets4)
library(tidyverse)
library(ggridges)

sh_raw<- read_sheet("https://docs.google.com/spreadsheets/d/1_thOimGodxEqC006RQLB84uZBe7oCf0e-W2CJDpkyYs/edit?resourcekey#gid=675895326",range= "Form Responses 1")

sh <- sh_raw
str(sh)
colnames(sh) <- c("timestamp","student_class","week","psych_hrs","class2_hrs","class3_hrs","class4_hrs","class5_hrs","hs_hrs","work_hrs")
sh[,2:3] <- lapply(sh[,2:3],as.factor)
sh <- sh[-1,]
sh %>% group_by(student_class) %>%
  summarise(across(ends_with("hrs"),.f = list(mean = mean, sd = sd, max = max), na.rm = TRUE)) %>% t()

# total hours worked

Total_Hours_possible <-  24*7

hist(apply(sh[,4:10],1,sum,na.rm=T))

sh<- sh %>% mutate(total_hrs=rowSums( across(ends_with('hrs')),na.rm=T))



sh %>% group_by(student_class) %>%
  select(student_class,psych_hrs) %>%
  ggplot(aes(x=student_class,y=psych_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=class2_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=class3_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=class4_hrs))+geom_boxplot()


sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=class5_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=hs_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=work_hrs))+geom_boxplot()



sh %>% pivot_longer(cols = 4:10,names_to = 'source',values_to = 'hours') %>%
  group_by(student_class,source) %>%
  ggplot(aes(x=source,y=hours))+
  geom_boxplot()+
  facet_grid(~student_class)+
  geom_hline(yintercept = quantile(sh$psych_hrs,na.rm=T)[3],color='blue')


# ridges

sh %>% pivot_longer(cols = 4:10,names_to = 'source',values_to = 'hours') %>%
  group_by(student_class,source) %>%
  ggplot(aes(y=source,x=hours))+  geom_density_ridges()+
  facet_wrap(~student_class)


psych_total_comp.lm <- lm(sh$total_hrs~sh$psych_hrs)
plot(sh$psych_hrs,sh$total_hrs)
abline(psych_total_comp.lm)


count_na <- function(x) sum(!is.na(x))
apply(sh[,4:10],1,count_na)
sh <- sh %>% mutate(count_of_activities = apply(sh[,4:10],1,count_na))



sh %>% mutate(non_work_hrs = Total_Hours_possible- total_hrs) %>%
  mutate(free_time=non_work_hrs - Total_Hours_possible*.33) %>%
    #select(11:13) %>%
  ggplot(aes(y=free_time,x=count_of_activities))+
  geom_point()+
  geom_smooth()

hist(sh$count_of_activities)






#what is the average psych hours for students reporting 1, 2, 3, 4, 5 classes?  group


sh %>% group_by(student_class,count_of_activities) %>% summarise(mean(psych_hrs)) %>% ggplot(aes(x=count_of_activities,y=`mean(psych_hrs)`))+
  geom_point()+geom_smooth()






# Week 3




sh3_raw<- read_sheet("https://docs.google.com/spreadsheets/d/1KFlGLc8jLC144P7kjcrxHYk8j_qQaMiUYRd7VA-nC4E/edit?usp=sharing",range= "Form Responses 1")

sh3 <- sh3_raw
str(sh3)
colnames(sh3) <- c("timestamp","student_class","week","psych_hrs","class2_hrs","class3_hrs","class4_hrs","class5_hrs","hs_hrs","work_hrs")


sh3[,2:3] <- lapply(sh3[,2:3],as.factor)

sh3 %>% group_by(student_class) %>%
  summarise(across(ends_with("hrs"),  list(mean = mean, sd = sd, max = max ), na.rm = TRUE)) %>% t()

#sh3 <- sh3[-1,]

# Standard error of mean



str(sh)

mean(sh$total_hrs-sh$work_hrs,na.rm=T)

sh %>% group_by(student_class) %>%
  summarise(mean=mean(total_hrs-work_hrs,na.rm=T))


sims <- 1000
sample.df <- data.frame('mean'=1:sims,'sd'=NA)
for (i in 1:sims){
  samp<- sample(nrow(sh),nrow(sh),replace = T)
  sample.df[i,1] <- mean(sh$total_hrs[samp]-sh$work_hrs[samp],na.rm=T)
  sample.df[i,2] <- sd(sh$total_hrs[samp]-sh$work_hrs[samp],na.rm=T)
}
apply(sample.df,2,mean)

#standard error is just the sd of these means
apply(sample.df[1],2,sd)

hist(sample.df$mean)
abline(v=apply(sample.df[1],2,sd)*2+mean(sample.df$mean),col='blue')
abline(v=mean(sample.df$mean)-apply(sample.df[1],2,sd)*2,col='blue')
abline(v=mean(sample.df$mean),col='red')

sd(sh$total_hrs-sh$work_hrs,na.rm=T)/sqrt(nrow(sh))


# standard error of mean example
sample.df[1,2]/sqrt(nrow(sh))

sample.df$se<- sample.df[,2]/sqrt(nrow(sh))

apply(sample.df,2,mean)

hist(sample.df$mean)
abline(v=mean(sh$total_hrs-sh$work_hrs,na.rm=T))
abline(v=mean(sample.df$mean),col='blue')

#reminder of quantitude where have a sample distribution at top, long line down page, and then each row is a new sample



#standard error for work hours given it's so skewey

mean(sh$work_hrs,na.rm=T)

sims<- 1000
sample_wrk.df <- data.frame('mean'=1:sims,'sd'=NA)
for (i in 1:sims){
  samp<- sample(nrow(sh),nrow(sh),replace = T)
  sample_wrk.df[i,1] <- mean(sh$work_hrs[samp],na.rm=T)
  sample_wrk.df[i,2] <- sd(sh$work_hrs[samp],na.rm=T)
}

sample_wrk.df


hist(sh$work_hrs)
abline(v=mean(sh$work_hrs,na.rm=T))
abline(v=mean(sh$work_hrs,na.rm=T)+2*(sd(sh$work_hrs,na.rm=T)/sqrt(nrow(sh))),col='blue')
abline(v=mean(sh$work_hrs,na.rm=T)-2*(sd(sh$work_hrs,na.rm=T)/sqrt(nrow(sh))),col='blue')


hist(sample_wrk.df$mean)
abline(v=mean(sample_wrk.df$mean))
abline(v=mean(sample_wrk.df$mean)+2*sd(sample_wrk.df$mean),col='blue')
abline(v=mean(sample_wrk.df$mean)-2*sd(sample_wrk.df$mean),col='blue')
