library(googlesheets4)
library(tidyverse)
library(ggridges)

sh<- read_sheet("https://docs.google.com/spreadsheets/d/1_thOimGodxEqC006RQLB84uZBe7oCf0e-W2CJDpkyYs/edit?resourcekey#gid=675895326",range= "Form Responses 1")


str(sh)
colnames(sh) <- c("timestamp","student_class","week","psych_hrs","class2_hrs","class3_hrs","class4_hrs","class5_hrs","hs_hrs","work_hrs")
sh[,2:3] <- lapply(sh[,2:3],as.factor)
sh <- sh[-1,]
sh %>% group_by(student_class) %>%
  summarise(across(ends_with("hrs"),.f = list(mean = mean, sd = sd, max = max), na.rm = TRUE)) %>% t()



sh %>% group_by(student_class) %>%
  select(student_class,psych_hrs) %>%
  ggplot(aes(x=student_class,y=psych_hrs))+geom_boxplot()

sh %>% group_by(student_class) %>%
  ggplot(aes(x=student_class,y=class2_hrs))+geom_boxplot()


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


#what is the average psych hours for students reporting 1, 2, 3, 4, 5 classes?  For loop?






# Week 3




sh3<- read_sheet("https://docs.google.com/spreadsheets/d/1KFlGLc8jLC144P7kjcrxHYk8j_qQaMiUYRd7VA-nC4E/edit?usp=sharing",range= "Form Responses 1")


str(sh3)
colnames(sh3) <- c("timestamp","student_class","week","psych_hrs","class2_hrs","class3_hrs","class4_hrs","class5_hrs","hs_hrs","work_hrs")
sh3[,2:3] <- lapply(sh3[,2:3],as.factor)

sh3 %>% group_by(student_class) %>%
  summarise(across(ends_with("hrs"),  list(mean = mean, sd = sd, max = max ), na.rm = TRUE)) %>% t()

#sh3 <- sh3[-1,]

