library(googlesheets4)
library(tidyverse)

sh<- read_sheet("https://docs.google.com/spreadsheets/d/1_thOimGodxEqC006RQLB84uZBe7oCf0e-W2CJDpkyYs/edit?resourcekey#gid=675895326",range= "Form Responses 1")


str(sh)
colnames(sh) <- c("timestamp","student_class","week","psych_hrs","class2_hrs","class3_hrs","class4_hrs","class5_hrs","hs_hrs","work_hrs")
sh[,2:3] <- lapply(sh[,2:3],as.factor)

sh %>% group_by(student_class,week) %>%
  summarise(across(ends_with("hrs"),  list(mean = mean, sd = sd, max = max ), na.rm = TRUE))

