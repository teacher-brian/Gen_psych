# processing student generated theory questions
library(clipr)
library(googlesheets4)
library(tidyverse)
q<- read_sheet("https://docs.google.com/spreadsheets/d/1oS_4LnTDPsO2R-AlIDX0UxO0TuoSol3xIdZTW6BPABg/edit?resourcekey=&gid=2010885043#gid=2010885043")
colnames(q)
q %>% group_by(`Please select your class`) %>%
  arrange( `Please select your class`,`What is your name as in canvas`) %>%
  print(n=100)


colnames(q) <- c("time","name","class","q1","q1answers","q2","q2answers","q3","q3answers")

q %>% pivot_longer(cols=c(4,6,8),names_to = "q_num",values_to = "q")


q_long <- q %>%
  rename_with(~ sub("^q(\\d+)$", "question\\1", .x)) %>%
  rename_with(~ sub("^q(\\d+)answers$", "answers\\1", .x)) %>%
  pivot_longer(
    cols = matches("^(question|answers)\\d+$"),
    names_to = c(".value", "item"),
    names_pattern = "^(question|answers)(\\d+)$"
  )

q_long %>% filter(class=="11am")
