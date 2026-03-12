# processing student generated theory questions

library(googlesheets4)
library(tidyverse)
q<- read_sheet("https://docs.google.com/spreadsheets/d/1oS_4LnTDPsO2R-AlIDX0UxO0TuoSol3xIdZTW6BPABg/edit?resourcekey=&gid=2010885043#gid=2010885043")
colnames(q)
q %>% group_by(`Please select your class`) %>%
  arrange( `Please select your class`,`What is your name as in canvas`) %>%
  print(n=100)
