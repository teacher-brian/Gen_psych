library(clipr)
library(tidyverse)

#functions
## 5 likert
transformNameWithNum <- function(df, col) {
  tibble(
    "asNum"  = case_when({{col}} =="strongly disagree"~5,
                         {{col}} =="disagree"~4,
                         {{col}} =="neutral"~3,
                         {{col}} =="agree"~2,
                         {{col}} =="strongly agree"~1))
}


## 7 likert
transformNameWith7Num <- function(df, col) {
  tibble(
    "asNum"  = case_when({{col}} =="strongly disagree"~7,
                         {{col}} =="Mostly disagree"~6,
                         {{col}} =="disagree"~5,
                         {{col}} =="neutral"~4,
                         {{col}} =="agree"~3,
                         {{col}} =="Mostly agree"~2,
                         {{col}} =="strongly agree"~1))
}

fin<- read_clip()

data.frame(fin) %>%
  mutate(fin=tolower(fin)) %>%
  mutate(finNum=case_when(fin=="strongly disagree"~7,
                          fin=="Mostly disagree"~6,
                          fin=="disagree"~5,
                          fin=="neutral"~4,
                          fin=="agree"~3,
                          fin=="Mostly agree"~2,
                          fin=="strongly agree"~1)) %>%
  summarize(mean(finNum,na.rm=T))



concentrate <- read_clip()
tense <- read_clip()
deadline <- read_clip()

df <- data.frame(concentrate,tense,deadline)
df %>%
  mutate(across (concentrate:deadline, ~transformNameWithNum(df, .x), .unpack = T))

data.frame(concentrate,tense,deadline) %>%
  mutate(across(concentrate:deadline, tolower)) %>%
  mutate(across(concentrate:deadline,
               ~case_when(.=="strongly disagree"~5,

                          .=="disagree"~4,
                          .=="neutral"~3,
                          .=="agree"~2,

                          .=="strongly agree"~1))) %>%
  group_by(concentrate,deadline) %>%
  summarize(mean(tense,na.rm=T))


