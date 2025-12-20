library(clipr)
library(tidyverse)

#functions
## 5 likert
transformNameWithNum <- function(df, col) {
  tibble(
    "asNum"  = case_when({{col}} =="Strongly Disagree"~5,
                         {{col}} =="Disagree"~4,
                         {{col}} =="Neutral"~3,
                         {{col}} =="Agree"~2,
                         {{col}} =="Strongly Agree"~1))
}


## 7 likert
transformNameWith7Num <- function(df, col) {
  tibble(
    "asNum"  = case_when({{col}} =="Strongly Disagree"~7,
                         {{col}} =="Mostly Disagree"~6,
                         {{col}} =="Disagree"~5,
                         {{col}} =="Neutral"~4,
                         {{col}} =="Agree"~3,
                         {{col}} =="Mostly Agree"~2,
                         {{col}} =="Strongly Agree"~1))
}

fin<- read_clip()

data.frame(fin) %>%
  mutate(finNum=case_when(fin=="Strongly Disagree"~7,
                          fin=="Mostly Disagree"~6,
                          fin=="Disagree"~5,
                          fin=="Neutral"~4,
                          fin=="Agree"~3,
                          fin=="Mostly Agree"~2,
                          fin=="Strongly Agree"~1)) %>%
  summarize(mean(finNum,na.rm=T))



concentrate <- read_clip()
tense <- read_clip()
deadline <- read_clip()

df <- data.frame(concentrate,tense,deadline)
df %>%
  mutate(across (concentrate:deadline, ~transformNameWithNum(df, .x), .unpack = T))

data.frame(concentrate,tense,deadline) %>%
  mutate(across(concentrate:deadline,
               ~case_when(.=="Strongly Disagree"~5,

                          .=="Disagree"~4,
                          .=="Neutral"~3,
                          .=="Agree"~2,

                          .=="Strongly Agree"~1))) %>%
  group_by(concentrate,deadline) %>%
  summarize(mean(tense,na.rm=T))


