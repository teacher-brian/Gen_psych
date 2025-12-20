library(clipr)
library(tidyverse)

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


quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble(
    val = quantile(x, probs),
    quant = probs
  )
}

transformNameWithNum <- function(df, col) {
  tibble(
    "asNum"  = case_when({{col}} =="Strongly Disagree"~5,
                        {{col}} =="Disagree"~4,
                        {{col}} =="Neutral"~3,
                        {{col}} =="Agree"~2,
                        {{col}} =="Strongly Agree"~1))
}

#mutate(df, "{{col}}_asNum" := )

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


