#word cloud load data

library(googlesheets4)
df <- read_sheet("https://docs.google.com/spreadsheets/d/1RzKjFkglxiXZWBSJCgoYecrDb6fCvPLXiCjoKybTPLg/edit?usp=sharing")

df <- df |> pivot_longer(cols=3:5,names_to = 'topic', values_to = 'value') |>
  mutate(class=as.factor(`What class time are you in?`),topic=as.factor(topic)) |>
  select(Timestamp,class,topic,value)

write.csv(df,file="wordCloudRaw.csv")
