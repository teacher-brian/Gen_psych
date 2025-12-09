library(googlesheets4)
library(tidyverse)


ts_raw<- read_sheet("https://docs.google.com/spreadsheets/d/11zT13iBZw3QIIQDS1j8GJnI4GMmHo_S-oxfL1dGPssU/edit?usp=sharing",range= "Form Responses 1")
write.csv(ts_raw,"formTracking.csv")
ts <- read.csv("formTracking.csv")
str(ts)
ts <- ts  %>% select(-X)
dim(ts)

# which forms have been 'used'

dfts<- data.frame((apply(ts[,5:57],2,function(x) sum(!is.na(x)))))

colnames(dfts) <- "count"
dfts %>%
  rownames_to_column("form") %>%
  mutate(form = fct_reorder(form, count)) %>%

  # arrange(desc(count)) %>%
  ggplot(aes(x=form,y=count))+

  geom_col()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))



# make a list of surveys with more than 11:

dfts %>%
  rownames_to_column("form") %>%
  mutate(form = fct_reorder(form, count)) %>%
  filter(count>9) %>% print() %>%  arrange(form) %>% nrow()


  ts <- ts %>%  mutate(across(2:4,as.factor))
#which classs has done it
ts[,1:4] %>%
  group_by(what.class.are.you.in) %>% tally

# days to due date

ts[,1:4] %>%
  group_by(what.class.are.you.in) %>% select(Timestamp,what.class.are.you.in) %>%
  summarise(mean("2025-12-08 23:59:00"-as.Date(Timestamp)))


# which students
ts[,1:4] %>%
  group_by(What.is.your.Last.name.) %>%
  select(What.is.your.first.name.,What.is.your.Last.name.) %>%
  # filter(What.is.your.Last.name.=="Smith") %>%
 print(n=100)

ts %>% filter(What.is.your.Last.name.=='Smith') %>% t()

# check count

ts[-c(1:4),] %>%
  pivot_longer(cols = -c(`Timestamp`,`What.is.your.first.name.`,`What.is.your.Last.name.`,`what.class.are.you.in`),
               names_to = "form",
               values_to = "completed",
               values_drop_na = T) #%>%


