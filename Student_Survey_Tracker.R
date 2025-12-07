library(googlesheets4)
library(tidyverse)


#ts_raw<- read_sheet("https://docs.google.com/spreadsheets/d/11zT13iBZw3QIIQDS1j8GJnI4GMmHo_S-oxfL1dGPssU/edit?usp=sharing",range= "Form Responses 1")
write.csv(ts_raw,"formTracking.csv")
ts <- read.csv("formTracking.csv")
str(ts)
ts <- ts[,-1]
dfts<- data.frame((apply(ts[,5:53],2,function(x) sum(!is.na(x)))))

colnames(dfts) <- "count"
dfts %>%
  rownames_to_column("form") %>%
  mutate(form = fct_reorder(form, count)) %>%

  # arrange(desc(count)) %>%
  ggplot(aes(x=form,y=count))+

  geom_col()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))





# #
#
# ts %>% summarise(across(`form 1`: `form 49`, ~ count(.x)))
# # summarise(across(c(col1, col2, col3), ~sum(is.na(.)), .names = "na_count_{.col}"))