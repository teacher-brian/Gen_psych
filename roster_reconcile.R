# for zoom
library()
x <- c('tidyr','dplyr','lubridate','stringr','clipr')
lapply(x,library,character.only=T)


roll<- read_clip()

roll.df<- str_split_fixed(roll,'\t',n=5) %>% as.data.frame()
head(roll.df)
colnames(roll.df) <- roll.df[1,]
roll.df <- roll.df[-1,]
row.names(roll.df) <- 1:nrow(roll.df)


roll.df[order(roll.df$`Name (Original Name)`,roll.df$`Leave Time`),] %>% select(1,4,5)

roll.df[grep('jus',roll.df$`Name (Original Name)`,ignore.case = T),]





credits<- read.csv( '/home/brian/Downloads/1-20-22_MoreThan15credits.csv')
d1 <- read.csv( '/home/brian/Downloads/1-20-22_d1.csv')
v2 <- read.csv( '/home/brian/Downloads/1-20-22_v2.csv')
roster<- rbind(d1,v2)

# split name into last and first

roster %>% separate(Name,into=c('Last','First'),sep=',') %>% inner_join(credits,by =c("Last" = "Last.Name"))



credits[grep("Lopez",credits$Last.Name,ignore.case = T),]
roster[grep("Lopez",credits$Last,ignore.case = T),]
