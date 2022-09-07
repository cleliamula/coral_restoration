#Number of genera and species used

df<-read.csv("restoration_final.csv",header=T)
library(tidyverse)
df$Coral_Species <- gsub(" ", "_", df$Coral_Species)

library(dplyr)
df1 <- separate_rows(df,Coral_Species, sep= ";")
df1 %>% count(Coral_Species)
n_distinct(df1$Coral_Species)

#dominant genera
df2 <- separate_rows(df,Genera, sep= ";")
n_distinct(df2$Genera)
table(df2$Genera)

#diversity percentage 
div_perc<-(df$rest_sp_n/df$div)
df <-cbind(df,div_perc)
df<- df[!is.na(df$div_perc),]

to_del<-which(!is.finite(df$div_perc))
df2<-df[-to_del,]
nrow(df2)

mean(df2$div_perc)
