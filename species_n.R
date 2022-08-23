#Number of genera and species used

df<-read.csv("~/Desktop/PhD project/corals R/data/restoration_final.csv")
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
#(numero generi/specie usate in un sito vs. numero generi/specie coralli totali per quel sito).
div_perc<-(df$rest_sp_n/df$div)
df <-cbind(df,div_perc)
df<- df[!is.na(df$div_perc),]
df<-df[-125,]
mean(df$div_perc)
max(df$div_perc)
plot(df)
