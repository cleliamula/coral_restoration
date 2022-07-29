library(ggplot2)
library(ggpubr)

#comparative boxplots: natural vs restore reefs
reef<-read.csv("reefs_final.csv",header=T)

a<-ggboxplot(
  reef,
  x="restoration",
  y="SSTA_mean_trend",
  xlab = "natural reefs vs restoration reefs",
  ylab = "SSTA trend", outlier.colour = NA)

remoteness_log<- log(reef$remotness+1)
reef<- cbind(reef, remoteness_log)
range(reef$remoteness_log)
b<-ggboxplot(
  reef,
  x="restoration",
  y="remoteness_log",
  xlab = "natural reefs vs restoration reefs",
  ylab = "remoteness", outlier.colour = NA, ylim=c(0,1.9))

range(reef$fishing_demersal_destructive_trend, na.rm=TRUE)
c<-ggboxplot(
  reef,
  x="restoration",
  y="fishing_demersal_destructive_trend",
  xlab = "natural reefs vs restoration reefs",
  ylab = "commercial fishing trend", outlier.colour = NA,ylim=c(-900,900) )

range(reef$cumulative_impact_trend,  na.rm=TRUE)
d<-ggboxplot(
  reef,
  x="restoration",
  y="cumulative_impact_trend",
  xlab = "natural reefs vs restoration reefs",
  ylab = "cumulative impacts trend", outlier.colour = NA, ylim=c(0,35) )

e<-ggboxplot(
  reef,
  x="restoration",
  y="SST_trend",
  xlab = "natural reefs vs restoration reefs",
  ylab = "SST trend", outlier.colour = NA, ylim=c(-0.1, 0.2))

f<-ggboxplot(
  reef,
  x="restoration",
  y="rich",
  xlab = "natural reefs vs restoration reefs",
  ylab = "coral diversity",outlier.colour = NA)

pdf("restored_vs_reef.pdf")

((a)|(e))/
  ((b)|(c))/
  ((f)|(d))

dev.off()
                 
t.test(reef$SSTA_mean_trend~reef$restoration)

t.test(reef$remotness~reef$restoration)

t.test(reef$fishing_demersal_destructive_trend~reef$restoration)

t.test(reef$cumulative_impact_trend~reef$restoration)

t.test(reef$rich~reef$restoration)
