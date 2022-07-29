library(ggplot2)
library(ggpubr)
####################STATISTICAL Explorations
df<-read.csv("~/Desktop/PhD project/corals R/data/restoration_final.csv")

library(patchwork)

div <- ggscatter(df, x = "div", y = "survival",
                add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                xlab = "coral diversity",
                ylab = "survival",
                conf.int = TRUE # Add confidence interval
)


rem <- ggscatter(df, x = "remoteness", y = "survival",
                 xlab = "remoteness",
                 ylab = "survival",
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = TRUE # Add confidence interval
)


SST <- ggscatter(df, x = "SSTmean_trend", y = "survival",
                 xlab = "SST trend",
                 ylab = "survival",
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = TRUE # Add confidence interval
)


SSTA <- ggscatter(df, x = "SSTA_mean_trend", y = "survival",
                  xlab = "SSTA trend",
                  ylab = "survival",
                 add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                 conf.int = TRUE # Add confidence interval
)


fish <- ggscatter(df, x = "commercial_fishing_demersal_destructive_trend", y = "survival",
                  xlab = "commercial fishing trend",
                  ylab = "survival",
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE # Add confidence interval
)


imp <- ggscatter(df, x = "cumulative_impacts_trend", y = "survival",
                 xlab = "cumulative impacts trend",
                 ylab = "survival",
                  add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
                  conf.int = TRUE # Add confidence interval
)


pdf("correlations.pdf")

(div+ stat_cor(method = "spearman", label.x = 1, label.y = 130)|rem+ stat_cor(method = "spearman", label.x = -0.1, label.y = 130))/
  (SST+ stat_cor(method = "spearman", label.x = -0.2, label.y = 130)|(SSTA+ stat_cor(method = "spearman", label.x = -0.2, label.y = 130)))/
  (fish+ stat_cor(method = "spearman", label.x = -0.1, label.y = 130)|(imp+ stat_cor(method = "spearman", label.x = -0.1, label.y = 130)))

dev.off()

###number of species used
df_sp<-df[!is.na(df$rest_sp_n),]
plot(df_sp$survival, df_sp$rest_sp_n)
cor(df_sp$survival, df_sp$rest_sp_n) 
cor.test(df_sp$rest_sp_n,df_sp$survival,method='spearman') 

###BOXPLOT technique used
ggplot(df) + geom_boxplot(aes(x=Technique, y=survival)) 
t.test(df$disturbances)
###BOXPLOT disturbance 
df_dis<-df[!is.na(df$Disturbance),]
ggplot(df_dis) + geom_boxplot(aes(x=Disturbance, y=survival)) 
