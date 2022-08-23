df<-read.csv("~/Desktop/PhD project/corals R/data/restoration_final.csv")
df<- df[!is.na(df$survival),]

library(caret)
library(randomForest)
names(df)
str(df)

data.imputed<-rfImpute(survival ~ ., data=df, iter=6)

####MODEL 1
rf1<-randomForest(survival ~ .,data=df)
print(rf1)
summary(rf1)
plot(rf1)
attributes(rf1)
rf1$rsq
rf1$oob.times

p1<-predict(rf1, data.imputed)
head(p1)
head(data.imputed$survival)
range(p1)
range(data.imputed$survival)
t<-tuneRF(data.imputed, data.imputed$survival,
       stepFactor = 0.5, 
       plot=TRUE,
       ntreeTry = 200,
       trace = TRUE,
       improve = 0.05,)

####MODEL 2
rf2<-randomForest(survival ~ .,data=data.imputed, ntree=100, importance=TRUE, proximity=TRUE)
plot(rf2)
p2<-predict(rf2, data.imputed)

rf2$err.rate[,]

library(ggplot2)

plot(data.imputed$survival,p2,
     las=1,cex.axis=1,cex.lab=1,
     pch=1,cex=0.8,xlab="observed survival",ylab="predicted survival")
abline(0,1)

hist(treesize(rf2),
     main="number of nodes of the Trees",)
varImpPlot(rf2)
importance(rf2)
varUsed(rf2)

##RF with CLASSIFICATION
#df$survival<-1*df$survival>50

rfc<-randomForest(as.factor(survival>70) ~ .,data=clean.imputed)
plot(rf3)
p3<-predict(rf3, clean.imputed)

hist(treesize(rfc),
     main="number of nodes of the Trees",)
varImpPlot(rfc)
importance(rfc)
varUsed(rfc)