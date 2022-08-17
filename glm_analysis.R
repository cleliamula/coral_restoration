###Multivariate binomial analysis - difference between restored and natural reefs
glm<-glm(restoration ~ SSTA_mean_trend+remotness+fishing_demersal_destructive_trend+cumulative_impact_trend+rich, family=binomial, data=reef)
summary(glm)

model<-glm(restoration ~ ., family=binomial, data=reef)

library(MASS)

model_step=stepAIC(model)

glm_step<-glm(restoration ~ DHW_trend + SSTA_mean_trend + HS_trend + 
                BAA_trend + light_trend + fishing_demersal_destructive_trend + 
                remotness + rich + cumulative_impact_trend + SST_trend, family=binomial, data=reef)
summary(glm_step)
