library(raster)
library(rgdal)
library(viridis)

clim_fold<-'./climate_05_deg_buffered/'
sce<-'ssp585' 
var<-'tos'

reef<-as.matrix(raster('reef_05.tif'))
reef_cells<-which(reef==1)

t_month_dict<-list()
for (month in 1:12){
  t_month_dict[[month]]<-rep(0,length(reef_cells))
}

sc<-0
for (year in 2015:2019){
  for (month in 1:12){
    t_m<-as.matrix(raster(paste0(clim_fold,sce,'_',var,'_',sc,'.tif')))
    t_month_dict[[month]]<-t_month_dict[[month]]+t_m[reef_cells]
    sc<-sc+1
  }
}


for (month in 1:12){
  t_month_dict[[month]]<-t_month_dict[[month]]/5
}


###compute the monthly anomalies
anom_585<-c()
for (year in 2020:2100){
  for (month in 1:12){
    t_m<-as.matrix(raster(paste0(clim_fold,sce,'_',var,'_',sc,'.tif')))[reef_cells]
    anom_585<-cbind(anom_585,(1*(t_m-2)>t_month_dict[[month]]))
    sc<-sc+1
  }
  print (year)
}

#anom is a table where each row is a reef locality, and each column is a month (from 2015 to 2100)
#1 means that there was an anomaly at a given month in a given reef locality
#if you compute rowSums, you get the number of monthly anomalies per locality, which you can then map
library(ggplot2)
anom_count_585<-matrix(NA,360,720)
anom_count_585[reef_cells]<-rowSums(anom_585)

plot(raster(anom_count_585),col=plasma(100),xlim=c(0,1),ylim=c(0,1))


#first time of bleaching
first_t_585<-c()
for (i in 1:nrow(anom_585)){
  first_t_585<-c(first_t_585,min(which(anom_585[i,]==1)))
}

anom_first_585<-matrix(NA,360,720)
anom_first_585[reef_cells]<-first_t_585
plot(raster(anom_first_585),col=plasma(100))
dim(anom_585)

##trend
trend<-function(v){
  return(lm((1:length(v))~v)$coefficients[[2]])
}

anom_trend_585<-apply(anom_585,1,trend) 
