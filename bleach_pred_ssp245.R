library(raster)
library(rgdal)
library(viridis)

clim_fold<-'./climate_05_deg_buffered/'
sce<-'ssp245'
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
anom_245<-c()
for (year in 2020:2100){
  for (month in 1:12){
    t_m<-as.matrix(raster(paste0(clim_fold,sce,'_',var,'_',sc,'.tif')))[reef_cells]
    anom_245<-cbind(anom_245,(1*(t_m-2)>t_month_dict[[month]]))
    sc<-sc+1
  }
  print (year)
}

#anom is a table where each row is a reef locality, and each column is a month (from 2015 to 2100)
#1 means that there was an anomaly at a given month in a given reef locality
#if you compute rowSums, you get the number of monthly anomalies per locality, which you can then map
library(ggplot2)
anom_count_245<-matrix(NA,360,720)
anom_count_245[reef_cells]<-rowSums(anom_245)
ocean_05<-raster("./ocean_05.tif")
plot(ocean_05,col="lightblue")
plot(raster(anom_count_245),col=plasma(100))

#first time bleaching
first_t_245<-c()
for (i in 1:nrow(anom_245)){
  first_t_245<-c(first_t_245,min(which(anom_245[i,]==1)))
}

anom_first_245<-matrix(NA,360,720)
anom_first_245[reef_cells]<-first_t_245
plot(raster(anom_first_245),col=plasma(100))

##trend
trend<-function(v){
  return(lm((1:length(v))~v)$coefficients[[2]])
}

anom_trend_245<-apply(anom_245,1,trend) 