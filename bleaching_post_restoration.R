#Bleaching after restoration
df<-read.csv("restoration_final.csv",header=T) 

BAA_dict<-list()
for (year in 1986:2021){
  BAA_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/baa-max_",year,".tiff")))
}

anom_post<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){anom_post<-c(anom_post,NA)} else {
    baa<-c()
    for (y in year:2021){
      baa<-c(baa,BAA_dict[[y]][row,col]) 
    }
    anom_post<-c(anom_post,sum(baa==4)) 
  }
}

table(anom_post) 
