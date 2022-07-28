library(raster)
library(rgdal)

#Import coral restoration dataset matched with 
df<-read.csv("dataset_matched.csv",header=T)
df<- df[!is.na(df$Year),]

#UPLOAD RASTERS about annual max of heat degree weeks
dhw<-list.files("./dhw_tiffs_05", ".tiff")

###create dictionary of dhw raster per year
dhw_dict<-list()
for (year in 1986:2021){
  dhw_dict[[year]]<-as.matrix(raster(paste0("./dhw_tiffs_05/dhw_",year,".tiff")))
}

###Degree Heating Week (DHW) shows how much heat stress has accumulated n an area over
#the past 12 weeks (3 months) by adding up any temperature exceeding 
#the bleaching threshold during that time period

#restoration year

dhw_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    dhw_y<-c(dhw_y,dhw_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {dhw_y<-c(dhw_y,NA)}
}

dhw_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){dhw_pre<-c(dhw_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,dhw_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    dhw_pre<-c(dhw_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
dhw_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){dhw_trend<-c(dhw_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,dhw_dict[[year]][row,col]) 
    }
    dhw_trend<-c(dhw_trend,lm(1:length(td)~td)[[1]][2])
  }
}


df<-cbind(df,dhw_y, dhw_pre, dhw_trend)


###################SST mean

#UPLOAD RASTERS 
SSTmean<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "sst-mean_")

SSTmean_dict<-list()
for (year in 1986:2021){
  SSTmean_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/sst-mean_",year,".tiff")))
}

#restoration year
SSTmean_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTmean_y<-c(SSTmean_y,SSTmean_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTmean_y<-c(SSTmean_y,NA)}
}

SSTmean_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmean_pre<-c(SSTmean_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTmean_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmean_pre<-c(SSTmean_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTmean_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmean_trend<-c(SSTmean_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTmean_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmean_trend<-c(SSTmean_trend,lm(1:length(td)~td)[[1]][2])
  }
}


###################
###SSTmax

#UPLOAD RASTERS
SSTmax<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "sst-max_")

###create dictionary of dhw raster per year
SSTmax_dict<-list()
for (year in 1986:2021){
  SSTmax_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/sst-max_",year,".tiff")))
}

#restoration year
SSTmax_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTmax_y<-c(SSTmax_y,SSTmax_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTmax_y<-c(SSTmax_y,NA)}
}


SSTmax_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmax_pre<-c(SSTmax_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTmax_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmax_pre<-c(SSTmax_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTmax_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmax_trend<-c(SSTmax_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTmax_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmax_trend<-c(SSTmax_trend,lm(1:length(td)~td)[[1]][2])
  }
}



###################
###SSTmin
#UPLOAD RASTERS 
SSTmin<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "sst-min_")

###create dictionary of dhw raster per year
SSTmin_dict<-list()
for (year in 1986:2021){
  SSTmin_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/sst-min_",year,".tiff")))
}

#restoration year
SSTmin_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTmin_y<-c(SSTmin_y,SSTmin_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTmin_y<-c(SSTmin_y,NA)}
}


SSTmin_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmin_pre<-c(SSTmin_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTmin_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmin_pre<-c(SSTmin_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTmin_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTmin_trend<-c(SSTmin_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTmin_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTmin_trend<-c(SSTmin_trend,lm(1:length(td)~td)[[1]][2])
  }
}


###################SSTA mean
###SST ANOMALY
#UPLOAD RASTERS 
SSTA_mean<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "ssta-mean_")

###create dictionary of dhw raster per year
SSTA_mean_dict<-list()
for (year in 1986:2021){
  SSTA_mean_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/ssta-mean_",year,".tiff")))
}

#restoration year
SSTA_mean_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTA_mean_y<-c(SSTA_mean_y,SSTA_mean_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTA_mean_y<-c(SSTA_mean_y,NA)}
}

SSTA_mean_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_mean_pre<-c(SSTA_mean_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTA_mean_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_mean_pre<-c(SSTA_mean_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTA_mean_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_mean_trend<-c(SSTA_mean_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTA_mean_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_mean_trend<-c(SSTA_mean_trend,lm(1:length(td)~td)[[1]][2])
  }
}

###################SSTA max
###SST ANOMALY
#UPLOAD RASTERS 
SSTA_max<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "ssta-max_")

###create dictionary of dhw raster per year
SSTA_max_dict<-list()
for (year in 1986:2021){
  SSTA_max_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/ssta-max_",year,".tiff")))
}

#restoration year
SSTA_max_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTA_max_y<-c(SSTA_max_y,SSTA_max_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTA_max_y<-c(SSTA_max_y,NA)}
}


SSTA_max_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_max_pre<-c(SSTA_max_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTA_max_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_max_pre<-c(SSTA_max_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTA_max_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_max_trend<-c(SSTA_max_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTA_max_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_max_trend<-c(SSTA_max_trend,lm(1:length(td)~td)[[1]][2])
  }
}



###################SSTA min
###SST ANOMALY
#UPLOAD RASTERS
SSTA_min<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "ssta-min_")

###create dictionary of dhw raster per year
SSTA_min_dict<-list()
for (year in 1986:2021){
  SSTA_min_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/ssta-min_",year,".tiff")))
}

#restoration year
SSTA_min_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    SSTA_min_y<-c(SSTA_min_y,SSTA_min_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {SSTA_min_y<-c(SSTA_min_y,NA)}
}


SSTA_min_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_min_pre<-c(SSTA_min_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,SSTA_min_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_min_pre<-c(SSTA_min_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
SSTA_min_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){SSTA_min_trend<-c(SSTA_min_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,SSTA_min_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    SSTA_min_trend<-c(SSTA_min_trend,lm(1:length(td)~td)[[1]][2])
  }
}



###################HS
###CORAL BLEACHING HOTSPOT
#UPLOAD RASTERS 
HS<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "hs-max_")

###create dictionary of dhw raster per year
HS_dict<-list()
for (year in 1986:2021){
  HS_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/hs-max_",year,".tiff")))
}

#restoration year
HS_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    HS_y<-c(HS_y,HS_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {HS_y<-c(HS_y,NA)}
}


HS_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){HS_pre<-c(HS_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,HS_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    HS_pre<-c(HS_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
HS_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){HS_trend<-c(HS_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,HS_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    HS_trend<-c(HS_trend,lm(1:length(td)~td)[[1]][2])
  }
}



##################BAA
###BLEACHING ALERT AREA
#UPLOAD RASTERS 
BAA<-list.files("~/Desktop/PhD project/corals R/nooa_tiffs_05/tiffs_05", "baa-max_")

###create dictionary of raster per year
BAA_dict<-list()
for (year in 1986:2021){
  BAA_dict[[year]]<-as.matrix(raster(paste0("./nooa_tiffs_05/tiffs_05/baa-max_",year,".tiff")))
}

#restoration year
BAA_y<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,3]
  if (year>1985){
    BAA_y<-c(BAA_y,BAA_dict[[year]][row,col]) #valore nell'anno della restoration}
  } else {BAA_y<-c(BAA_y,NA)}
}


BAA_pre<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){BAA_pre<-c(BAA_pre,NA)} else {
    y_pre<-year-5
    if (y_pre<1986){y_pre<-1986}
    mean_pre<-c()
    for (year in y_pre:year){
      mean_pre<-c(mean_pre,BAA_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    BAA_pre<-c(BAA_pre,mean(mean_pre))
  }
  print (i)
}



##mettere in data_trend tutti i valori della localita' dall'anno y_pre all'anno y_post
BAA_trend<-c()
for (i in 1:nrow(df)){
  row<-df[i,1]+1
  col<-df[i,2]+1
  year<-df[i,]$Year
  if (year<1986){BAA_trend<-c(BAA_trend,NA)} else {
    y_pre<-year-5
    y_post<-year+5
    if (y_pre<1986){y_pre<-1986}
    if (y_post>2021){y_post<-2021}
    td<-c()
    for (year in y_pre:y_post){
      td<-c(td,BAA_dict[[year]][row,col]) #valore nell'anno della restoration}
    }
    BAA_trend<-c(BAA_trend,lm(1:length(td)~td)[[1]][2])
  }
}



df<-cbind(df,SSTmean_y, SSTmean_pre, SSTmean_trend, SSTmax_y, SSTmax_pre, SSTmax_trend, 
              SSTmin_y, SSTmin_pre, SSTmin_trend, SSTA_mean_y, SSTA_mean_pre, SSTA_mean_trend,
              SSTA_max_y, SSTA_max_pre, SSTA_max_trend, SSTA_min_y, SSTA_min_pre, SSTA_min_trend,
              HS_y, HS_pre, HS_trend, BAA_y, BAA_pre, BAA_trend)

###CORAL GLOBAL DIVERSITY

ocean_05<-raster("~/Desktop/PhD project/corals R/data/ocean_05.tif")
plot(ocean_05,col="lightblue")
o<-as.matrix(ocean_05)   #NAs correspond to land cells

corals<- list.files(path="~/Desktop/PhD project/corals R/data/coral_ranges", pattern =".tif$", full.names=TRUE)


n<-length(corals)

mlat<-360
mlon<-720
corals_rich<-matrix(rep(0,mlat*mlon),nrow=mlat,ncol=mlon)

for(i in 1:n){    
  coral<-raster(paste0(corals[i]))
  m<-as.matrix(coral)
  m[which(is.na(m))]<-0
  m<-1*(m>0.9)    #turn suitability matrix into binary matrix (threshold=0.9)
  m<-m*o  #multiply fish matrix x ocean matrix (keep NAs in ocean matrix to exclude land cells)
  corals_rich<-corals_rich+m   #add cell values to diversity matrix
  print(i)
}

crich_gl<-c()
for(r in 1:nrow(corals_rich)){
  for(c in 1:ncol(corals_rich)){
    if(!is.na(corals_rich[r,c])){     #exclude land cells (NAs)
      crich_gl<-rbind(crich_gl,c(r,c,corals_rich[r,c]))
    }
  }
}
crich_gl<-data.frame(lat=crich_gl[,1],lon=crich_gl[,2],corals_rich=crich_gl[,3])

head(crich_gl)

#ADD CORAL RICHNESS
dim(corals_rich)
div<-c()
for (i in 1:nrow(df)){
  r<-df[i,1]+1
  c<-df[i,2]+1
  div<-c(div,corals_rich[r,c])}

df<-cbind(df,div)
          
write.csv(df,"FINAL_DATASET.csv",row.names=F)
