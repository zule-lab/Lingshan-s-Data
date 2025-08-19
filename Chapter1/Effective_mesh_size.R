
#Load the packages
p <- c("stringr","sp","raster","dplyr","tidyverse","terra","rgdal")
lapply(p, library, character.only=T)


setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/")

configuration<-dir(path="Data/Concordia/Processed_data/CSV/Configuration/Boroughs/",pattern="csv",recursive=T,full.name=T)
ID<-str_sub(configuration,62,-5)
summ<-array(NA,dim=c(34,2))
for(i in 1:34){
  data<-read.csv(configuration[i])
  #effective mesh size
  area1<-data[data$DN==4,]$Area
  areasq<-area1^2
  sum<-sum(area1)
  sumsq<-sum(areasq)
  ems<-sumsq/sum
  summ[i,1]<-ems
  summ[i,2]<-ID[i]
}


sum(data[data$DN==4,]$Area^2)/sum(data[data$DN==4,]$Area)


