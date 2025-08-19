p <- c("stringr","sp","raster","dplyr","tidyverse","terra","rgdal")
lapply(p, library, character.only=T)

#census tract scale: resample the LST to the spatial resolution of MCI data
file_mci<-dir(path="E:/Data/Concordia/Processed_data/Metropolitan Canopy Index/2021_MCI/Census_tract_raster/",pattern="tif",recursive=T,full.name=T)
ID<-str_sub(file_mci,89,-5)
file_lst<-dir(path="E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2020_LST/Census_clip_2020/",pattern="tif",recursive=T,full.name=T)

for(i in 1:543){
  a<-raster(grep(pattern=ID[i],file_mci,value=T))
  b<-raster(grep(pattern=ID[i],file_lst,value=T))
  resample(b,a,method='bilinear',file=paste('E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2020_LST/Census_clip_2020_resample_2021/',ID[i],'.tif',sep=''))
}



file_lst_2020<-dir(path="E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2020_LST/Census_clip_2020_resample_2021/",pattern="tif",recursive=T,full.name=T)
file_lst_2021<-dir(path="E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2021_LST/Census_clip_2021_resample/",pattern="tif",recursive=T,full.name=T)
ID<-str_sub(file_lst_2021,107,-5)
file_mci<-dir(path="E:/Data/Concordia/Processed_data/Metropolitan Canopy Index/2021_MCI/Census_tract_raster/",pattern="tif",recursive=T,full.name=T)


for(i in 1:543){
  lst<-raster(grep(pattern=ID[i],file_lst_2020,value=T))
  LST<-getValues(lst)
  lst1<-raster(grep(pattern=ID[i],file_lst_2021,value=T))
  LST_1<-getValues(lst1)
  mci<-raster(grep(pattern=ID[i],file_mci,value=T))
  MCI_v<-getValues(mci)
  df<-data.frame(LST_2020 = LST,LST_2021 = LST_1, MCI = MCI_v)
  df<-na.omit(df)
  write.csv(df, file=paste('E:/Data/Concordia/Processed_data/CSV/Composition/Census_tract_2020_2021/',ID[i],'.csv',sep=''))
}

#get composition metrics Census Tracts
file_lst_1<-dir(path="E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2021_LST/Census_clip_2021_resample/",pattern="tif",recursive=T,full.name=T)
ID<-str_sub(file_lst_1,113,-5)
file_csv<-dir(path="E:/Data/Concordia/Processed_data/CSV/Composition/Census_tract_2020_2021/",pattern="csv",recursive=T,full.name=T)
summ<-array(NA,dim=c(length(file_lst_1),7))
for(i in 1:length(file_csv)){
  csv<-read.csv(file_csv[i])
  summ[i,1]<-sum(csv$MCI==1)/(sum(csv$MCI==0)+sum(csv$MCI==1)+sum(csv$MCI==2)+sum(csv$MCI==3)+sum(csv$MCI==4))
  summ[i,2]<-sum(csv$MCI==2)/(sum(csv$MCI==0)+sum(csv$MCI==1)+sum(csv$MCI==2)+sum(csv$MCI==3)+sum(csv$MCI==4))
  summ[i,3]<-sum(csv$MCI==3)/(sum(csv$MCI==0)+sum(csv$MCI==1)+sum(csv$MCI==2)+sum(csv$MCI==3)+sum(csv$MCI==4))
  summ[i,4]<-sum(csv$MCI==4)/(sum(csv$MCI==0)+sum(csv$MCI==1)+sum(csv$MCI==2)+sum(csv$MCI==3)+sum(csv$MCI==4))
  summ[i,5]<-sum(csv$MCI==5)/(sum(csv$MCI==0)+sum(csv$MCI==1)+sum(csv$MCI==2)+sum(csv$MCI==3)+sum(csv$MCI==4)+sum(csv$MCI==5))
  summ[i,6]<-mean(csv$LST_2020[csv$MCI!=5])
  summ[i,7]<-mean(csv$LST_2021[csv$MCI!=5])
}
df<-data.frame(CensusTract=ID,LM=summ[,1],HM=summ[,2],LV=summ[,3],HV=summ[,4],W=summ[,5],LST_2020=summ[,6],LST_2021=summ[,7])
write.csv(df,file="Concordia/Processed_data/CSV/Composition/Composition_2021_2020_census_tract.csv")


#Configuration  Borough
file_lst<-dir(path="E:/Data/Concordia/Processed_data/Urban Heat Island/Lansat8_collection2/2021_LST/Borough_clip_2021_resample/",pattern="tif",recursive=T,full.name=T)
ID<-str_sub(file_lst,112,-5)
configuration<-dir(path="E:/Data/Concordia/Processed_data/CSV/Configuration/Boroughs_2021/",pattern="csv",recursive=T,full.name=T)

summ<-array(NA,dim=c(34,4,4))
for(i in 1:34){
  df<-read.csv(file=configuration[i])
  #effective mesh size
  summ[i,1,1]<-sum((df$Area[df$DN==4])^2)/(sum(df$Area[df$DN==4])*10000)
  summ[i,2,1]<-sum(df$Area[df$DN==3]^2)/(sum(df$Area[df$DN==3])*10000)
  summ[i,3,1]<-sum(df$Area[df$DN==2]^2)/(sum(df$Area[df$DN==2])*10000)
  summ[i,4,1]<-sum(df$Area[df$DN==1]^2)/(sum(df$Area[df$DN==1])*10000)
  #effective mesh 
  summ[i,1,2]<-sum(df$Area[df$DN==4])*10000/sum((df$Area[df$DN==4])^2)
  summ[i,2,2]<-sum(df$Area[df$DN==3])*10000/sum((df$Area[df$DN==3])^2)
  summ[i,3,2]<-sum(df$Area[df$DN==2])*10000/sum((df$Area[df$DN==2])^2)
  summ[i,4,2]<-sum(df$Area[df$DN==1])*10000/sum((df$Area[df$DN==1])^2)
  #Largest patch index
  summ[i,1,3]<-max(df$Area[df$DN==4])/sum(df$Area[df$DN==4])
  summ[i,2,3]<-max(df$Area[df$DN==3])/sum(df$Area[df$DN==3])
  summ[i,3,3]<-max(df$Area[df$DN==2])/sum(df$Area[df$DN==2])
  summ[i,4,3]<-max(df$Area[df$DN==1])/sum(df$Area[df$DN==1])
  #Mean patch shape index
  summ[i,1,4]<-sum(0.25*df$Perimeter[df$DN==4]/sqrt(df$Area[df$DN==4]))/length(df$DN[df$DN==4])
  summ[i,2,4]<-sum(0.25*df$Perimeter[df$DN==3]/sqrt(df$Area[df$DN==3]))/length(df$DN[df$DN==3])
  summ[i,3,4]<-sum(0.25*df$Perimeter[df$DN==2]/sqrt(df$Area[df$DN==2]))/length(df$DN[df$DN==2])
  summ[i,4,4]<-sum(0.25*df$Perimeter[df$DN==1]/sqrt(df$Area[df$DN==1]))/length(df$DN[df$DN==1])
  
}
df<-data.frame(Borough=ID,EMS_HV=summ[,1,1],EMS_LV=summ[,2,1],EMS_HM=summ[,3,1],EMS_LM=summ[,4,1],EMD_HV=summ[,1,2],EMD_LV=summ[,2,2],EMD_HM=summ[,3,2],EMD_LM=summ[,4,2],LPI_HV=summ[,1,3],LPI_LV=summ[,2,3],LPI_HM=summ[,3,3],LPI_LM=summ[,4,3],MPSh_HV=summ[,1,4],MPSh_LV=summ[,2,4],MPSh_HM=summ[,3,4],MPSh_LM=summ[,4,4])
write.csv(df,file="E:/Data/Concordia/Processed_data/CSV/Configuration/Configuration_borough_2021_2020.csv")


composition<-read.csv(file="Concordia/Processed_data/CSV/Composition/Composition_2021_2020_borough.csv")
configuration<-read.csv(file="Concordia/Processed_data/CSV/Configuration/Configuration_borough_2021_2020.csv")
dataframe<- merge(composition,configuration,by="Borough")
write.csv(dataframe,file='Concordia/Processed_data/CSV/Final/Spatial_pattern_2020_2021.csv')




#Calculate configuration metrics Census Tract
file_com<-dir(path="E:/Data/Concordia/Processed_data/CSV/Composition/Census_tract_2020_2021/",pattern="csv",recursive=T,full.name=T)
ID<-str_sub(file_com,79,-5)
file_csv<-dir(path="E:/Data/Concordia/Processed_data/CSV/Configuration/Cencus_tracts_2021/",pattern="csv",recursive=T,full.name=T)

summ<-array(NA,dim=c(length(file_com),4,4))
for(i in 1:length(file_com)){
  df<-read.csv(file=grep(pattern=ID[i],file_csv,value=T))
  df<-na.omit(df)
  #effective mesh size
  summ[i,1,1]<-sum((df$Area[df$DN==4])^2)/(sum(df$Area[df$DN==4])*10000)
  summ[i,2,1]<-sum(df$Area[df$DN==3]^2)/(sum(df$Area[df$DN==3])*10000)
  summ[i,3,1]<-sum(df$Area[df$DN==2]^2)/(sum(df$Area[df$DN==2])*10000)
  summ[i,4,1]<-sum(df$Area[df$DN==1]^2)/(sum(df$Area[df$DN==1])*10000)
  #effective mesh 
  summ[i,1,2]<-sum(df$Area[df$DN==4])*10000/sum((df$Area[df$DN==4])^2)
  summ[i,2,2]<-sum(df$Area[df$DN==3])*10000/sum((df$Area[df$DN==3])^2)
  summ[i,3,2]<-sum(df$Area[df$DN==2])*10000/sum((df$Area[df$DN==2])^2)
  summ[i,4,2]<-sum(df$Area[df$DN==1])*10000/sum((df$Area[df$DN==1])^2)
  #Largest patch index
  summ[i,1,3]<-max(df$Area[df$DN==4])/sum(df$Area[df$DN==4])
  summ[i,2,3]<-max(df$Area[df$DN==3])/sum(df$Area[df$DN==3])
  summ[i,3,3]<-max(df$Area[df$DN==2])/sum(df$Area[df$DN==2])
  summ[i,4,3]<-max(df$Area[df$DN==1])/sum(df$Area[df$DN==1])
  #Mean patch shape index
  summ[i,1,4]<-sum(0.25*df$Perimeter[df$DN==4]/sqrt(df$Area[df$DN==4]))/length(df$DN[df$DN==4])
  summ[i,2,4]<-sum(0.25*df$Perimeter[df$DN==3]/sqrt(df$Area[df$DN==3]))/length(df$DN[df$DN==3])
  summ[i,3,4]<-sum(0.25*df$Perimeter[df$DN==2]/sqrt(df$Area[df$DN==2]))/length(df$DN[df$DN==2])
  summ[i,4,4]<-sum(0.25*df$Perimeter[df$DN==1]/sqrt(df$Area[df$DN==1]))/length(df$DN[df$DN==1])
}
dataframe<-data.frame(CensusTract=ID,EMS_HV=summ[,1,1],EMS_LV=summ[,2,1],EMS_HM=summ[,3,1],EMS_LM=summ[,4,1],EMD_HV=summ[,1,2],EMD_LV=summ[,2,2],EMD_HM=summ[,3,2],EMD_LM=summ[,4,2],LPI_HV=summ[,1,3],LPI_LV=summ[,2,3],LPI_HM=summ[,3,3],LPI_LM=summ[,4,3],MPSh_HV=summ[,1,4],MPSh_LV=summ[,2,4],MPSh_HM=summ[,3,4],MPSh_LM=summ[,4,4])

write.csv(dataframe,file="E:/Data/Concordia/Processed_data/CSV/Configuration/Configuration_census_2020_2021.csv")


#Combine composition and configuration metrics Census tract
a<-read.csv("Concordia/Processed_data/CSV/Composition/Composition_2021_2020_census_tract.csv")
b<-read.csv('Concordia/Processed_data/CSV/Configuration/Configuration_census_2020_2021.csv')
a<-na.omit(a)
b<-na.omit(b)
df<- merge(a,b,by="CensusTract")
df$Census<-sprintf("%0.2f",df$CensusTract)
write.csv(df,file="Concordia/Processed_data/CSV/Final/Spatial_pattern_2020_2021_censustract.csv")


