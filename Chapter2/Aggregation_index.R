#Calculate the aggregation index

p <- c("raster","stringr","sp","landscapemetrics")
lapply(p, library, character.only=T)

file_mci<-dir(path="D:/Lingshan/ArcGIS/Chapter2/MCI_rasters/",pattern="tif",recursive=T,full.name=T)
ID<-str_sub(file_mci,41,-5)

summary<-array(NA,dim=c(2,26))
for(i in 1:26){
  raster_data<-raster(grep(pattern=ID[i],file_mci,value=T))
  AI_result <- lsm_c_ai(raster_data)
  summary[1,i]<-ID[i]
  summary[2,i]<- AI_result$value[AI_result$class == 4]
}

Table<-data.frame(ID=summary[1,], AI=summary[2,])
write.csv(Table, file="D:/Lingshan/ArcGIS/Chapter2/AI_table.csv")


