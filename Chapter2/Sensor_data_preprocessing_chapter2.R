# Sensor data preprocessing
# Extract the daily maximum, minimum, mean and hourly data

p <- c("timetk","dplyr","stringr","lubridate")
lapply(p, library, character.only=T)

#My laptop
file_lst<-dir(path="C:/Users/umroot/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/summer2022/Unedited/",pattern="csv",recursive=T,full.name=T)
Name<-str_sub(file_lst,119,-5)
#University computer
file_lst<-dir(path="/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/summer2022/Unedited/",pattern="csv",recursive=T,full.name=T)
Name<-str_sub(file_lst,119,-5)

for (i in 39:41){
  data<-read.csv(grep(pattern=Name[i],file_lst,value=T))
  data$YMDH <- strftime(data$Date.Time, "%Y-%m-%d-%H")
  data$YMD <- strftime(data$Date.Time, "%Y-%m-%d")
  data$hour<-strftime(data$Date.Time, "%H")
  data$Temp<-as.numeric(data$Temp)
  data$RH<-as.numeric(data$RH)
  data$Locat<-str_sub(file_lst[i],-5,-5)
  data$ID<-str_sub(file_lst[i],119,-7)
  data$Name<-str_sub(file_lst[i],119,-5)
  
  data1 <- data %>%
    group_by(YMDH) %>% # group by the hour column
    summarise(Temperature=mean(Temp,na.rm=T),Date.Time = first (Date.Time),Humidity=mean(RH,na.rm=T),Location = first(Locat),ID= first(ID), Name = first(Name)) 
  data1<-na.omit(data1)
  data1$hour<-strftime(data1$Date.Time, "%H")
  write.csv(data1, file=paste('/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Hourly/',Name[i],'.csv',sep=''))
  
  data2 <- data %>%
    group_by(YMD) %>% # group by the day column
    slice_max(order_by = Temp, n = 1, with_ties = FALSE)
  data2<-na.omit(data2)
  data2$hour<-strftime(data2$Date.Time, "%H")
  write.csv(data2, file=paste('/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Daily_maximum/',Name[i],'.csv',sep=''))
  
  data3 <- data %>%
    group_by(YMD) %>% # group by the day column
    slice_min(order_by = Temp, n = 1, with_ties = FALSE)
  data3$hour<-strftime(data3$Date.Time, "%H")
  write.csv(data3, file=paste('/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Daily_minimum/',Name[i],'.csv',sep=''))
}

#Merge the csv

#maximum
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Daily_maximum/") 
csv_files <- list.files(pattern = ".csv")
# Read and combine all CSV files into one data frame
combined_df <- bind_rows(lapply(csv_files, read.csv))
write.csv(combined_df, "daily_maximum_summary.csv", row.names = FALSE)


#minimum
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Daily_minimum/") 
csv_files <- list.files(pattern = ".csv")
# Read and combine all CSV files into one data frame
combined_df <- bind_rows(lapply(csv_files, read.csv))
write.csv(combined_df, "daily_minimum_summary.csv", row.names = FALSE)

#hourly
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/Summer2022_new/Hourly/") 
csv_files <- list.files(pattern = ".csv")
# Read and combine all CSV files into one data frame
combined_df <- bind_rows(lapply(csv_files, read.csv))
write.csv(combined_df, "hourly_summary.csv", row.names = FALSE)


#Add the collumn of tempearture difference in each location relative to the refernece sensor
AT<-read.csv('/Users/L_INGSHA/OneDrive - Concordia University - Canada/Data/Concordia/Processed_data/Credosense/summer2022/Hourly/Summary_bylocation/Summary.csv')
T_dif<-rep(NA,times = 27996)
for (i in 1:27996) {
  if(length(AT$Temperature[AT$YMDH == AT$YMDH[i] & AT$Location == AT$Location[i] & AT$ID == AT$ID[i]]-AT$Temperature[AT$YMDH == AT$YMDH[i] & AT$Location == 4 & AT$ID == AT$ID[i]])>0){
    T_dif[i]<-AT$Temperature[AT$YMDH == AT$YMDH[i] & AT$Location == AT$Location[i] & AT$ID == AT$ID[i]]-AT$Temperature[AT$YMDH == AT$YMDH[i] & AT$Location == 4 & AT$ID == AT$ID[i]]
  }
  else{
    T_dif[i]<- NA
  }
}
AT$T_Dif<-T_dif

H_dif<-rep(NA,times = 27996)
for (i in 1:27996) {
  if(length(AT$Humidity[AT$YMDH == AT$YMDH[i] & AT$Location == AT$Location[i] & AT$ID == AT$ID[i]]-AT$Humidity[AT$YMDH == AT$YMDH[i] & AT$Location == 4 & AT$ID == AT$ID[i]])>0){
    H_dif[i]<-AT$Humidity[AT$YMDH == AT$YMDH[i] & AT$Location == AT$Location[i] & AT$ID == AT$ID[i]]-AT$Humidity[AT$YMDH == AT$YMDH[i] & AT$Location == 4 & AT$ID == AT$ID[i]]
  }
  else{
    H_dif[i]<- NA
  }
}
AT$H_Dif<-H_dif

AT$Date<-strptime(AT$YMDH, "%Y-%m-%d-%H")
AT$hour<-hour(AT$Date)
AT$DayorNight<- 'N'
AT$DayorNight[AT$hour>6 & AT$hour<20]<- 'D'

Data<-AT
attach(Data)
#Caluclate the wet-bulb temperature
Data$Tw<- Temperature* atan(0.151977 * (Humidity + 8.313659)^0.5) + 
  atan(Temperature + Humidity) - 
  atan(Humidity - 1.676331) + 
  0.00391838 * (Humidity^1.5) * atan(0.023101 * Humidity) - 4.686035
#Calculate air temperature difference
for (i in 1:length(Data$X)) {
  matching_row <- which(Data$Location == 4 & 
                          Data$YMDH == Data$YMDH[i] & 
                          Data$ID == Data$ID[i] & 
                          Data$hour == Data$hour[i])
  if (length(matching_row) > 0) {
    Data$T_Dif[i] <- Data$Temperature[i] - Data$Temperature[matching_row[1]]
  } else {
    Data$T_Dif[i] <- NA
  }
  
}
#Calculate relative humidity difference
for (i in 1:length(Data$X)) {
  matching_row <- which(Data$Location == 4 & 
                          Data$YMDH == Data$YMDH[i] & 
                          Data$ID == Data$ID[i] & 
                          Data$hour == Data$hour[i])
  if (length(matching_row) > 0) {
    Data$H_Dif[i] <- Data$Humidity[i] - Data$Humidity[matching_row[1]]
  } else {
    Data$H_Dif[i] <- NA
  }
  
}
#Calculate wet-bulb temperature difference
for (i in 1:length(Data$X)) {
  matching_row <- which(Data$Location == 4 & 
                          Data$YMDH == Data$YMDH[i] & 
                          Data$ID == Data$ID[i] & 
                          Data$hour == Data$hour[i])
  if (length(matching_row) > 0) {
    Data$Tw_Dif[i] <- Data$Tw[i] - Data$Tw[matching_row[1]]
  } else {
    Data$Tw_Dif[i] <- NA
  }
  
}
#Remove the NA rows
df<-na.omit(Data)
write.csv(df, file = 'Summary_hourly_add_dif(reference included).csv')
#Remove the outside reference sites
df_new<-df[df$Location != 4,]
#Save the data
write.csv(df_new, file = 'Summary_hourly.csv')

