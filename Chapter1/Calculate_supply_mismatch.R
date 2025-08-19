
#New model's result 


data<-read.csv(file.choose())  ##

data$Pre_LST_dif<-14.12*data$HV+7.61*data$LV+1.95*data$LPI_HV
data$Supply<-(data$Pre_LST_dif-min(data$Pre_LST_dif))/(max(data$Pre_LST_dif)-min(data$Pre_LST_dif))

data$Error2020<-data$Obs_2020-data$Pre_LST_dif
data$Error2021<-data$Obs_2021-data$Pre_LST_dif

data$Mismatch<-data$Supply-data$Demand
write.csv(data, file = '/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter1/Chapter1_result_final.csv')





