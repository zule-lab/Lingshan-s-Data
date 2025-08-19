library(ggplot2)
library(plotly)
library(readxl)


#Load data
data<-read.csv(file.choose())
attach(data)

#MODEL1
fit1<-lm(LST_2020~HV+LV)
summary(fit1)
fit1_stand<-lm(scale(LST_2020)~0+scale(HV)+scale(LV))
summary(fit1_stand)
#MODEL2
fit2<-lm(LST_2020~HV+LV+EMS_HV+LPI_HV+MPSh_HV)
summary(fit2)

fit2_stand<-lm(scale(LST_2020)~0+scale(HV)+scale(LV)+scale(EMS_HV)+scale(LPI_HV)
               +scale(MPSh_HV))
summary(fit2_stand)
#MODEL3
fit3<-lm(LST_2020~HV+LV+EMD_HV+LPI_HV+MPSh_HV)
summary(fit3)

fit3_stand<-lm(scale(LST_2020)~0+scale(HV)+scale(LV)+scale(EMD_HV)+scale(LPI_HV)
               +scale(MPSh_HV))
summary(fit3_stand)

#MODEL4
fit4<-lm(LST_2020~HV+LV+EMS_LV+LPI_LV+MPSh_LV)
summary(fit4)

fit4_stand<-lm(scale(LST_2020)~0+scale(HV)+scale(LV)+scale(EMS_LV)+scale(LPI_LV)
               +scale(MPSh_LV))
summary(fit4_stand)

#MODEL5
fit5<-lm(LST_2020~HV+LV+EMD_LV+LPI_LV+MPSh_LV)
summary(fit5)

fit5_stand<-lm(scale(LST_2020)~0+scale(HV)+scale(LV)+scale(EMD_LV)+scale(LPI_LV)
               +scale(MPSh_LV))
summary(fit5_stand)


par(mfrow=c(2,2)) 
plot(fit)