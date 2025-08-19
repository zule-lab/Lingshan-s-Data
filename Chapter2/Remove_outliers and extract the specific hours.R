##Load packages
Packages.6 <- c("tidyverse", "dplyr")
lapply(Packages.6, library, character.only = TRUE)
#My own laptop
setwd("/Users/umroot/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")
#University computer
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")



#hourly data
Hourly.Meta<- read.csv("Summary_hourly_add_dif.csv")
Hourly.Meta$Location<-as.factor(Hourly.Meta$Location)
Hourly.Meta$ID<-as.factor(Hourly.Meta$ID)
Hourly.Meta$YMD <- strftime(Hourly.Meta$Date, "%Y-%m-%d")
Hourly.Meta$DayorNight<-"N"
Hourly.Meta$DayorNight[Hourly.Meta$hour > 6 & Hourly.Meta$hour < 20]<-"D"
# Detect and remove the outliers

Q1_hum <- quantile(Hourly.Meta$H_Dif, 0.25, na.rm = T)
Q3_hum <- quantile(Hourly.Meta$H_Dif, 0.75, na.rm = T)
IQR_tem <- Q3_hum - Q1_hum
lower_bound_hum <- Q1_hum - 1.5 * IQR_tem
upper_bound_hum <- Q3_hum + 1.5 * IQR_tem
Hourly_hum_rm<-Hourly.Meta[Hourly.Meta$H_Dif< upper_bound_hum & Hourly.Meta$H_Dif >lower_bound_hum,]


##


#Extract the specific hours for daytime and nighttime and summarize the daily average
Hourly_extract<- Hourly_hum_rm[Hourly_hum_rm$hour %in% c(0:3,10:15, 22),]
summary_df <- Hourly_extract %>%
  group_by(YMD,Location,ID,DayorNight ) %>%
  summarise(
    Temperature = mean(Temperature),
    Humidity = mean(Humidity),
    T_Difference = mean(T_Dif),
    H_Difference = mean(H_Dif),
    Tw = mean(Tw),
    Tw_Difference = mean(Tw_Dif)
  )

#Save the data
write.csv(summary_df, file = 'hourly_final(7 parks).csv')


