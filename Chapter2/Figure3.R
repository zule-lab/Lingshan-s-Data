#Load the packages
p <- c("ggplot2","dplyr","tidyverse")
lapply(p, library, character.only=T)

setwd("/Users/umroot/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")

setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")

#Load the Hourly data
Data<-read.csv('Summary_hourly_add_dif.csv')
Data<-Data[Data$Location<4,]
Data<-na.omit(Data)
#Remove the outliers
Q1_hum <- quantile(Data$H_Dif, 0.25, na.rm = T)
Q3_hum <- quantile(Data$H_Dif, 0.75, na.rm = T)
IQR_tem <- Q3_hum - Q1_hum
lower_bound_hum <- Q1_hum - 1.5 * IQR_tem
upper_bound_hum <- Q3_hum + 1.5 * IQR_tem
Hourly_hum_rm<-Data[Data$H_Dif< upper_bound_hum & Data$H_Dif >lower_bound_hum,]

#Select the sunny days
Hourly_hum_rm$Date <- as.Date(Hourly_hum_rm$Date.Time)
sunny_dates<-as.Date(c("2022-08-24","2022-08-29","2022-09-02","2022-09-03","2022-09-07","2022-09-08",
                       "2022-09-09","2022-09-10","2022-09-11","2022-09-15","2022-09-24","2022-09-30","2022-10-03"))
Hourly_hum_rm <- Hourly_hum_rm %>%
  filter(Date %in% sunny_dates)


# Get the mean and quantiles of air temperature difference, relative humidity difference and wet-bulb temperature difference
summary_df <- Hourly_hum_rm %>%
  group_by(hour,Location) %>%
  summarise(
    Temperature = mean(Temperature),
    Humidity = mean(Humidity),
    T_Difference = mean(T_Dif),
    T_Difference_75 = quantile(T_Dif, 0.75),
    T_Difference_25 = quantile(T_Dif,0.25),
    H_Difference = mean(H_Dif),
    H_Difference_75 = quantile(H_Dif, 0.75),
    H_Difference_25 = quantile(H_Dif, 0.25),
    Tw_Difference = mean(Tw_Dif),
    Tw_Difference_75 = quantile(Tw_Dif, 0.75),
    Tw_Difference_25 = quantile(Tw_Dif,0.25),
  )
#Make the Location from numeric to factor
summary_df$Location<-as.factor(summary_df$Location)

#Set up the color palette
ordercolors<-c("coral1","olivedrab3","darkgreen")


#Plot Air Temperature difference series line chart
ggplot(summary_df, aes(x= hour, color = Location)) +
  geom_point(aes(y = T_Difference), size = 2) +
  scale_color_manual(values = ordercolors,labels = c("Grassland","Discrete Tree Cluster","Aggregate Tree Cluster")) +
  geom_line(aes(y = T_Difference, group = Location)) + 
  geom_errorbar(aes(ymin = T_Difference_25, 
                    ymax = T_Difference_75),
                width = 0.5, size = 1, position = position_dodge(0.9)) +
  geom_hline(yintercept=0, linetype='dotted', col = 'black',size = 1.25)+
  scale_x_continuous(name = "Hour", 
                     breaks = seq(0, 24, by = 1),  # Adjust breaks for hour
                     limits = c(0, 24)) + 
  scale_y_continuous(name = "Temperature Difference" 
  ) +  # Set limits for Temperature
  labs(title = "Hourly Temperature difference",
       x = "Hour") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18, color = "black"),
        axis.text = element_text(size=18, color = "black"))


#Plot relative Humidity difference series line chart
ggplot(summary_df, aes(x= hour, color = Location)) +
  geom_point(aes(y = H_Difference), size = 2) +
  scale_color_manual(values = ordercolors,labels = c("Grass","Discrete Tree Cluster","Aggregate Tree Cluster")) +
  geom_line(aes(y = H_Difference, group = Location)) + 
  geom_errorbar(aes(ymin = H_Difference_25, 
                    ymax = H_Difference_75),
                width = 0.5, size = 1, position = position_dodge(0.9)) +
  geom_hline(yintercept=0, linetype='dotted', col = 'black',size = 1.25)+
  scale_x_continuous(name = "Hour", 
                     breaks = seq(0, 24, by = 1),  # Adjust breaks for hour
                     limits = c(0, 24)) + 
  scale_y_continuous(name = "Relative Humidity Difference") +  # Set limits for Humidity
  labs(title = "Hourly relative Humidity difference ",
       x = "Hour") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18, color = "black"),
        axis.text = element_text(size=18, color = "black"))

#Plot Wet-Bulb Temperature difference series line chart
ggplot(summary_df, aes(x= hour, color = Location)) +
  geom_point(aes(y = Tw_Difference), size = 2) +
  scale_color_manual(values = ordercolors,labels = c("Grassland","Discrete Tree Cluster","Aggregate Tree Cluster")) +
  geom_line(aes(y = Tw_Difference, group = Location)) + 
  geom_errorbar(aes(ymin = Tw_Difference_25, 
                    ymax = Tw_Difference_75),
                width = 0.5, size = 1, position = position_dodge(0.9)) +
  geom_hline(yintercept=0, linetype='dotted', col = 'black',size = 1.25)+
  scale_x_continuous(name = "Hour", 
                     breaks = seq(0, 24, by = 1),  # Adjust breaks for hour
                     limits = c(0, 24)) + 
  scale_y_continuous(name = "Wet-Bulb Temperature Difference" 
  ) +  # Set limits for Temperature
  labs(title = "Hourly Wet-Bulb Temperature difference",
       x = "Hour") +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 18, color = "black"),
        axis.text = element_text(size=18, color = "black"))
