#Select date under different weather condition
Packages <- c( "dplyr","ggplot2","gghalves")
lapply(Packages, library, character.only = TRUE)

setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")

Hourly<- read.csv("Summary_hourly_add_dif.csv")


#Select the sunny days
Hourly$Date <- as.Date(Hourly$Date.Time)
sunny_dates<-as.Date(c("2022-08-24","2022-08-29","2022-09-02","2022-09-03","2022-09-07","2022-09-08",
          "2022-09-09","2022-09-10","2022-09-11","2022-09-15","2022-09-24","2022-09-30","2022-10-03"))
df_sunny <- Hourly %>%
  filter(Date %in% sunny_dates)

#Select the cloudy days

cloudy_dates<-as.Date(c("2022-08-26","2022-08-27","2022-09-20","2022-09-21","2022-09-22","2022-09-25",
        "2022-09-26","2022-09-28", "2022-10-01","2022-10-06"))

df_cloudy <- Hourly %>%
  filter(Date %in% cloudy_dates)

#Select the rainy days
rainy_dates <- as.Date(c("2022-09-13","2022-09-19","2022-09-18","2022-09-27"))
df_rainy <- Hourly %>%
  filter(Date %in% rainy_dates)
                             


#Set up the color palette
ordercolors<-c("coral1","lightblue")

#Plot the Air temperature difference during sunny days
df_sunny$Location<-as.factor(df_sunny$Location)
df_sunny$DayorNight<- 'N'
df_sunny$DayorNight[df_sunny$hour>6 & df_sunny$hour<20]<- 'D'

#Extract the specific hours for hottest and coolest period
df_sunny<- df_sunny[df_sunny$hour %in% c(0:3,11:15, 23),]

ggplot(data = df_sunny,
       aes(x=Location, y=T_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-6,4) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Temperature Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))

#Plot the Relative Humidity difference during sunny days
ggplot(data = df_sunny,
       aes(x=Location, y=H_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-20,25) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Relative Humidity Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))

#Plot the Wetbulb Temperature difference during sunny days
ggplot(data = df_sunny,
       aes(x=Location, y=Tw_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_sunny, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-6,4) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Wet-bulb Temperature Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))


#Plot the Air temperature difference during cloudy days
df_cloudy$Location<-as.factor(df_cloudy$Location)
df_cloudy$DayorNight<- 'N'
df_cloudy$DayorNight[df_cloudy$hour>6 & df_cloudy$hour<20]<- 'D'
#Extract the specific hours for hottest and coolest period
df_cloudy<- df_cloudy[df_cloudy$hour %in% c(0:3,11:15, 23),]

ggplot(data = df_cloudy,
       aes(x=Location, y=T_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-6,4) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Temperature Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))


#Plot the Relative Humidity difference during cloudy days
ggplot(data = df_cloudy,
       aes(x=Location, y=H_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-20,25) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Relative Humidity Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))

#Plot the Wetbulb Temperature difference during cloudy days
ggplot(data = df_cloudy,
       aes(x=Location, y=Tw_Dif, fill=DayorNight)) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "N"),side = "r",aes(fill = DayorNight), color=NA, alpha=0.35) +
  geom_half_violin(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", color=NA, alpha=0.35) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "N"),aes(fill = DayorNight),side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  geom_half_boxplot(data = subset(df_cloudy, DayorNight == "D"),aes(fill = DayorNight),side = "l", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  scale_fill_manual(values = ordercolors,labels = c("Day","Night")) +
  scale_y_continuous(limits = c(-6,4) ) +
  scale_x_discrete(labels = c('Grass','Discrete Tree cluster','Aggregate Tree Cluster')) +
  geom_hline(yintercept=0, linetype='dotted', col = 'darkgrey',size = 1.25)+
  labs(y="Wet-bulb Temperature Difference Relative to the Outside Reference (°C)",x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 20, color = "black"),
        axis.text = element_text(size=20, color = "black"))

