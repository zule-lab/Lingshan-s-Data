
##Load packages
Packages.6 <- c("broom.mixed", "lme4", "ggpubr", "lmerTest", "sjPlot", "car", "effects", "multcomp", "dplyr", "glmmTMB", "stats")
lapply(Packages.6, library, character.only = TRUE)
#My own laptop
setwd("/Users/umroot/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")
#University computer
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")

summary_df<-read.csv("hourly_final(all parks).csv")
summary_df$Location<-as.factor(summary_df$Location)
summary_df$ID<-as.factor(summary_df$ID)
attach(summary_df)




# Extract sunny days
summary_df$Date <- as.Date(summary_df$YMD)
sunny_dates<-as.Date(c("2022-08-24","2022-08-29","2022-09-02","2022-09-03","2022-09-07","2022-09-08",
                       "2022-09-09","2022-09-10","2022-09-11","2022-09-15","2022-09-24","2022-09-30","2022-10-03"))
summary_df <- summary_df %>%
  filter(Date %in% sunny_dates)


#Extract Cloudy days
summary_df$Date <- as.Date(summary_df$YMD)
cloudy_dates<-as.Date(c("2022-08-26","2022-08-27","2022-09-20","2022-09-21","2022-09-22","2022-09-25",
                        "2022-09-26","2022-09-28", "2022-10-01","2022-10-06"))
summary_df <- summary_df %>%
  filter(Date %in% cloudy_dates)

#Model build

model.tem.hourly<- lmer(T_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)
summary(model.tem.hourly)

model.hum.hourly<- lmer(H_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)
summary(model.hum.hourly)

model.Twdif.hourly<- lmer(Tw_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)
summary(model.Twdif.hourly)


#  MODEL DIAGNOSTICS

plot(model.tem.hourly, which = 1)
shapiro.test(resid(model.tem.hourly))
hist(residuals(model.tem.hourly))
qqnorm(residuals(model.tem.hourly))
qqline(residuals(model.tem.hourly))

plot(model.hum.hourly, which = 1)
shapiro.test(resid(model.hum.hourly))
hist(residuals(model.hum.hourly))
qqnorm(residuals(model.hum.hourly))
qqline(residuals(model.hum.hourly))

plot(model.Twdif.hourly, which = 1)
shapiro.test(resid(model.Twdif.hourly))
hist(residuals(model.Twdif.hourly))
qqnorm(residuals(model.Twdif.hourly))
qqline(residuals(model.Twdif.hourly))





