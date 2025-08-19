##Load packages
Packages.6 <- c("broom.mixed","ggplot2", "lme4", "tidyverse","ggpubr", "lmerTest", "sjPlot", "car", "effects", "multcomp", "dplyr", "glmmTMB", "stats","emmeans")
lapply(Packages.6, library, character.only = TRUE)
#My own laptop
setwd("/Users/umroot/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")
#University computer
setwd("/Users/L_INGSHA/OneDrive - Concordia University - Canada/Thesis/Data/chapter2/")

#read in metadata file

summary_df<-read.csv("hourly_final.csv")

#Build the models
model.tem.hourly<- lmer(T_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)


model.hum.hourly<- lmer(H_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)


model.Twdif.hourly<- lmer(Tw_Difference ~ 1 + Location + DayorNight +Location*DayorNight +  (1 + Location + DayorNight +Location*DayorNight|ID), data= summary_df)


#Obtain Estimated Marginal Means
em_means <- emmeans(model.tem.hourly, ~ Location * DayorNight)

em_means <- emmeans(model.hum.hourly, ~ Location * DayorNight)

em_means <- emmeans(model.Twdif.hourly, ~ Location * DayorNight)

# Define a Linear Trend Contrast
trend_contrast <- contrast(em_means, method = "poly", by = "DayorNight", degree = 1) 

# View results
summary(trend_contrast)


