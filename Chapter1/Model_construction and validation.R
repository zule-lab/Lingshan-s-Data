
Packages <- c("tidyverse", "caret","GGally","corpcor","mctest","car")
lapply(Packages, library, character.only = TRUE)

#New method

data<-read.csv(file.choose())   #Spatial_pattern_2020_2021_censustract#
data$LST_dif_2020<-max(data$LST_2020)-data$LST_2020
data$LST_dif_2021<-max(data$LST_2021)-data$LST_2021
attach(data)

set.seed(200)
random_sample <- createDataPartition(LST_dif_2020,p = 0.8, list = FALSE)
training  <- data[random_sample, ]
testing <- data[-random_sample, ]

fit1 <- lm(LST_dif_2020~HV+LV+EMS_HV+EMS_LV+LPI_HV+LPI_LV+MPSh_HV+MPSh_LV+0, data = training)

#The ‘mctest’ package in R provides the Farrar-Glauber test and other relevant tests for multicollinearity. 
#There are two functions viz. ‘omcdiag’ and ‘imcdiag’ under ‘mctest’ package in R which will provide 
#the overall and individual diagnostic checking for multicollinearity respectively.
vif(fit1)
cor(training[, c("HV","LV","EMS_HV","EMS_LV","LPI_HV","LPI_LV","MPSh_HV","MPSh_LV")])
omcdiag(fit1)
imcdiag(fit1)


fit2 <- lm(LST_dif_2020~HV+LV+LPI_HV+0, data = training)
vif(fit2)
omcdiag(fit2)
imcdiag(fit2)




model_linear <- lm(LST_dif_2020~HV+LV+LPI_HV+0, data = training)
summary(model_linear)
predictions_linear <- predict(model_linear, testing)

data.frame( R2 = R2(predictions_linear, testing$LST_dif_2020),
            RMSE = RMSE(predictions_linear, testing$LST_dif_2020),
            MAE = MAE(predictions_linear, testing$LST_dif_2020))
data.frame( R2 = R2(predictions_linear, testing$LST_dif_2021),
            RMSE = RMSE(predictions_linear, testing$LST_dif_2021),
            MAE = MAE(predictions_linear, testing$LST_dif_2021))
par(mfrow=c(2,2)) 

#residuals vs fitted plot
plot(model_linear, which = 1)
shapiro.test(resid(model_linear))
hist(residuals(model_linear))
qqnorm(residuals(model_linear))
qqline(residuals(model_linear))



model_quad <- lm(LST_dif_2020 ~ HV + I(HV^2)
                   + LV + I(LV^2)
                   + LPI_HV + I(LPI_HV^2)+0,
                   data = training)


summary(model_quad)
plot(model_quad, which = 1)
hist(residuals(model_quad))
qqnorm(residuals(model_quad))
qqline(residuals(model_quad))
#validation
predictions_quad <- predict(model_quad, testing)

data.frame( R2 = R2(predictions_quad, testing$LST_dif_2020),
            RMSE = RMSE(predictions_quad, testing$LST_dif_2020),
            MAE = MAE(predictions_quad, testing$LST_dif_2020))
data.frame( R2 = R2(predictions_quad, testing$LST_dif_2021),
            RMSE = RMSE(predictions_quad, testing$LST_dif_2021),
            MAE = MAE(predictions_quad, testing$LST_dif_2021))

# Nested F-test
anova(model_linear, model_quad)


# AIC/BIC
AIC(model_linear, model_quad)
BIC(model_linear, model_quad)


#polynomial model
model_poly <- lm(LST_dif_2020 ~ poly(HV, 2)
                + poly(LV, 2)
                + poly(LPI_HV, 2)+0,
                data = training)

summary(model_poly)
plot(model_poly, which = 1)
hist(residuals(model_poly))
qqnorm(residuals(model_poly))
qqline(residuals(model_poly))
#validation
predictions_poly <- predict(model_poly, testing)

data.frame( R2 = R2(predictions_poly, testing$LST_dif_2020),
            RMSE = RMSE(predictions_poly, testing$LST_dif_2020),
            MAE = MAE(predictions_poly, testing$LST_dif_2020))
data.frame( R2 = R2(predictions_poly, testing$LST_dif_2021),
            RMSE = RMSE(predictions_poly, testing$LST_dif_2021),
            MAE = MAE(predictions_poly, testing$LST_dif_2021))

# Nested F-test
anova(model_linear, model_poly)


# AIC/BIC
AIC(model_linear, model_poly)
BIC(model_linear, model_poly)

