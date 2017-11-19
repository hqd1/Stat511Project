rm(list=ls())
#setwd("C:/Users/Huy/Desktop/511 project")
setwd("H:/Stat511/Project")
data = read.csv("Monthly_forRegression.csv", head = T)
summary(data)


####boxplotting response against categorical variables
par(mfrow=c(1,2))
plot(Avg_FRP~factor(Month), data = data)
plot(Avg_FRP~factor(Year), data = data)

##############plotting response against continuous variables
##creating a new data frame with Avg_FRP and continuous predictors
newdata = data[,-(2:4)]
par(mfrow=c(3,4))

for (i in 1:12){
#plot partial residuals  
  res.y = lm(Avg_FRP~.,data = newdata[,-(i+1)])$res
  res.x = lm(newdata[,(i+1)]~.-Avg_FRP, data = newdata[,-(i+1)])$res
  plot(res.x,res.y, main = paste("Partial residual plot for ",colnames(newdata)[i+1]))
  abline(lsfit(res.x,res.y))
  
#plot loess and confidence band
  loess.fit <- loess( y ~ x, data=data.frame(x=res.x,y=res.y))
  x.grid <- seq( from=min(res.x), to=max(res.x), length=200)
  tmp <- predict( loess.fit, newdata=data.frame( x=x.grid),se=T,interval = "confidence")
  lines( x.grid, tmp$fit, lwd=3, col=2)
  lines(x.grid,tmp$fit - qt(0.975,tmp$df)*tmp$se, lty=2, col = 2)
  lines(x.grid,tmp$fit + qt(0.975,tmp$df)*tmp$se, lty=2, col = 2)
}


######################## MLR diagnostics
dev.off()
fit = lm(Avg_FRP ~ factor(Month) + factor(Year) + Present_Evap + Present_Precip + 
           Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
           Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
           Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
           Pre3months_Temp_Anomaly, data = data)
par(mfrow=c(2,2))
plot(fit)
#interestingly, the residuals don't seem to be correlated
#let's check wih acf and pacf (you can look at chapter 4.1.4 for more info)
par(mfrow=c(1,2))
acf(fit$res)
pacf(fit$res)
#durbin watson test
n = nrow(data)
d = sum((fit$res[-1]-fit$res[-n])^2)/sum(fit$res^2)
d
#The distribution of the test statistic is difficult to obtain. But when d is close to 2, we fail to reject the null.
#The closer d is to 0 or 4, the more evidence against the null hypothesis. 
# In this case, d is very close to 2
#################### Investigate normality and transformation of response variable
hist(data$Avg_FRP)                                   ######It looks heavily right-skewed. 
log.y = log(data$Avg_FRP)                            ####Try a log transform
fit2 = lm(log.y ~ factor(Month) + factor(Year) + Present_Evap + Present_Precip + 
            Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
            Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
            Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
            Pre3months_Temp_Anomaly, data = data)
par(mfrow=c(2,2))
plot(fit2)                                           ###Ah, looks how nice the diagnostics are. this must be a dream.
summary(fit2)
#######################
##################investigate multicollinearity
library(car)
vif(fit)
#install.packages("corrplot")
library(corrplot)
cor = cor(newdata)
sum(cor(newdata)>=0.7 & cor(newdata)<1)/2   ###number of correlation greater than .7
corrplot(cor, method = "number")
#notice the correlation between temp and evap, some present and pre3months variables
#It seems like we do have multicollinearity problem. We should try some variable selection method.
#Howabout stepwise AIC, BIC, lasso?
#since number of predictors is not prohibitive, let's actually perform best subset
#install.packages("leaps")
library(leaps)
regsubsets.out <- regsubsets(log.y ~ factor(Month) + factor(Year) + Present_Evap + Present_Precip + 
                                 Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
                                 Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
                                 Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
                                 Pre3months_Temp_Anomaly,
                               data = data,
                               nbest = 1, # 1 best model for each number of predictors
                               nvmax = NULL, #NULL for no limit on number of variables
                               force.in = NULL, force.out = NULL,
                               intercept = TRUE,
                               method = "exhaustive")
reg.sum=summary(regsubsets.out)
reg.sum$outmat
#extract min cp, min bic, max adjr2 models
min.cp = which.min(reg.sum$cp) ###find the minimum location
min.bic= which.min(reg.sum$bic)
max.adjr2= which.max(reg.sum$adjr2)
#plot cp, bic, adjr2
par(mfrow=c(1,3))
plot(reg.sum$cp,xlab = "Number of Variables", ylab = "Cp", type = "l", col = "pink")
points(reg.sum$cp, pch = 19, col = "blue")
points(min.cp,reg.sum$cp[min.cp], pch = 4, cex = 3, col = "red")
plot(reg.sum$bic,xlab = "Number of Variables", ylab = "BIC", type = "l", col = "pink")
points(reg.sum$bic, pch = 19, col = "blue")
points(min.bic,reg.sum$bic[min.bic], pch = 4, cex = 3, col = "red")
plot(reg.sum$adjr2,xlab = "Number of Variables", ylab = "Adjusted R2", type = "l", col = "pink")
points(reg.sum$adjr2, pch = 19, col = "blue")
points(max.adjr2,reg.sum$adjr2[max.adjr2], pch = 4, cex = 3, col = "red")

#choose model with 14 variables for interpretability and high adj r2
#extract model with 14 variable
reg.sum$which[14,]
#create new data with only 14 variable without having to dummy code
X = model.matrix(fit2)
colnames(X)
summary(data$Month)
condensed.data = data.frame(log.y,X[,-c(1,6,9,11:19,22,23,25,27,29,32)])
View(condensed.data)

#fit the model after variable selection
fit3 = lm(log.y~.,data = condensed.data)
par(mfrow=c(2,2))
plot(fit3)
summary(fit3)
