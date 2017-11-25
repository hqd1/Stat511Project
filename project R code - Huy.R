rm(list=ls())
setwd("C:/Users/Huy/Desktop/511 project")
data = read.csv("Monthly_forRegression_LatLong.csv", head = T)
#data = read.csv("Monthly_forRegression.csv", head = T)
summary(data)
View(data)

####boxplotting response against categorical variables
par(mfrow=c(1,2))
plot(Avg_FRP~factor(Month), data = data)
plot(Avg_FRP~factor(Year), data = data)

##############checking for linearity of response against continuous variables
##creating a new data frame with Avg_FRP and continuous predictors
newdata = data[,-(2:4)]     ###new data is data excluding categorical variable
par(mfrow=c(3,5))

for (i in 1:14){
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
fit = lm(Avg_FRP ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
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
##################investigate multicollinearity
dev.off()
library(car)
vif(fit)
#install.packages("corrplot")
library(corrplot)
cor = cor(newdata)
sum(cor(newdata)>=0.7 & cor(newdata)<1)/2   ###number of correlation greater than .7
corrplot(cor, method = "number")
#notice the correlation between temp and evap, some present and pre3months variables

#################### Investigate normality and transformation of response variable
hist(data$Avg_FRP)                                   ######It looks heavily right-skewed. 
shapiro.test(fit$residuals)$p.value
log.y = log(data$Avg_FRP)                            ####Try a log transform
fit2 = lm(log.y ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
            Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
            Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
            Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
            Pre3months_Temp_Anomaly, data = data)
#Checking diagnostics
par(mfrow=c(2,2))
plot(fit2)                                           ###Ah, looks how nice the diagnostics are. this must be a dream.
par(mfrow=c(1,1))
hist(fit2$residuals)                                 ###This is to confirm that residuals are indeed normal
shapiro.test(fit2$residuals)
#Looking for statistical significance of predictors
summary(fit2)
#######################
#It seems like we do have multicollinearity problem. We should try some variable selection method.
#Howabout stepwise AIC, BIC, lasso?
#since number of predictors is not prohibitive, let's actually perform best subset
#install.packages("leaps")
library(leaps)
regsubsets.out <- regsubsets(log.y ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip +
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
min.cp
min.bic
max.adjr2
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

#choose model with 10 variables for interpretability and high adj r2
#extract model with 10 variable
reg.sum$which[7,]
#create new data with only 14 variable without having to dummy code
X = model.matrix(fit2)
colnames(X)
condensed.data = data.frame(log.y,X[,c(7,8,10,21,22,23,24,27,30,34)])
View(condensed.data)

#fit the model after variable selection
fit3 = lm(log.y~.,data = condensed.data)
par(mfrow=c(2,2))
plot(fit3)
#shapiro.test(fit3$residuals)$p.value
summary(fit3)
#I think I'm dreaming
#try with interaction terms
fit4 = lm(log.y ~ factor.Year.2002 + factor.Year.2003 + factor.Year.2005 + 
            Lat + Long + Present_Evap + Present_Precip + Present_Precip_Anomaly + 
            Pre3months_Precip + Pre3months_Temp_Anomaly + Lat*Long +Present_Evap*Present_Precip + I(Lat^2), data = condensed.data)
summary(fit4)
anova(fit3, fit4)

#try a model without Year
fit5 = lm(log.y~.,data = condensed.data[,-(2:4)])

#compare fit3 (the sparse model), fit5 (without Year) and naive guess in terms of cvmspe
set.seed(45150)
folds = sample(rep(1:10, length = nrow(data)))
folds
with.year=c()
without.year=c()
naive = c()
for (i in 1:10){
  fit3 = lm(log.y~.,data = condensed.data[folds!=i,])
  pred3 = predict(fit3,newdata = condensed.data[folds==i,])
  with.year[i] = mean((exp(pred3)-data[folds==i,1])^2)
  fit5 = lm(log.y~.-factor.Year.2002-factor.Year.2003-factor.Year.2005,data = condensed.data[folds!=i,])
  pred5 = predict(fit5,newdata = condensed.data[folds==i,])
  without.year[i] = mean((exp(pred5)-data[folds==i,1])^2)
  naive[i] = mean((mean(data[folds==i,1])-data[folds==i,1])^2)
}
mean(with.year)
mean(without.year)
mean(naive)
