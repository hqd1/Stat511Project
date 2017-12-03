rm(list=ls())
setwd("C:/Users/Huy/Desktop/511 project")
data = read.csv("Monthly_forRegression_LatLong.csv", head = T)
#data = read.csv("Monthly_forRegression.csv", head = T)
summary(data)
View(data)

#### plot the full model diagnostics
dev.off()
fit = lm(Avg_FRP ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
           Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
           Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
           Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
           Pre3months_Temp_Anomaly, data = data)
par(mfrow=c(2,2))
plot(fit)
#### with log.y
log.y = log(data$Avg_FRP)                            ####Try a log transform
fit2 = lm(log.y ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
            Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
            Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
            Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
            Pre3months_Temp_Anomaly, data = data)
plot(fit2)
hist(log.y)
####boxplotting response against categorical variables
par(mfrow=c(1,2))
boxplot(log.y~factor(Month), data = data, xlab = "Month", ylab = "log(Avg_FRP)")$out
boxplot(log.y~factor(Year), data = data,xlab= "Year", ylab = "log(Avg_FRP)")$out
##############checking for linearity of response against continuous variables
##creating a new data frame with Avg_FRP and continuous predictors
newdata = data[,-(2:4)]     ###new data is data excluding categorical variable
par(mfrow=c(3,3))
outliers = list()
for (i in 1:14){
  #plot partial residuals  
  res.y = lm(log.y~.,data = newdata[,-(i+1)])$res
  res.x = lm(newdata[,(i+1)]~.-Avg_FRP, data = newdata[,-(i+1)])$res
  plot(res.x,res.y, main = paste("Partial residual plot for ",colnames(newdata)[i+1]))
  #identify outliers while plotting
  quan = quantile(res.y, probs = c(.25,.75))   #find 1st and 3rd quartile
  IQR = quan[2]-quan[1]                        #interquartile range
  lower = quan[1] - 1.5*IQR
  upper = quan[2] + 1.5*IQR
  out.y = res.y[res.y>upper | res.y<lower]
  out.x = res.x[res.y>upper | res.y<lower]
  text(out.x, out.y, labels = which(res.y>upper | res.y<lower),cex= 0.8)
  abline(lsfit(res.x,res.y))
  outliers[[i]] = which(res.y>upper | res.y<lower)
  
  #plot loess and confidence band
  loess.fit <- loess( y ~ x, data=data.frame(x=res.x,y=res.y))
  x.grid <- seq( from=min(res.x), to=max(res.x), length=200)
  tmp <- predict( loess.fit, newdata=data.frame( x=x.grid),se=T,interval = "confidence")
  lines( x.grid, tmp$fit, lwd=3, col=2)
  lines(x.grid,tmp$fit - qt(0.975,tmp$df)*tmp$se, lty=2, col = 2)
  lines(x.grid,tmp$fit + qt(0.975,tmp$df)*tmp$se, lty=2, col = 2)
}

#Look at outliers
outliers
data[c(35, 40,  70,  78, 123, 158, 247, 249, 395, 397, 409, 429, 470, 483, 500, 513, 537, 634),]
######################## MLR diagnostics

#interestingly, the residuals don't seem to be correlated
#let's check wih acf and pacf (you can look at chapter 4.1.4 for more info)
par(mfrow=c(1,2))
acf(fit2$res)
pacf(fit2$res)
#durbin watson test
n = nrow(data)
d = sum((fit2$res[-1]-fit2$res[-n])^2)/sum(fit2$res^2)
d
#The distribution of the test statistic is difficult to obtain. But when d is close to 2, we fail to reject the null.
#The closer d is to 0 or 4, the more evidence against the null hypothesis. 
# In this case, d is very close to 2
##################investigate multicollinearity
dev.off()
library(car)
vif(fit2)
#install.packages("corrplot")
library(corrplot)
cor = cor(newdata)
sum(cor(newdata)>=0.7 & cor(newdata)<1)/2   ###number of correlation greater than .7
corrplot(cor, method = "number")
#notice the correlation between temp and evap, some present and pre3months variables


#######################
### Fixing nonlinearity by adding polynomial terms
fit3 = lm(log.y ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
            Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
            Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
            Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
            Pre3months_Temp_Anomaly + I(Lat^2) + I(Pre3months_Precip^2) + I(Pre3months_Precip_Anomaly^2), data = data)
fit4 = lm(log.y ~ factor(Month) + factor(Year) + Lat + Long + Present_Evap + Present_Precip + 
            Present_Moisture + Present_Temp + Present_Precip_Anomaly + 
            Present_Temp_Anomaly + Pre3months_Evap + Pre3months_Precip + 
            Pre3months_Moisture + Pre3months_Temp + Pre3months_Precip_Anomaly + 
            Pre3months_Temp_Anomaly + I(Lat^2) + I(Pre3months_Precip^2) + I(Pre3months_Precip_Anomaly^2) +
            I(Lat^3) + I(Pre3months_Precip^3) + I(Pre3months_Precip_Anomaly^3), data = data)

anova(fit2,fit3, fit4)         #it seems that model with second-degree polynomial (i.e. fit3) is appropriate.
#we do have multicollinearity problem. We should try some variable selection method.
#since number of predictors is not prohibitive, let's actually perform best subset
#install.packages("leaps")
#######extracting the design matrix and obtain Y
X = model.matrix(fit3)
View(X)
Y = log.y
alldata=data.frame(cbind(Y,X[,-1]))
n = nrow(alldata)
V=10
idx.group=rep(1:V,floor(n/V+1))       #create V groups 
idx.group=idx.group[1:n]             #truncate down to size n
idx.group=sample(idx.group)          #stir and mix (maybe not mix :D)
library(glmnet)
cvmspe.lasso=rep(NA,V)
naive = rep(NA,V)
for(v in 1:V){
  idx.holdout=which(idx.group==v)
  ## lasso ridge (trying 100 different lambda values)
  lasso=glmnet(x=data.matrix(alldata[-idx.holdout,-1]),y=as.numeric(alldata[-idx.holdout,1]),alpha=1,nlambda=100)
  cv.lasso=cv.glmnet(x=data.matrix(alldata[-idx.holdout,-1]),y=as.numeric(alldata[-idx.holdout,1]),alpha=1,nfolds=10,nlambda=200)
  lambda.lasso=cv.lasso$lambda.min
  
  yhat.lasso=predict(cv.lasso,s="lambda.min",newx=data.matrix(alldata[idx.holdout,-1]))
  cvmspe.lasso[v]=mean((exp(alldata$Y[idx.holdout])-exp(yhat.lasso))^2)     #mspe on the original scale, not log scale
  naive[v] = mean((exp(alldata$Y[idx.holdout]) - exp(mean(alldata$Y[-idx.holdout])))^2)
}
mean(cvmspe.lasso)
mean(naive)

betas.lasso=coef(cv.lasso,s="lambda.1se")
betas.lasso

#checking for multicollinearity again based on lasso betas
fit5 = lm(log.y~factor(Year) + Lat + Long + Present_Evap + Present_Precip + Present_Moisture+Pre3months_Precip+Pre3months_Temp_Anomaly, data = data)
vif(fit5)
