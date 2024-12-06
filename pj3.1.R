library(astsa)
library(forecast)
library(TSA)
data <- read.csv("Desktop/stat153/fires.csv") 
plot(data$year,data$acres,type='l',
     xlab = "Year",
     ylab = "Acres",
     main = "Bearcounty Wildfires (Raw Data)")
data$lgacres <- log(data$acres)
plot(data$year,data$lgacres,type='l',
     xlab = "Year",
     ylab = "log(Acres)",
     main = "Bearcounty Wildfires (Log-Transformed Data)")

data$X2<- data$X^2

model1 = lm(lgacres ~ X + X2, data = data)
plot(data$year, data$lgacres, type = 'l',
     xlab = "Year",
     ylab = "log(Acres)",
     main = "Parametric Signal Model")
lines(data$year, model1$fitted.values, col='red', lwd = 2)
plot(data$year,model1$residuals,type='l', 
     xlab = "Year",
     ylab = "Residuals",
     main = "Residuals")
acf(model1$residuals,
    main = "ACF of Residuals") 
pacf(model1$residuals,
     main = "PACF of Residuals")
auto.arima(model1$residuals)
s1.1 = sarima(model1$residuals,p=1,d=0,q=0)
s1.2 = sarima(model1$residuals,p=1,d=0,q=1)
s1.1
s1.2
par(mfrow=c(1,1))
lag.max = 20
ACF = acf(model1$residuals,lag.max = lag.max,plot = FALSE)$acf[-1]
ACF <- c(ACF,0)
PACF = pacf(model1$residuals,lag.max = lag.max,plot = FALSE)$acf
ylim = c(-0.3, 0.6)
Lag = 1:lag.max
L = 2/sqrt(length(model1$residuals))

## ACF 
plot(Lag, ACF, type = "h"
     , ylim = ylim
     , main = "ACF of Residuals")
abline(h = c(0, -L, L), lty = c(1, 2, 2), col = c(1,4, 4))
# noise 1
a = ARMAacf(ar=s1.1$fit$coef[1],lag.max = lag.max)
points(Lag,a[-1],col='red',cex=.5)
# noise 2
a = ARMAacf(ar=s1.2$fit$coef[1],ma=s1.2$fit$coef[2],lag.max = lag.max)
points(Lag,a[-1],col='blue',cex=.5)
## PACF
plot(Lag, PACF, type = "h"
     , ylim = ylim
     , main = "PACF of Residuals")
abline(h = c(0, -L, L), lty = c(1, 2, 2), col = c(1,4, 4))
# noise 1
p = ARMAacf(ar=s1.1$fit$coef[1],lag.max = lag.max,pacf = TRUE)
points(Lag,p,col='red',cex=.5)
# noise 2
p = ARMAacf(ar=s1.2$fit$coef[1],ma=s1.2$fit$coef[2],lag.max = lag.max,pacf = TRUE)
points(Lag,p,col='blue',cex=.5)

d = diff(data$lgacres)
fitted_diff = data$lgacres[1:78]+mean(d)
plot(data$year, data$lgacres, type = 'l',
     xlab = "Year",
     ylab = "log(Acres)",
     main = "Differencing Fitted Values")
lines(data$year[2:79], fitted_diff, col='red', lwd = 2)
plot(data$year[2:79], d, type = "l",
     xlab = "Year",
     ylab = "log(Acres)",
     main = "First Differences of log(Acres)")
acf(d, main = "ACF of Residuals")
pacf(d, main = "PACF of Residuals")
auto.arima(d)
s2.1 = sarima(d,p=2,d=0,q=0)
s2.2 = sarima(d, p=0, d=0, q=1)
s2.1
s2.2
par(mfrow=c(1,1))
lag.max = 20
ACF = acf(d,lag.max = lag.max,plot = FALSE)$acf[-1]
ACF <- c(ACF,0)
PACF = pacf(d,lag.max = lag.max,plot = FALSE)$acf
ylim = c(-0.4, 0.25)
Lag = 1:lag.max
L = 2/sqrt(length(model1$residuals))

## ACF 
plot(Lag, ACF, type = "h"
     , ylim = ylim
     , main = "ACF of Residuals")
abline(h = c(0, -L, L), lty = c(1, 2, 2), col = c(1,4, 4))
# noise 1
a = ARMAacf(ar=c(s2.1$fit$coef[1],s2.1$fit$coef[2]),lag.max = lag.max)
points(Lag,a[-1],col='red',cex=.5)
# noise 2
a = ARMAacf(ma=s2.2$fit$coef[1],lag.max = lag.max)
points(Lag,a[-1],col='blue',cex=.5)
## PACF
plot(Lag, PACF, type = "h"
     , ylim = ylim
     , main = "PACF of Residuals")
abline(h = c(0, -L, L), lty = c(1, 2, 2), col = c(1,4, 4))
# noise 1
p = ARMAacf(ar=c(s2.1$fit$coef[1],s2.1$fit$coef[2]),lag.max = lag.max,pacf = TRUE)
points(Lag,p,col='red',cex=.5)
# noise 2
p = ARMAacf(ma=s2.2$fit$coef[1],lag.max = lag.max,pacf = TRUE)
points(Lag,p,col='blue',cex=.5)

sum_squared_errors <- c(model1.1=0, model1.2=0, model2.1=0, model2.2=0)
for (i in 3:1) {
  train_set <- data[1:(nrow(data) - 10*i),]
  test_set <- data[(nrow(data) - 10*i + 1):(nrow(data) - 10*(i-1) ),]
  N = nrow(train_set)
  signal1 = lm(lgacres ~ X + X2, data = data)
  signal.forecast1 = predict(signal1,test_set)
  noise.forecast1.1 = sarima.for(signal1$residuals, n.ahead=10, p=1,d=0,q=0)$pred
  noise.forecast1.2 = sarima.for(signal1$residuals, n.ahead=10, p=1,d=0,q=1)$pred
  forecast1.1 = signal.forecast1 + noise.forecast1.1
  forecast1.2 = signal.forecast1 + noise.forecast1.2
  d = diff(data$lgacres)
  forecast2.1 = sarima.for(data$lgacres,n.ahead=10,p=2,d=1,q=0)$pred
  forecast2.2 = sarima.for(data$lgacres,n.ahead=10,p=0,d=1,q=1)$pred
  sum_squared_errors[1] = sum_squared_errors[1] + sum((forecast1.1 - test_set$lgacres)^2)
  sum_squared_errors[2] = sum_squared_errors[2] + sum((forecast1.2 - test_set$lgacres)^2)
  sum_squared_errors[3] = sum_squared_errors[3] + sum((forecast2.1 - test_set$lgacres)^2)
  sum_squared_errors[4] = sum_squared_errors[4] + sum((forecast2.2 - test_set$lgacres)^2)
}

rmse = matrix(sqrt(sum_squared_errors/30), nrow=4,ncol = 1)
colnames(rmse) = "RMSPE"
rownames(rmse) = c(
  "Parametric Model + ARMA(1,0)",
  "Parametric Model + ARMA(1,1)",
  "ARIMA(2,1,0)",
  "ARIMA(0,1,1)"
)
rmse

signal1 = lm(lgacres ~ X + X2, data = data)

s = summary(signal1)
s$coefficients[,1:2]
s1.1$ttable[,1:2]

data2 = tail(data,10)
data2$X = data2$X + 10
data2$X2<- data2$X^2

signal.forecast1 = predict(signal1,data2)
lgacres = data$lgacres
Acres = data$acres
attempt_f = sarima.for(Acres, n.ahead=10, p=1,d=0,q=0, xreg = exp(signal1$fitted.values), newxreg = exp(signal.forecast1))$pred
attempt = exp(sarima.for(lgacres, n.ahead=10, p=1,d=0,q=0, xreg = signal1$fitted.values, newxreg = signal.forecast1)$pred)
write.table(x = attempt,file = "Desktop/stat153/fires_3035169671.csv", sep=",",row.names=FALSE, col.names=FALSE)
