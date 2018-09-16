# BABS 502 - Forecasting for Management
# Instructor: Martha Essak
# Week 05 examples (Apr. 3-5, 2017)


library(fpp)
library(MPV)

############################
# Getting your point forecasts for the future
# ETS MODEL
plot(elecequip)

elecequip.training <- window(elecequip,end=2010-0.01)
elecequip.training
elecequip.test <- window(elecequip, start=2010-0.01)
elecequip.test # we will generate forecasts for 23 months into the future, matching our test set

elecequip.hwa <- ets(elecequip.training, model="AAA", damped=FALSE) # This is the Additive Holt Winters method, fit using the ets() function
accuracy(elecequip.hwa) # this tells you how well the model fits the training set

forecast(elecequip.hwa, h=23) # These are the forecasts for the test set, using only the data from the training set

accuracy(forecast(elecequip.hwa, h=23), elecequip.test) # these are the accuracy measures for the test set. Note that although the fit based on the one-step forecasts was quite good, the MASE here indicates that the forecast accuracy for the test set is low

plot(elecequip, ylim=c(60,140))
lines(forecast(elecequip.hwa, h=23)$mean, col="red") # this plot shows the accuracy of the model when forecasting the test set

elecequip.hwa.forecast <- forecast(ets(elecequip, model=elecequip.hwa), h=23) # these are the forecasts for the future, using the model developed based on the training set, but using all of the data
elecequip.hwa.forecast

plot(elecequip, xlim=c(1996, 2014), ylim=c(50, 130))
lines(elecequip.hwa.forecast$mean, col="blue")

############################
# Getting your point forecasts for the future
# ARIMA MODEL
plot(elec) # if we think the variance of the seasonal component is changing, we could try a Box-Cox transformation to start 

# select the last 3 years as the test set
elec.training <- window(elec,end=c(1992, 8))
elec.test <- window(elec,start=c(1992, 9))

lambda <- BoxCox.lambda(elec.training) # = 0.2120148; 
plot(BoxCox(elec.training,lambda), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")

elec.transformed.training <- BoxCox(elec.training,lambda)

Acf(elec.transformed.training) 
Pacf(elec.transformed.training)

# Deciding about differencing
plot(diff(elec.transformed.training, 12)) 
plot(diff(diff(elec.transformed.training, 12))) 


Acf(diff(diff(elec.transformed.training, 12)), lag=60)
Pacf(diff(diff(elec.transformed.training, 12)), lag=60) 

fit.elec.4 <- auto.arima(elec.training, lambda=lambda) 
summary(fit.elec.4)


fit.elec.4.forecasts <- forecast(fit.elec.4, h=36)$mean

plot(elec, ylim=c(1000, 17000))
lines(forecast(snaive(elec.training, h=36))$mean, col="red")
lines(fit.elec.4.forecasts, col="forestgreen")

# How to generate forecasts of the future?
fit.elec.future.forecasts <- forecast(Arima(elec, model=fit.elec.4), h=36) 

plot(elec, ylim=c(1000, 17000), xlim=c(1957, 2000))
lines(forecast(snaive(elec, h=36))$mean, col="red")
lines(fit.elec.future.forecasts$mean, col="forestgreen")


############################
# SLR: Time series model
plot(olympic, pch=16) # linear, year on x-axis; winning times (in seconds) for the men's 400 m final in the Olympic Games
olympic

fit1 <- lm(olympic$time ~ olympic$Year) # note that olpymic is not a ts object, so we will use the lm() command
summary(fit1) # you can use the intercept and slope to develop forecasts for the future times in the Olympic Games


plot(olympic, pch=16, xlim=c(1896, 2008), ylim=c(42, 54))
abline(fit1)
x1 <- c(2000, 2004, 2008)
y1 <- 196.079876 -0.076790*x1
points(x1, y1, col="blue", pch=16)

# You could try removing the first observation

olympic.subset <- subset(olympic, olympic$Year > 1897)

fit2 <- lm(olympic.subset$time ~ olympic.subset$Year) # note that olpymic is not a ts object, so we will use the lm() command
summary(fit2) # you can use the intercept and slope to develop forecasts for the future times in the Olympic Games


plot(olympic.subset, pch=16, xlim=c(1896, 2008), ylim=c(42, 54))
abline(fit2)
x2 <- c(2000, 2004, 2008)
y2 <- 175.96559 -0.06656*x2
points(x2, y2, col="blue", pch=16)

residuals(fit2)
mean(residuals(fit2))

plot(residuals(fit2) ~ olympic.subset$Year, pch=16)
abline(0,0, lty=2)

Acf(residuals(fit2))

hist(residuals(fit2))

############################
# MLR: Time series model
plot(fancy)
fancy # 7 years X 12 months

# create a model with a linear trend and dummy variables for the seasons

# take natural logarithms first
plot(log(fancy))

seasonplot(log(fancy))

fit3 <- tslm(log(fancy) ~ trend + season) # tslm is a function for time series linear models, so it gives output appropriate for a time series and we can use it with the forecast function
summary(fit3)

exp(forecast(fit3, h=12)$mean) # forecasts for the next year in original units

forecast.fancy <- forecast(fit3, h=12)
plot(forecast.fancy)

plot(fancy, xlim=c(1987, 1995), ylim=c(0, 120000))
lines(exp(forecast(fit3, h=12)$mean), col="blue")

# Note that here we did not use a test set to validate the accuracy of the model. Instead we fit the model to the entire dataset.

############################
# MLR: Time series model 2
ausbeer.set <- window(ausbeer, start=1980)
plot(ausbeer.set)

fit4 <- tslm(ausbeer.set ~ trend + season)
summary(fit4)


forecast.ausbeer <- forecast(fit4, h=12)
plot(forecast.ausbeer)

plot(ausbeer.set, xlim=c(1980, 2011), ylim=c(375, 600))
lines(forecast(fit4, h=12)$mean, col="blue")

# What are some strengths and weaknesses of this method?

############################
# SLR Explanatory model
plot(texasgas$consumption ~ texasgas$price, pch=16) # consumption vs. price; need to know price

texasgas

texasgas.price <- ts(texasgas$price, start=1)
plot(texasgas.price)

# What are some ways we could predict price?



#############
# Method 1: Use a (natural) log transformation of y

z1 <- lm(log(texasgas$consumption) ~ texasgas$price)
summary(z1) # residual standard error is in log units

PRESS(z1) # predictive residual sum of squares; cross-validation measure; needs MPV library
# The PRESS statistic is calculated by leaving out one observation, estimating the co-efficients with the remaining data, then predicting that value. The actual value is compared to the predicted value to give a residual. This is done with all the observations, then the sum of these squared residuals is taken.

exp(residuals(z1)) # residuals in original units

sqrt(sum((texasgas$consumption - exp(fitted(z1)))^2)/(nrow(texasgas)-2))  # Root MSE: 19.20329

plot(texasgas$consumption ~ texasgas$price, pch=16)
lines(texasgas$price, exp(fitted(z1)))

#############
# Method 2: Use a (natural) log transformation of y and x
# From the graph of the previous fit, we might decide to try transforming the x variable as well

z2 <- lm(log(texasgas$consumption) ~ log(texasgas$price))
summary(z2) # residual standard error is in log units

PRESS(z2) # predictive residual sum of squares; cross-validation measure

exp(fitted(z2))

sqrt(sum((texasgas$consumption - exp(fitted(z2)))^2)/(nrow(texasgas)-2)) # Root MSE: 15.93374

plot(texasgas$consumption ~ texasgas$price, pch=16)
lines(texasgas$price, exp(fitted(z2)))


#############
# Method 3: Fit a piecewise linear function with a knot at 60
piecewise1 <- lm(texasgas$consumption ~ texasgas$price*(texasgas$price <= 60) + texasgas$price*(texasgas$price > 60))
summary(piecewise1) # Residual standard error: 13.49

# The intercept for the line when x <= 60 is: (Intercept) + x <= 60TRUE = 84.7861 + 136.1068 = 220.8929
# The slope of the line when x <= 60 is: x + x:x<=60TRUE = -0.4470 + -2.4587 = -2.9057

# The intercept for the line when x > 60 is: (Intercept) + x > 60TRUE = 84.7861
# The slope of the line when x > 60 is: x + x:x>60TRUE = -0.4470

PRESS(piecewise1) # Note that this is not comparable to our other values because of the units. You would need to get the other residuals in the original units.

plot(texasgas$consumption ~ texasgas$price, pch=16)
x <- texasgas$price
curve((84.7861 + 136.1068) + (-0.4470 + -2.4587)*x, add=TRUE, from=min(x), to=60)
curve((84.7861) + -0.4470*x, add=TRUE, from=60, to=max(x))


#############
# Method 4: Use a quadratic transformation of x
texasgas$price.sq <- (texasgas$price)^2

z4 <- lm(texasgas$consumption ~ texasgas$price + texasgas$price.sq)
summary(z4) # Root MSE: 14.37

PRESS(z4)

plot(texasgas$consumption ~ texasgas$price, pch=16)
lines(texasgas$price, fitted(z4))


#############




