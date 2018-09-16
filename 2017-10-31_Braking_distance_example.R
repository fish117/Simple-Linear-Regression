######################################
# BABS 507 - Transformation example
# Instructor: Martha Essak
######################################


###############
# IMPORT DATA
###############

mydata <- read.csv(file.choose(), header=TRUE)

str(mydata)

########################
# NON-TRANSFORMED MODEL
########################

# Plot of braking distance (feet) against speed (mph)
plot(distance ~ mph, data = mydata, pch = 16)

# Does the relationship look linear?
# Are there any outliers?

z1 <- lm(distance ~ mph, data = mydata)

resid1 <- resid(z1)
predict1 <- predict(z1)

# residual plot for model based on original units
plot(resid1 ~ predict1, pch=16)
abline(0, 0, lty=2)

# Assess assumptions of linearity and equal variance

#################################
# ADD TRANSFORMATIONS TO DATASET
#################################

mydata$distance.log <- log(mydata$distance)#$表示refer to the coloumn name, log 表示自然对数
mydata$mph.log <- log(mydata$mph)
mydata$mph.sq <- (mydata$mph)^2
mydata$distance.sqrt <- sqrt(mydata$distance)

str(mydata)

# Create some scatterplots of the transformed variables
plot(distance ~ mph.log, data = mydata, pch = 16)
plot(distance.log ~ mph, data = mydata, pch = 16)
plot(distance.log ~ mph.log, data = mydata, pch = 16)

plot(distance ~ mph.sq, data = mydata, pch = 16)

plot(distance.sqrt ~ mph, data = mydata, pch = 16)

############################################
# TRANSFORMED MODEL WITH SQUARE ROOT OF Y
############################################

plot(distance.sqrt ~ mph, data = mydata, pch = 16)

# Does the relationship look linear?

z2 <- lm(distance.sqrt ~ mph, data = mydata)

resid2 <- resid(z2)
predict2 <- predict(z2)

# residual plot for model
plot(resid2 ~ predict2, pch=16)
abline(0, 0, lty=2)

plot(distance.sqrt ~ mph, data = mydata, pch = 16)
abline(z2)

# Assess assumptions of linearity and equal variance


# Assess assumption of normality of errors

hist(resid2)

shapiro.test(resid2) #Shapiro-Wilk test

qqnorm(resid2, ylab= "residuals", xlab = "Normal scores", pch=16)
qqline(resid2)

summary(z2)

anova(z2)
                  

#################################
# CALCULATIONS IN ORIGINAL UNITS
#################################

mydata2 <- cbind(mydata, predict2)#cbind 为coloumn bind
mydata2

# Get the predicted values in the original units, using (predicted values)^2
mydata2$predict.orig <- (mydata2$predict2)^2

SSY <- sum((mydata2$distance - mean(mydata2$distance))^2)
SSY
SSE <- sum((mydata2$distance - mydata2$predict.orig)^2)
SSE
SSreg <- sum((mydata2$predict.orig - mean(mydata2$distance))^2)
SSreg

# When we have done the backtransformation of the predicted values, the equation SSY = SSreg + SSE is no longer true.

# Calculate measures of goodness of fit in original units
pseudo.R2 <- 1 - SSE/SSY # Always use this formula when calculating the Pseudo-R2
pseudo.R2

nrow(mydata) # 19 observations
rootMSE <- sqrt(SSE/17)# sqaure root of mean square error 19-2=17,in the original units
rootMSE

#############################################
# CONFIDENCE INTERVALS FOR THE CO-EFFICIENTS
#############################################

# Values for the co-efficients and their standard errors
summary(z2)

qt(0.975, 17) # This is the quantile function, which you use when you have a p-value and are looking for a t value
# remember to split your alpha value

# REFERENCE
?qt # This will show you some of other functions that you can use related to t values.


2*(1-pt(24.588, 17)) # This is the function that will give you a p-value from a t-value. You need to put in the t-value and the degrees of freedom. You also need to do 1- the probability because the command gives the probability on the left side, and since the test statistic is positive, you want the probability on the right side. Since it is a two-sided test, you will want to multiply that probability by 2 to get your p-value.


confint(z2, level=0.95) # Note that these confidence intervals will be in the transformed units.
#                 2.5 %     97.5 %
# (Intercept)  -2.7872080 -0.6037083
# mph           0.2730621  0.3243220
#confint 是对co-eefficient 求出置信区间,然后再backtransform ,all things are doing in trasnform units

(-1.69546)^2 # square the intercept to get it back in the original units
(-2.7872080)^2 #backtrasnform slope,直接平方
(-0.6037083)^2 

# b0 = 2.874585 [0.3644637, 7.768528]     in original units


####################################
# CREATE CONFIDENCE BANDS FOR PLOTS
####################################
xnew <- seq(min(mydata$mph), max(mydata$mph), length.out = 100)
xnew

ynew.ci <- data.frame(predict(z2, newdata = data.frame(mph = xnew), interval = "confidence", level = 0.95))
ynew.ci

new.values <- cbind(xnew,ynew.ci) # combine the vector with the new x-values and the dataframe with the predicted y-values and values for the confidene bands
print(new.values)


# create the plot with the regression line and the confidence bands in log units
plot(distance.sqrt ~ mph, data = mydata, pch = 16)
abline(z2)
lines(ynew.ci$lwr ~ xnew, lty = 2)
lines(ynew.ci$upr ~ xnew, lty = 2)




# backtransform everything
new.values
new.values$fit.back <- (new.values$fit)^2
new.values$lwr.back <- (new.values$lwr)^2
new.values$upr.back <- (new.values$upr)^2

new.values

# create the plot in the original units, with the confidence intervals for the mean value of y (confidence bands) backtransformed into the original units
plot(distance ~ mph, data = mydata, pch = 16)
lines(new.values$fit.back ~ new.values$xnew, lty=1)
lines(new.values$lwr.back ~ new.values$xnew, lty = 2)
lines(new.values$upr.back ~ new.values$xnew, lty = 2)










