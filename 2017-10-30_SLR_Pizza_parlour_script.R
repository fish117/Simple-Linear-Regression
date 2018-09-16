############################
# Pizza Parlour Example
# BABS 507 - Week 02
############################



mydata <- read.csv(file.choose(), header=TRUE)

print(mydata)
# Note that student population and quartlerly sales are in thousands



plot(sales ~ student.population, data=mydata, pch=16)
# 对于outlier 异常点要注意解释，outlier不仅是resedual小的点，可能是远离主体的点。对于Influential的点，
#要注意舍去，因为偶尔会改变regression的值。
z1 <- lm(sales ~ student.population, data=mydata)
resid1 <- resid(z1)
predict1 <- predict(z1)


# residual plot using predicted values for y
plot(resid1 ~ predict1, pch=16)
abline(0,0, lty=2)
#正的负的值都有，a good residual point is very even, this is looking quite good.

# residual plot using values for x
plot(resid1 ~ mydata$student.population, pch=16)
abline(0,0, lty=2)

hist(resid1)

shapiro.test(resid1) #Shapiro-Wilk test

qqnorm(resid1, ylab= "residuals", xlab = "Normal scores", pch=16)
qqline(resid1)
#fail to reject H0, it is normal distribution

# Note where the results for testing the significance of the regression can be found
anova(z1)

summary(z1)

# plot with regression line
plot(sales ~ student.population, data=mydata, pch=16)
abline(z1)


# create vectors and dataframes with data for confidence bands
xnew <- seq(min(mydata$student.population), max(mydata$student.population), length.out = 100)
xnew
ynew.ci <- data.frame(predict(z1, newdata = data.frame(student.population = xnew), interval = "confidence", level = 0.95))
ynew.ci


# create the plot with the regression line and the confidence bands
plot(sales ~ student.population, data=mydata, pch=16)
abline(z1)
lines(ynew.ci$lwr ~ xnew, lty = 2)
lines(ynew.ci$upr ~ xnew, lty = 2)


# create vectors and dataframes with data for prediction intervals
xnew <- seq(min(mydata$student.population), max(mydata$student.population), length.out = 100)
ynew.pred <- data.frame(predict(z1, newdata = data.frame(student.population = xnew), interval = "prediction", level = 0.95))#interval = prediction 所以是Predictioninterval

plot(sales ~ student.population, data=mydata, pch=16)
abline(z1)
lines(ynew.pred$lwr ~ xnew, lty = 2)
lines(ynew.pred$upr ~ xnew, lty = 2)
# cbind means column bind 
#######

SSY <- sum((mydata$sales - mean(mydata$sales))^2)
SSY 

SSX <- sum((mydata$student.population - mean(mydata$student.population))^2)
SSX

student.pop.mean <- mean(mydata$student.population)
sales.mean <- mean(mydata$sales)

xy <- (mydata$sales - mean(mydata$sales))*(mydata$student.population - mean(mydata$student.population))
xy
SPxy <- sum(xy)
SPxy

# calculate the slope
slope <- SPxy/SSX
slope

#calculate the intercept
sales.mean - slope*student.pop.mean

# Calculate all the sum of squares
predict1 <- predict(z1)
predict1

# use cbind to bind column together to make a data frame that has the original data, and the predicted values. If you need to backtransform any predicted values, it is very useful to have this data frame to work with.
mydata2 <- cbind(mydata, predict1)
mydata2

SSY <- sum(( mydata2$sales - mean( mydata2$sales))^2)
SSY
SSE <- sum(( mydata2$sales - mydata2$predict1)^2)
SSE
SSreg <- sum(( mydata2$predict1 - mean( mydata2$sales))^2)
SSreg

# calculate F statistic
Fstat <- (SSreg/1)/(SSE/(10-2))
Fstat

# find the p-value for numerator df = 1, denominator df = 8
1-pf(Fstat, 1, 8)

# find the critical value  for numerator df = 1, denominator df = 8 and alpha = 0.05
qf(0.95, 1, 8)


#########
# Here are some example for finding critical values and p-values using R.

# In the functions, the first letter is either "q" for "quantile" or "p" for probability

# F distribution

qf(0.95, 1, 8) # Find the F critical value for numerator degrees of freedom = 1, denominator df = 8, alpha = 0.05

qf(0.05, 1, 8, lower.tail=FALSE) # this is an alternative way to get the same answer as above. Instead of entering 1-alpha, you get to enter alpha directly. lower.tail = FALSE tells R to place alpha on the right side of the distribution (placing the 0.95 on the left).

pf(4.9, 1, 8, lower.tail = FALSE) # how to get a p-value from a F statistic of 4.9. Numerator df=1, denominator df=8.



# t distribution

qt(0.975, 8, lower.tail=TRUE ) # Finding the t critical value for alpha = 0.05, which is divided by 2 for a two-sided test. df error = 8

qt(0.025, 8, lower.tail=FALSE) # This is an alternative to the code above.df error = 8

pt(8.617, 8, lower.tail = FALSE) # p-value for a test statistic of t=6.4 when df error = 8. If you are doing a two-tailed test (which is what we have done in class so far), you will need to multiply this value by 2.

2*pt(8.617, 8, lower.tail = FALSE)
