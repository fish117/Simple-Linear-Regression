########################################
# BABS 507: Multicollinearity example
# Martha Essak
########################################


mydata <- aerospace_and_defense_companies_1_
str(mydata) #market.value is in millions of dollars; assets are in millions of dollars


plot(market.value ~ assets, data=mydata, pch=16)
plot(market.value ~ employees, data=mydata, pch=16)

# We might expect assets and employees to be positively associated because if a company is larger it will have more employees and more assets.
# Let's see what happens when we fit a model with both explanatory variables.

z1 <- lm(market.value ~ assets + employees, data=mydata)
summary(z1)

# Is the regression significant?

# Is each explanatory variable significant in the presence of the other variable?

# Let's take a look at the correlation matrix.

mydata.2 <- mydata[c(2, 3, 4)] # put columns 2, 3, and 4 in a dataset so that all the variables present are numeric
mydata.2

cor.matrix <- cor(mydata.2, method="pearson")
round(cor.matrix, 4)

library(Hmisc)
cor.matrix.2 <- rcorr(as.matrix(mydata.2))
cor.matrix.2

# Note that the correlation between the explanatory variables (assets and employees) is 0.9444, which is quite high


# Calculate the VIF for each explanatory variable
zx1 <- lm(assets ~ employees, data=mydata)
summary(zx1)
VIF1 <- 1/(1-0.8919)
VIF1

zx2 <- lm(employees ~ assets, data=mydata)
summary(zx2)
VIF2 <- 1/(1-0.8919)
VIF2 # since there are only two explanatory variables, you get the same VIF




# Since both explanatory variables are measuring size in slightly different ways, we can keep one of the variables as a measure of size, and change the other variable to a ratio. To do this we will add a new column to the dataset, employees/assets, which indicates the number of employees per million dollars of assets. This new variable is contributing new information about the efficiency in the use of employees.

mydata$employees.over.assets <- mydata$employees/mydata$assets
str(mydata)

z2 <- lm(market.value ~ assets + employees.over.assets, data=mydata)
summary(z2)

# Is the regression significant?

# Is each explanatory variable significant in the presence of the other variable?


# Let's take a look at the correlation matrix.

mydata.3 <- mydata[c(2, 3, 5)]
cor.matrix.new <- cor(mydata.3, method="pearson")
round(cor.matrix.new, 4)

# The correlation between the two explanatory variables is much weaker now.

cor.matrix.3 <- rcorr(as.matrix(mydata.3))
cor.matrix.3


# Calculate the VIF for each explanatory variable
zx1 <- lm(assets ~ employees.over.assets, data=mydata)
summary(zx1)
VIF1 <- 1/(1-0.07357)
VIF1

zx2 <- lm(employees.over.assets ~ assets, data=mydata)
summary(zx2)
VIF2 <- 1/(1-0.07357)
VIF2 # since there are only two explanatory variables, you get the same VIF











