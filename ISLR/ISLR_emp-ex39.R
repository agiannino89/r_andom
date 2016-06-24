# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 9
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This question involves the use of multiple linear regression on the Auto data set.
library("ISLR")
data(Auto)
#
# (a) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)
#
# (b) Compute the matrix of correlations between the variables using the function 
# cor(). You will need to exclude the name variable, which is qualitative.
Auto_sub <- Auto[-9]
cor(Auto_sub)
# 
# (c) Use the lm() function to perform a multiple linear regression with mpg as the 
# response and all other variables except name as the predictors. Use the summary() 
# function to print the results. 
reg <- lm(mpg ~ ., data = Auto_sub) 
summary(reg)
# Comment on the output. For instance:
## i. Is there a relationship between the predictors and the response?
## F-statistic: 256.7 on 7 and 389 DF,  p-value: < 2.2e-16
## ii. Which predictors appear to have a statistically significant relationship to 
## the response?
## Displacement (*), weight (***), acceleration (*), year (***), origin (***)
## iii. What does the coefficient for the year variable suggest?
##  Every year the mpg increases on average by 0.7734.
#
# (d) Use the plot() function to produce diagnostic plots of the linear regression fit.
plot(reg)
# Comment on any problems you see with the fit. Do the residual plots suggest any 
# unusually large outliers? Does the leverage plot identify any observations with 
# unusually high leverage?
# Yes, see the plots.
#
# (e) Use the * and : symbols to fit linear regression models with interaction 
# effects. Do any interactions appear to be statistically significant?
reg_int <- lm(mpg ~ weight*year*origin, data = Auto_sub)
summary(reg_int)
#
# (f) Try a few different transformations of the variables, such as log(X), √X, X2. 
# Comment on your findings.
reg_tr1 <- lm(mpg ~ horsepower + log(horsepower) + weight*year*origin, data = Auto_sub)
summary(reg_tr1)

reg_tr2 <- lm(mpg ~ horsepower + sqrt(horsepower) + weight*year*origin, data = Auto_sub)
summary(reg_tr2)

reg_tr3 <- lm(mpg ~ horsepower + I(horsepower^2) + weight*year*origin, data = Auto_sub)
summary(reg_tr3)
