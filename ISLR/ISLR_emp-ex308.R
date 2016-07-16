# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 8
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This question involves the use of simple linear regression on the Auto data 
# set.
library("ISLR")
data(Auto)
#
# (a) Use the lm() function to perform a simple linear regression with mpg as 
# the response and horsepower as the predictor. Use the summary() function to 
# print the results. Comment on the output. 
reg <- lm(mpg ~ horsepower, data = Auto)
summary(reg)
# For example:
## i. Is there a relationship between the predictor and the response? 
## F-statistic: 599.7 on 1 and 390 DF, p-value: < 2.2e-16.
## ii. How strong is the relationship between the predictor and the response?
## Adjusted R-squared:  0.6049 
## iii. Is the relationship between the predictor and the response positive or 
## negative?
## Negative.
## iv. What is the predicted mpg associated with a horsepower of 98? What are
## the associated 95 % confidence and prediction intervals?
predict(reg, data.frame(horsepower = 98), interval = "confidence")
predict(reg, data.frame(horsepower = 98), interval = "prediction")
#
# (b) Plot the response and the predictor. Use the abline() function to display 
# the least squares regression line.
plot(Auto$horsepower, Auto$mpg)
abline(reg, col = "red")
#
# (c) Use the plot() function to produce diagnostic plots of the least squares 
# regression fit. Comment on any problems you see with the fit.
plot(reg)
