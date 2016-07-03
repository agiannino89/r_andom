# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 10
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This question should be answered using the Carseats data set.
library("ISLR")
head(Carseats)
#
# (a) Fit a multiple regression model to predict Sales using Price, Urban, and
# US.
reg <- lm(Sales ~ Price + Urban + US, data = Carseats)
summary(reg)
#
# (b) Provide an interpretation of each coefficient in the model. Be careful
# some of the variables in the model are qualitative!
# Urban and US are qualitative dummy variables.
#
# (c) Write out the model in equation form, being careful to handle the 
# qualitative variables properly.
#
# (d) For which of the predictors can you reject the null hypothesis 
# H0 :βj = 0?
# Price and US.
# We can reject the null hypothesis for the Price and US.
#
# (e) On the basis of your response to the previous question, fit a smaller 
# model that only uses the predictors for which there is evidence of 
# association with the outcome.
reg.small <- lm(Sales ~ Price + US, data = Carseats)
summary(reg.small)
#
# (f) How well do the models in (a) and (e) fit the data?
# Adjusted R-squared:  0.2354 , F-statistic: 62.43 on 2 and 397 DF.
#
# (g) Using the model from (e), obtain 95% confidence intervals for the
# coefficient(s).
confint(reg.small)
#
# (h) Is there evidence of outliers or high leverage observations in the model
# from (e)?
par(mfrow = c(2, 2))
plot(reg.small)
# Yes, there are high leverage observations: see the plot.
