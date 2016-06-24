# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 15
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This problem involves the Boston data set, which we saw in the lab for this chapter. 
# We will now try to predict per capita crime rate using the other variables in this
# data set. In other words, per capita crime rate is the response, and the other 
# variables are the predictors.
library("MASS")
#
# (a) For each predictor, fit a simple linear regression model to predict the response.
# Describe your results. In which of the models is there a statistically significant 
# association between the predictor and the response? Create some plots to back up 
# your assertions.
reg_zn <- lm(crim ~ zn, data = Boston)
reg_indus <- lm(crim ~ indus, data = Boston)
reg_chas <- lm(crim ~ chas, data = Boston)
reg_nox <- lm(crim ~ nox, data = Boston)
reg_rm <- lm(crim ~ rm, data = Boston)
reg_age <- lm(crim ~ age, data = Boston)
reg_dis <- lm(crim ~ dis, data = Boston)
reg_rad <- lm(crim ~ rad, data = Boston)
reg_tax <- lm(crim ~ tax, data = Boston)
reg_ptratio <- lm(crim ~ ptratio, data = Boston)
reg_black <- lm(crim ~ black, data = Boston)
reg_lstat <- lm(crim ~ lstat, data = Boston)
reg_medv <- lm(crim ~ medv, data = Boston)
#
summary(reg_zn)
summary(reg_indus)
summary(reg_chas)
summary(reg_nox)
summary(reg_rm)
summary(reg_age)
summary(reg_dis)
summary(reg_rad)
summary(reg_tax)
summary(reg_ptratio)
summary(reg_black)
summary(reg_lstat)
summary(reg_medv)
#
# (b) Fit a multiple regression model to predict the response using all of the 
# predictors. Describe your results. For which predictors can we reject the null 
# hypothesis H0 : βj = 0?
reg_tot <- lm(crim ~ ., data = Boston)
summary(reg_tot)
#
# (c) How do your results from (a) compare to your results from (b)? Create a plot 
# displaying the univariate regression coefficients from (a) on the x-axis, and the 
# multiple regression coefficients from (b) on the y-axis. That is, each predictor is
# displayed as a single point in the plot. Its coefficient in a simple linear 
# regression model is shown on the x-axis, and its coefficient estimate in the multiple
# linear regression model is shown on the y-axis.
simple <- vector("numeric",0)
simple <- c(simple, reg_zn$coefficient[2])
simple <- c(simple, reg_indus$coefficient[2])
simple <- c(simple, reg_chas$coefficient[2])
simple <- c(simple, reg_nox$coefficient[2])
simple <- c(simple, reg_rm$coefficient[2])
simple <- c(simple, reg_age$coefficient[2])
simple <- c(simple, reg_dis$coefficient[2])
simple <- c(simple, reg_rad$coefficient[2])
simple <- c(simple, reg_tax$coefficient[2])
simple <- c(simple, reg_ptratio$coefficient[2])
simple <- c(simple, reg_black$coefficient[2])
simple <- c(simple, reg_lstat$coefficient[2])
simple <- c(simple, reg_medv$coefficient[2])
multi <- vector("numeric", 0)
multi <- c(multi, reg_tot$coefficients)
multi <- multi[-1]
plot(simple, multi, col = "red")
# (d) Is there evidence of non-linear association between any of the predictors and the
# response? To answer this question, for each predictor X, fit a model of the form
# Y = β0 +β1X +β2X2 +β3X3 +ε.
reg_p_zn <- lm(crim ~ poly(zn, 3))
reg_p_indus <- lm(crim ~ poly(indus, 3))
reg_p_chas <- lm(crim ~ poly(znchas, 3))
reg_p_nox <- lm(crim ~ poly(nox, 3))
reg_p_rm <- lm(crim ~ poly(rm, 3))
reg_p_age <- lm(crim ~ poly(age, 3))
reg_p_dis <- lm(crim ~ poly(dis, 3))
reg_p_rad <- lm(crim ~ poly(rad, 3))
reg_p_tax <- lm(crim ~ poly(tax, 3))
reg_p_ptratio <- lm(crim ~ poly(ptratio, 3))
reg_p_black <- lm(crim ~ poly(black, 3))
reg_p_lstat <- lm(crim ~ poly(lstat, 3))
reg_p_medv <- lm(crim ~ poly(zn, 3))

summary(reg_p_zn)
summary(reg_p_indus)
summary(reg_p_chas)
summary(reg_p_nox)
summary(reg_p_rm)
summary(reg_p_age)
summary(reg_p_dis)
summary(reg_p_rad)
summary(reg_p_tax)
summary(reg_p_ptratio)
summary(reg_p_black)
summary(reg_p_lstat)
summary(reg_p_medv)

