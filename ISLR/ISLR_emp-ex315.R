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
# This problem involves the Boston data set, which we saw in the lab for this
# chapter. We will now try to predict per capita crime rate using the other 
# variables in this data set. In other words, per capita crime rate is the 
# response, and the other variables are the predictors.
library("MASS")
#
# (a) For each predictor, fit a simple linear regression model to predict the
# response. Describe your results. In which of the models is there a 
# statistically significant association between the predictor and the response?
# Create some plots to back up your assertions.
reg.zn <- lm(crim ~ zn, data = Boston)
reg.indus <- lm(crim ~ indus, data = Boston)
reg.chas <- lm(crim ~ chas, data = Boston)
reg.nox <- lm(crim ~ nox, data = Boston)
reg.rm <- lm(crim ~ rm, data = Boston)
reg.age <- lm(crim ~ age, data = Boston)
reg.dis <- lm(crim ~ dis, data = Boston)
reg.rad <- lm(crim ~ rad, data = Boston)
reg.tax <- lm(crim ~ tax, data = Boston)
reg.ptratio <- lm(crim ~ ptratio, data = Boston)
reg.black <- lm(crim ~ black, data = Boston)
reg.lstat <- lm(crim ~ lstat, data = Boston)
reg.medv <- lm(crim ~ medv, data = Boston)
#
summary(reg.zn)
summary(reg.indus)
summary(reg.chas)
summary(reg.nox)
summary(reg.rm)
summary(reg.age)
summary(reg.dis)
summary(reg.rad)
summary(reg.tax)
summary(reg.ptratio)
summary(reg.black)
summary(reg.lstat)
summary(reg.medv)
#
# (b) Fit a multiple regression model to predict the response using all of the 
# predictors. Describe your results. For which predictors can we reject the 
# null hypothesis H0 : βj = 0?
reg.tot <- lm(crim ~ ., data = Boston)
summary(reg.tot)
#
# (c) How do your results from (a) compare to your results from (b)? Create a 
# plot displaying the univariate regression coefficients from (a) on the 
# x-axis, and the multiple regression coefficients from (b) on the y-axis. That
# is, each predictor is displayed as a single point in the plot. Its 
# coefficient in a simple linear regression model is shown on the x-axis, and
# its coefficient estimate in the multiple linear regression model is shown on
# the y-axis.
simple <- vector("numeric",0)
simple <- c(simple, reg.zn$coefficient[2])
simple <- c(simple, reg.indus$coefficient[2])
simple <- c(simple, reg.chas$coefficient[2])
simple <- c(simple, reg.nox$coefficient[2])
simple <- c(simple, reg.rm$coefficient[2])
simple <- c(simple, reg.age$coefficient[2])
simple <- c(simple, reg.dis$coefficient[2])
simple <- c(simple, reg.rad$coefficient[2])
simple <- c(simple, reg.tax$coefficient[2])
simple <- c(simple, reg.ptratio$coefficient[2])
simple <- c(simple, reg.black$coefficient[2])
simple <- c(simple, reg.lstat$coefficient[2])
simple <- c(simple, reg.medv$coefficient[2])
multi <- vector("numeric", 0)
multi <- c(multi, reg.tot$coefficients)
multi <- multi[-1]
plot(simple, multi, col = "red")
# (d) Is there evidence of non-linear association between any of the predictors
# and the response? To answer this question, for each predictor X, fit a model 
# of the form Y = β0 +β1X +β2X2 +β3X3 + ε.
reg.p.zn <- lm(crim ~ poly(zn, 3))
reg.p.indus <- lm(crim ~ poly(indus, 3))
reg.p.chas <- lm(crim ~ poly(znchas, 3))
reg.p.nox <- lm(crim ~ poly(nox, 3))
reg.p.rm <- lm(crim ~ poly(rm, 3))
reg.p.age <- lm(crim ~ poly(age, 3))
reg.p.dis <- lm(crim ~ poly(dis, 3))
reg.p.rad <- lm(crim ~ poly(rad, 3))
reg.p.tax <- lm(crim ~ poly(tax, 3))
reg.p.ptratio <- lm(crim ~ poly(ptratio, 3))
reg.p.black <- lm(crim ~ poly(black, 3))
reg.p.lstat <- lm(crim ~ poly(lstat, 3))
reg.p.medv <- lm(crim ~ poly(zn, 3))

summary(reg.p.zn)
summary(reg.p.indus)
summary(reg.p.chas)
summary(reg.p.nox)
summary(reg.p.rm)
summary(reg.p.age)
summary(reg.p.dis)
summary(reg.p.rad)
summary(reg.p.tax)
summary(reg.p.ptratio)
summary(reg.p.black)
summary(reg.p.lstat)
summary(reg.p.medv)