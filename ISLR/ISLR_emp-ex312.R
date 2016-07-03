# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 12
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This problem involves simple linear regression without an intercept.
# (a) Recall that the coefficient estimate b for the linear regression of Y 
# onto X without an intercept is given by (3.38). Under what circumstance is 
# the coefficient estimate for the regression of X onto Y the same as the 
# coefficient estimate for the regression of Y onto X?
# b_yx = cov(x,y)/Var(x), while b_xy = cov(x,y)/Var(y). 
# Therefore, b_yx = b_xy <-> sum(x^2) = sum(y^2)
#
# (b) Generate an example in R with n = 100 observations in which the 
# coefficient estimate for the regression of X onto Y is different from the 
# coefficient estimate for the regression of Y onto X.
set.seed (2)
x <- 1:100
y1 <- 5*x + rnorm(100)
reg.yx1 <- lm(y1 ~ x + 0)
summary(reg.yx1)
reg.xy1 <- lm(x ~ y1 + 0)
summary(reg.xy1)
#
# (c) Generate an example in R with n = 100 observations in which the 
# coefficient estimate for the regression of X onto Y is the same as the 
# coefficient estimate for the regression of Y onto X.
y2 <- 100:1
sum(x^2)
sum(y2^2)
#
reg.yx2 <- lm(y2 ~ x + 0)
summary(reg.yx2)
reg.xy2 <- lm(x ~ y2 + 0)
summary(reg.xy2)

