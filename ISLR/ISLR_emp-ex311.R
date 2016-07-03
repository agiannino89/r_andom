# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 11
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# In this problem we will investigate the t-statistic for the null hypothesis 
# H0 : β = 0 in simple linear regression without an intercept. To begin, we 
# generate a predictor x and a response y as follows.
set.seed (1)
x <- rnorm(100)
y <- 2*x + rnorm(100)
#
# (a) Perform a simple linear regression of y onto x, without an intercept. 
# Report the coefficient estimate b, the standard error of this coefficient 
# estimate, and the t-statistic and p-value associ- ated with the null 
# hypothesis H0 : β = 0. Comment on these results. (You can perform regression
# without an intercept using the command lm(y∼x+0).)
reg.yx <- lm(y ~ x + 0)
summary(reg.yx)
#
# (b) Now perform a simple linear regression of x onto y without an intercept,
# and report the coefficient estimate, its standard error, and the 
# corresponding t-statistic and p-values associated with the null hypothesis 
# H0 : β = 0. Comment on these results.
reg.xy <- lm(x ~ y + 0)
summary(reg.xy)
#
# (c) What is the relationship between the results obtained in (a) and (b)?
# y = 2x + e -> x = 0.5y- 0.5e 
#
# (d) For the regression of Y onto X without an intercept, the t-statistic for
# H0 : β = 0 takes the form b/SE(b), where b is given by (3.38), and where 
# SE(b) is in the book. These formulas are slightly different from those given
# in Sections 3.1.1 and 3.1.2, since here we are performing regression without
# an intercept.) Show algebraically, and confirm numerically in R, that the 
# t-statistic can be written as in the book.
n <- length(x)
t <- sqrt(n - 1)*(x %*% y)/sqrt(sum(x^2) * sum(y^2) - (x %*% y)^2)
as.numeric(t)
#
# (e) Using the results from (d), argue that the t-statistic for the regression 
# of y onto x is the same as the t-statistic for the regression of x onto y.
# It is easy to see that x and y are interchangeable from the equation in the 
# book.
#
# (f) In R, show that when regression is performed with an intercept, the 
# t-statistic for H0 : β1 = 0 is the same for the regression of y onto x as it
# is for the regression of x onto y.
reg.i1 <- lm(y ~ x)
summary(reg.i1)
reg.i2 <- lm(x ~ y)
summary(reg.i2)



