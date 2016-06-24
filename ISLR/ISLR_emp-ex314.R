# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 14
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This problem focuses on the collinearity problem.
# (a) Perform the following commands in R:
set.seed(1)
x1 <- runif(100)
x2 <- 0.5*x1 + rnorm(100)/10
y <- 2 + 2*x1 + 0.3*x2 + rnorm(100) 
# The last line corresponds to creating a linear model in which y is a function of x1
# and x2. Write out the form of the linear model. What are the regression coefficients?
#
# (b) What is the correlation between x1 and x2? Create a scatterplot displaying the 
# relationship between the variables.
cor(x1, x2)
plot(x1, x2)
#
# (c) Using this data, fit a least squares regression to predict y using x1 and x2. 
# Describe the results obtained. What are b0, b1, and b2? How do these relate to the 
# true β0, β1, and β2? Can you reject the null hypothesis H0 : β1 = 0? How about the 
# null hypothesis H0 : β2 = 0?
reg_1 <- lm(y ~ x1 + x2)
summary(reg_1)
# (d) Now fit a least squares regression to predict y using only x1. Comment on your
# results. Can you reject the null hypothesis H0 :β1 =0?
reg_2 <- lm(y ~ x1)
summary(reg_2)
#
# (e) Now fit a least squares regression to predict y using only x2. Comment on your
# results. Can you reject the null hypothesis H0 :β1 =0?
reg_3 <- lm(y ~ x2)
summary(reg_3)
#
# (f) Do the results obtained in (c)–(e) contradict each other? Explain your answer.
# (g) Now suppose we obtain one additional observation, which was unfortunately 
# mismeasured.
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y,6)
#Re-fit the linear models from (c) to (e) using this new data. What effect does this
# new observation have on the each of the models? In each model, is this observation 
# an outlier? A high-leverage point? Both? Explain your answers.
reg_4 <- lm(y ~ x1 + x2)
reg_5 <- lm(y ~ x1)
reg_6 <- lm(y ~ x2)
summary(reg_4)
summary(reg_5)
summary(reg_6)
par(mfrow = c(2, 2))
plot(reg_4)
par(mfrow = c(2, 2))
plot(reg_5)
par(mfrow = c(2, 2))
plot(reg_6)








