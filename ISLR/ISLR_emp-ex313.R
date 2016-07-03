# ============================================
# An Introduction to Statistical Learning
#           Chapter 3
#
# R-Code for Applied Exercises 13
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# In this exercise you will create some simulated data and will fit simple 
# linear regression models to it. Make sure to use set.seed(1) prior to 
# starting part (a) to ensure consistent results.
set.seed(1)
# (a) Using the rnorm() function, create a vector, x, containing 100 
# observations drawn from a N(0,1) distribution. This represents a feature, X.
x1 <- rnorm(100)
#
# (b) Using the rnorm() function, create a vector, eps, containing 100 
# observations drawn from a N(0,0.25) distribution i.e. a normal distribution 
# with mean zero and variance 0.25.
eps1 <- rnorm(100, sd = sqrt(0.25))
#
# (c) Using x and eps, generate a vector y according to the model
# Y =−1+0.5X+ε. (3.39)
y1 <- -1 + 0.5 * x1 + eps1
# What is the length of the vector y? What are the values of β0 and β1 in this
# linear model?
length(y1)
#
# (d) Create a scatterplot displaying the relationship between x and y. Comment
# on what you observe.
plot(x1, y1)
#
# (e) Fit a least squares linear model to predict y using x. Comment on the 
# model obtained. How do b0 and b1 compare to β0 and β1?
reg1 <- lm(y1 ~ x1)
summary(reg1)
# (f) Display the least squares line on the scatterplot obtained in (d). Draw 
# the population regression line on the plot, in a different color. Use the 
# legend() command to create an appropriate legend.
plot(x1, y1)
abline(reg1, col = "red")
abline(-1, 0.5, col = "blue")
#
# (g) Now fit a polynomial regression model that predicts y using x and x2. Is 
# there evidence that the quadratic term improves the model fit? Explain your 
# answer.
reg.poly1 <- lm(y1 ~ x1 + I(x1^2))
summary(reg.poly1)
#
# (h) Repeat (a)–(f) after modifying the data generation process in such a way
# that there is less noise in the data. The model (3.39) should remain the same.
# You can do this by decreasing the vari- ance of the normal distribution used
# to generate the error term ε in (b). Describe your results.
set.seed(1)
eps2 <- rnorm(100, sd = 0.1)
x2 <- rnorm(100)
y2 <- -1 + 0.5 * x2 + eps2
plot(x2, y2)
reg2 <- lm(y2 ~ x2)
summary(reg2)
#
# (i) Repeat (a)–(f) after modifying the data generation process in such a way
# that there is more noise in the data. The model (3.39) should remain the same.
# You can do this by increasing the variance of the normal distribution used to
# generate the error term ε in (b). Describe your results.
set.seed(1)
eps3 <- rnorm(100, sd = 0.75)
x3 <- rnorm(100)
y3 <- -1 + 0.5 * x3 + eps3
plot(x3, y3)
reg3 <- lm(y3 ~ x3)
summary(reg3)
#
# (j) What are the confidence intervals for β0 and β1 based on the original 
# data set, the noisier data set, and the less noisy data set? Comment on your
# results.
confint(reg1)
confint(reg2)
confint(reg3)



