# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 8
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# We will now perform cross-validation on a simulated data set.
# (a) Generate a simulated data set as follows:
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <-  x - 2*x^2 + rnorm(100)

# In this data set, what is n and what is p? Write out the model used to 
# generate the data in equation form.
# (b) Create a scatterplot of X against Y. Comment on what you find.
plot(x, y)
#
# (c) Set a random seed, and then compute the LOOCV errors that result from
# fitting the following four models using least squares:
library(boot)
Data = data.frame(x, y)
set.seed(123)
## i. Y = β0 + β1X + ε
log1 <- glm(y ~ x)
cv.glm(Data, log1)$delta
## ii. Y = β0 + β1X + β2X2 + ε
log2 <- glm(y ~ poly(x, 2))
cv.glm(Data, log2)$delta
## iii. Y = β0 +β1X +β2X2 +β3X3 +ε
log3 <- glm(y ~ poly(x, 3))
cv.glm(Data, log3)$delta
## iv. Y = β0 +β1X +β2X2 +β3X3 +β4X4 +ε.
log4 <- glm(y ~ poly(x, 4))
cv.glm(Data, log4)$delta
# Note you may find it helpful to use the data.frame() function
# to create a single data set containing both X and Y.
# (d) Repeat (c) using another random seed, and report your results.
# Are your results the same as what you got in (c)? Why?
set.seed(321)
## i. Y = β0 + β1X + ε
log1 <- glm(y ~ x)
cv.glm(Data, log1)$delta
## ii. Y = β0 + β1X + β2X2 + ε
log2 <- glm(y ~ poly(x, 2))
cv.glm(Data, log2)$delta
## iii. Y = β0 +β1X +β2X2 +β3X3 +ε
log3 <- glm(y ~ poly(x, 3))
cv.glm(Data, log3)$delta
## iv. Y = β0 +β1X +β2X2 +β3X3 +β4X4 +ε.
log4 <- glm(y ~ poly(x, 4))
cv.glm(Data, log4)$delta
# (e) Which of the models in (c) had the smallest LOOCV error? Is this what
# you expected? Explain your answer.
# (f) Comment on the statistical significance of the coefficient estimates that
# results from fitting each of the models in (c) using least squares. Do these 
# results agree with the conclusions drawn based on the cross-validation 
# results?
summary(log4)


