# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 7
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in
# order to compute the LOOCV test error estimate. Alternatively, one could 
# compute those quantities using just the glm() and predict.glm() functions, 
# and a for loop. You will now take this ap- proach in order to compute the
# LOOCV error for a simple logistic regression model on the Weekly data set.
# Recall that in the context of classification problems, the LOOCV error is 
# given in (5.4).
library("ISLR")
summary(Weekly)
set.seed(1)
attach(Weekly)
# (a) Fit a logistic regression model that predicts Direction using Lag1 and
# Lag2.
log1 <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = binomial)
summary(log1)
#
# (b) Fit a logistic regression model that predicts Direction using Lag1 and
# Lag2 using all but the first observation.
log2 <- glm(Direction ~ Lag1 + Lag2, data =  Weekly[-1, ], family = binomial)
summary(log2)
#
# (c) Use the model from (b) to predict the direction of the first observation. 
# You can do this by predicting that the first observation will go up if 
# P(Direction="Up"|Lag1, Lag2) > 0.5. Was this ob- servation correctly 
# classified?
predict.glm(log2, Weekly[1, ], type = "response") > 0.5
#
# (d) Write a for loop from i=1 to i=n, where n is the number of observations 
# in the data set, that performs each of the following steps:
## i. Fit a logistic regression model using all but the ith observation to
## predict Direction using Lag1 and Lag2.
## ii. Compute the posterior probability of the market moving up for the ith
# observation.
## iii. Use the posterior probability for the ith observation in order to 
## predict whether or not the market moves up.
## iv. Determine whether or not an error was made in predicting the direction 
## for the ith observation. If an error was made, then indicate this as a 1, 
## and otherwise indicate it as a 0.
count <- rep(0, dim(Weekly)[1])
for (i in 1:(dim(Weekly)[1])) {
  log = glm(Direction ~ Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  is.up = predict.glm(log, Weekly[i, ], type = "response") > 0.5
  is.true.up = Weekly[i, ]$Direction == "Up"
  if (is.up != is.true.up) 
    count[i] = 1
}
sum(count)
# (e) Take the average of the n numbers obtained in (d)iv in order to obtain the 
# LOOCV estimate for the test error. Comment on the results.
mean(count)




