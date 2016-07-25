# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 9
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# We will now consider the Boston housing data set, from the MASS library.
library("MASS")
library("boot")
head(Boston)
#
# (a) Based on this data set, provide an estimate for the population mean of
# medv. Call this estimate μˆ.
mu.hat <- mean(Boston$medv)
mu.hat
#
# (b) Provide an estimate of the standard error of μˆ. Interpret this result.
# Hint: We can compute the standard error of the sample mean by dividing the 
# sample standard deviation by the square root of the number of observations.
se.mu.hat <- sd(Boston$medv) / sqrt(length(Boston$medv))
#
# (c) Now estimate the standard error of μˆ using the bootstrap. How does this 
# compare to your answer from (b)?
boot.mean <- function(data, index) 
  return(mean(data[index]))

set.seed(123)
boot <- boot(Boston$medv, boot.mean, 1000)
boot
#
# (d) Based on your bootstrap estimate from (c), provide a 95 % confidence 
# interval for the mean of medv. Compare it to the results obtained using 
# t.test(Boston$medv).
# Hint: You can approximate a 95 % confidence interval using the formula 
# [μˆ − 2SE(μˆ), μˆ + 2SE(μˆ)].
c(boot$t0 - 2 * 0.4183554, boot$t0 + 2 * 0.4183554)
t.test(Boston$medv)
#
# (e) Based on this data set, provide an estimate, μˆmed, for the median value
# of medv in the population.
medv.med <- median(Boston$medv)
medv.med
#
# (f) We now would like to estimate the standard error of μˆmed. Unfortunately,
# there is no simple formula for computing the standard error of the median. 
# Instead, estimate the standard error of the median using the bootstrap. 
# Comment on your findings.
boot.median <- function(data, index) 
  return(median(data[index]))

boot(Boston$medv, boot.median, 1000)
# (g) Based on this data set, provide an estimate for the tenth percentile of 
# medv in Boston suburbs. Call this quantity μˆ0.1. (You can use the quantile()
# function.)
medv.hat.01 <- quantile(Boston$medv, c(0.1))
medv.hat.01
# (h) Use the bootstrap to estimate the standard error of μˆ0.1. Comment on 
# your findings.
boot.01 = function(data, index) 
  return(quantile(data[index], c(0.1)))

boot(Boston$medv, boot.01, 1000)

