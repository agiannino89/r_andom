# ============================================
# An Introduction to Statistical Learning
#           Chapter 2
#
# R-Code for Applied Exercises 10
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This exercise involves the Boston housing data set.
# (a) To begin, load in the Boston data set. The Boston data set is
# part of the MASS library in R.
library("MASS")
# Now the data set is contained in the object Boston.
head(Boston)
# Read about the data set:
?Boston
# How many rows are in this data set? How many columns? 
# What do the rows and columns represent? See the help file.
#
# (b) Make some pairwise scatterplots of the predictors (columns) in this data set. 
# Describe your findings.
pairs(Boston)
#
# (c) Are any of the predictors associated with per capita crime rate? 
# If so, explain the relationship.
plot(Boston$zn, Boston$crim)
plot(Boston$indus, Boston$crim)
plot(Boston$chas, Boston$crim)
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$indus, Boston$crim)
plot(Boston$dis, Boston$crim)
plot(Boston$rad, Boston$crim)
plot(Boston$tax, Boston$crim)
plot(Boston$ptratio, Boston$crim)
plot(Boston$black, Boston$crim)
plot(Boston$lstat, Boston$crim)
plot(Boston$medv, Boston$crim)
#
# (d) Do any of the suburbs of Boston appear to have particularly high crime rates? 
# Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.
hist(Boston$crim, breaks = 50)
range(Boston$crim)
which(Boston$crim > 50)

hist(Boston$tax, breaks = 50)
range(Boston$tax)
which(Boston$tax > 600)

hist(Boston$ptratio, breaks = 50)
range(Boston$ptratio)
which(Boston$ptratio > 21)
# 
# (e) How many of the suburbs in this data set bound the Charles river?
sum(Boston$chas)
#
# (f) What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)
#
# (g) Which suburb of Boston has lowest median value of owner-occupied homes? 
which.min(Boston$medv)
# What are the values of the other predictors for that suburb, and how do those 
# values compare to the overall ranges for those predictors? Comment on your findings.
Boston[which.min(Boston$medv),]
summary(Boston)
# (h) In this data set, how many of the suburbs average more than seven rooms per 
# dwelling? More than eight rooms per dwelling? Comment on the suburbs that average 
# more than eight rooms per dwelling.
sum(Boston$rm > 7)
sum(Boston$rm > 8)


