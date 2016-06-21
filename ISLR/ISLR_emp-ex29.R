# ============================================
# An Introduction to Statistical Learning
#           Chapter 2
#
# R-Code for Applied Exercises 9
#
# Author: Antonio Giannino
# ============================================
rm(list = ls());
#
# This exercise involves the Auto data set studied in the lab. 
# Make sure that the missing values have been removed from the data.
Auto <- read.csv("Datasets/Auto.csv")
# (a) Which of the predictors are quantitative, and which are qualitative?
str(Auto)
summary(Auto)
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$origin <- as.factor(Auto$origin)
Auto$year <- as.factor(Auto$year)
Auto <- na.omit(Auto)
idx_quant <- sapply(Auto, is.numeric)
Auto_quant <- Auto[, idx_quant]
#
# (b) What is the range of each quantitative predictor? You can answer this using the 
# range() function.
# (c) What is the mean and standard deviation of each quantitative predictor?
for (i in 1:dim(Auto_quant)[2]) {
  print(paste0("Variable: ", i, " Range:"))
  print(range(Auto_quant[,i]))
  print(paste0("Mean: ", mean(Auto_quant[,i])))
  print(paste0("Standard Deviation: ", sd((Auto_quant[,i]))))
}
# (d) Now remove the 10th through 85th observations. What is the range, mean, and 
# standard deviation of each predictor in the subset of the data that remains?
Auto_quant_sub <- Auto_quant[-c(10:85),]
for (i in 1:dim(Auto_quant_sub)[2]) {
  print(paste0("Variable: ", i, ", Range:"))
  print(range(Auto_quant_sub[,i]))
  print(paste0("Mean: ", mean(Auto_quant_sub[,i])))
  print(paste0("Standard Deviation: ",sd((Auto_quant_sub[,i]))))
}

# (e) Using the full data set, investigate the predictors graphically, using 
# scatterplots or other tools of your choice. Create some plots highlighting 
# the relationships among the predictors. Comment on your findings.
plot(Auto$cylinders, Auto$displacement)
plot(Auto$cylinders, Auto$weight)
plot(Auto$cylinders, Auto$acceleration)
#
plot(Auto$displacement, Auto$horsepower)
plot(Auto$displacement, Auto$weight)
plot(Auto$displacement, Auto$acceleration)
#
plot(Auto$year, Auto$acceleration)
plot(Auto$year, Auto$weight)
#
# (f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other 
# variables. Do your plots suggest that any of the other variables might be useful 
# in predicting mpg? Justify your answer.
plot(Auto$cylinders, Auto$mpg)
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)
plot(Auto$year, Auto$mpg)
plot(Auto$origin, Auto$mpg)



