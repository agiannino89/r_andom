# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 11
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# In this problem, you will develop a model to predict whether a given car gets
# high or low gas mileage based on the Auto data set.
library("ISLR")
head(Auto)
#
# (a) Create a binary variable, mpg01, that contains a 1 if mpg contains a 
# value above its median, and a 0 if mpg contains a value below its median. You
# can compute the median using the median() function. Note you may find it 
# helpful to use the data.frame() function to create a single data set 
# containing both mpg01 and the other Auto variables.
attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto <- data.frame(Auto, mpg01)
#
# (b) Explore the data graphically in order to investigate the association 
# between mpg01 and the other features. Which of the other features seem most
# likely to be useful in predicting mpg01? Scatterplots and boxplots may be 
# useful tools to answer this question. Describe your findings.
cor(Auto[, -9])
plot(as.factor(mpg01), cylinders, xlab = "mpg01", ylab = "cylinders")
plot(as.factor(mpg01), displacement, xlab = "mpg01", ylab = "displacement")
plot(as.factor(mpg01), horsepower, xlab = "mpg01", ylab = "horsepower")
plot(as.factor(mpg01), weight, xlab = "mpg01", ylab = "weight")
plot(as.factor(mpg01), acceleration, xlab = "mpg01", ylab = "acceleration")
plot(as.factor(mpg01), year, xlab = "mpg01", ylab = "year")
#
# (c) Split the data into a training set and a test set.
smp.size <- floor(0.7 * nrow(Auto))
set.seed(123)
train.ind <- sample(seq_len(nrow(Auto)), size = smp.size)
train <- Auto[train.ind, ]
test <- Auto[-train.ind, ]
mpg01.train <- mpg01[train.ind]
mpg01.test <- mpg01[-train.ind]
#
# (d) Perform LDA on the training data in order to predict mpg01 using the 
# variables that seemed most associated with mpg01 in (b). What is the test 
# error of the model obtained?
library("MASS")
lda <-  lda(mpg01 ~ cylinders + weight + displacement + horsepower, 
            data = train)
lda.pred <-  predict(lda, test)
conf1 <- table(lda.pred$class, mpg01.test)
conf1
paste0("Accuracy: " , round(sum(diag(conf1))/sum(conf1), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf1))/sum(conf1), digits = 3))
paste0("Specificity: ", round(1-conf1[3]/sum(conf1[1,]), digits = 3)) 
paste0("Precision: ", round(conf1[4]/sum(conf1[,2]) , digits = 3)) 
paste0("Recall: ", round(conf1[4]/sum(conf1[2,]), digits = 3)) 
#
# (e) Perform QDA on the training data in order to predict mpg01 using the
# variables that seemed most associated with mpg01 in (b). What is the test 
# error of the model obtained?
qda <-  qda(mpg01 ~ cylinders + weight + displacement + horsepower, 
            data = train)
qda.pred <-  predict(qda, test)
conf2 <- table(qda.pred$class, mpg01.test)
conf2
paste0("Accuracy: " , round(sum(diag(conf2))/sum(conf2), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf2))/sum(conf2), digits = 3))
paste0("Specificity: ", round(1-conf2[3]/sum(conf2[1,]), digits = 3)) 
paste0("Precision: ", round(conf2[4]/sum(conf2[,2]) , digits = 3)) 
paste0("Recall: ", round(conf2[4]/sum(conf2[2,]), digits = 3)) 
#
# (f) Perform logistic regression on the training data in order to predict 
# mpg01 using the variables that seemed most associated with mpg01 in (b). What
# is the test error of the model obtained?
log <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, 
           data = train, family = binomial)
summary(log)
log.probs <- predict(log, test, type = "response")
log.pred <- rep(0, length(log.probs))
log.pred[log.probs > 0.5] <- 1
conf3 <- table(log.pred, mpg01.test)
conf3
paste0("Accuracy: " , round(sum(diag(conf3))/sum(conf3), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf3))/sum(conf3), digits = 3))
paste0("Specificity: ", round(1-conf3[3]/sum(conf3[1,]), digits = 3)) 
paste0("Precision: ", round(conf3[4]/sum(conf3[,2]) , digits = 3)) 
paste0("Recall: ", round(conf3[4]/sum(conf3[2,]), digits = 3)) 
#
# (g) Perform KNN on the training data, with several values of K, in order to
# predict mpg01. Use only the variables that seemed most associated with mpg01
# in (b). What test errors do you obtain? Which value of K seems to perform the
# best on this data set?
library("class")
train.knn = cbind(cylinders, weight, displacement, horsepower)[train.ind, ]
test.knn = cbind(cylinders, weight, displacement, horsepower)[-train.ind, ]
set.seed(1)
knn.pred <-  knn(train.knn, test.knn, mpg01.train, k = 10)
#k = 1
knn1.pred = knn(train.knn, test.knn, mpg01.train, k = 1)
conf4 <- table(knn1.pred, mpg01.test)
conf4
paste0("Accuracy: " , round(sum(diag(conf4))/sum(conf4), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf4))/sum(conf4), digits = 3))
paste0("Specificity: ", round(1-conf4[3]/sum(conf4[1,]), digits = 3)) 
paste0("Precision: ", round(conf4[4]/sum(conf4[,2]) , digits = 3)) 
paste0("Recall: ", round(conf4[4]/sum(conf4[2,]), digits = 3)) 
#k = 10
knn10.pred = knn(train.knn, test.knn, mpg01.train, k = 10)
conf5 <- table(knn10.pred, mpg01.test)
conf5
paste0("Accuracy: " , round(sum(diag(conf5))/sum(conf5), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf5))/sum(conf5), digits = 3))
paste0("Specificity: ", round(1-conf5[3]/sum(conf5[1,]), digits = 3)) 
paste0("Precision: ", round(conf5[4]/sum(conf5[,2]) , digits = 3)) 
paste0("Recall: ", round(conf5[4]/sum(conf5[2,]), digits = 3)) 
#k = 100
knn100.pred = knn(train.knn, test.knn, mpg01.train, k = 100)
conf6 <- table(knn100.pred, mpg01.test)
conf6
paste0("Accuracy: " , round(sum(diag(conf6))/sum(conf6), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf6))/sum(conf6), digits = 3))
paste0("Specificity: ", round(1-conf6[3]/sum(conf6[1,]), digits = 3)) 
paste0("Precision: ", round(conf6[4]/sum(conf6[,2]) , digits = 3)) 
paste0("Recall: ", round(conf6[4]/sum(conf6[2,]), digits = 3)) 
