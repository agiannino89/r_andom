# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 13
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# Using the Boston data set, fit classification models in order to predict 
# whether a given suburb has a crime rate above or below the median. Explore
# logistic regression, LDA, and KNN models using various sub-sets of the 
# predictors. Describe your findings.
library(MASS)
attach(Boston)
summary(Boston)
head(Boston)
crime01 <-  rep(0, length(crim))
crime01[crim > median(crim)] <- 1
Boston <- data.frame(Boston, crime01)
cor(Boston)

plot(as.factor(crime01), zn, xlab = "mpg01", ylab = "zn")
plot(as.factor(crime01), indus, xlab = "mpg01", ylab = "indus")
plot(as.factor(crime01), nox, xlab = "mpg01", ylab = "nox")
plot(as.factor(crime01), rm, xlab = "mpg01", ylab = "rm")
plot(as.factor(crime01), age, xlab = "mpg01", ylab = "age")
plot(as.factor(crime01), dis, xlab = "mpg01", ylab = "dis")
plot(as.factor(crime01), rad, xlab = "mpg01", ylab = "rad")
plot(as.factor(crime01), tax, xlab = "mpg01", ylab = "tax")
plot(as.factor(crime01), ptratio, xlab = "mpg01", ylab = "ptratio")

smp_size <- floor(0.7 * nrow(Boston))
set.seed(123)
train.ind <- sample(seq_len(nrow(Boston)), size = smp_size)
train <- Boston[train.ind, ]
test <- Boston[-train.ind, ]
crime01.train <- crime01[train.ind]
crime01.test <- crime01[-train.ind]

library("MASS")
lda <-  lda(crime01 ~ . - crime01 - crim, data = train)
lda.pred <-  predict(lda, test)
conf1 <- table(lda.pred$class, crime01.test)
conf1
paste0("Accuracy: " , round(sum(diag(conf1))/sum(conf1), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf1))/sum(conf1), digits = 3))
paste0("Specificity: ", round(1-conf1[3]/sum(conf1[1,]), digits = 3)) 
paste0("Precision: ", round(conf1[4]/sum(conf1[,2]) , digits = 3)) 
paste0("Recall: ", round(conf1[4]/sum(conf1[2,]), digits = 3)) 

qda <-  qda(crime01 ~ . - crime01 - crim, data = train)
qda.pred <-  predict(qda, test)
conf2 <- table(qda.pred$class, crime01.test)
conf2
paste0("Accuracy: " , round(sum(diag(conf2))/sum(conf2), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf2))/sum(conf2), digits = 3))
paste0("Specificity: ", round(1-conf2[3]/sum(conf2[1,]), digits = 3)) 
paste0("Precision: ", round(conf2[4]/sum(conf2[,2]) , digits = 3)) 
paste0("Recall: ", round(conf2[4]/sum(conf2[2,]), digits = 3)) 

log <- glm(crime01 ~. - crime01 - crim , data = train, family = binomial)
summary(log)
log.probs <- predict(log, test, type = "response")
log.pred <- rep(0, length(log.probs))
log.pred[log.probs > 0.5] <- 1
conf3 <- table(log.pred, crime01.test)
conf3
paste0("Accuracy: " , round(sum(diag(conf3))/sum(conf3), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf3))/sum(conf3), digits = 3))
paste0("Specificity: ", round(1-conf3[3]/sum(conf3[1,]), digits = 3)) 
paste0("Precision: ", round(conf3[4]/sum(conf3[,2]) , digits = 3)) 
paste0("Recall: ", round(conf3[4]/sum(conf3[2,]), digits = 3)) 

library("class")
train.knn = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                  lstat, medv)[train.ind, ]
test.knn = cbind(zn, indus, chas, nox, rm, age, dis, rad, tax, ptratio, black, 
                 lstat, medv)[-train.ind, ]
set.seed(1)
knn.pred <-  knn(train.knn, test.knn, crime01.train, k = 10)
#k = 1
knn1.pred = knn(train.knn, test.knn, crime01.train, k = 1)
conf4 <- table(knn1.pred, crime01.test)
conf4
paste0("Accuracy: " , round(sum(diag(conf4))/sum(conf4), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf4))/sum(conf4), digits = 3))
paste0("Specificity: ", round(1-conf4[3]/sum(conf4[1,]), digits = 3)) 
paste0("Precision: ", round(conf4[4]/sum(conf4[,2]) , digits = 3)) 
paste0("Recall: ", round(conf4[4]/sum(conf4[2,]), digits = 3)) 
#k = 10
knn10.pred = knn(train.knn, test.knn, crime01.train, k = 10)
conf5 <- table(knn10.pred, crime01.test)
conf5
paste0("Accuracy: " , round(sum(diag(conf5))/sum(conf5), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf5))/sum(conf5), digits = 3))
paste0("Specificity: ", round(1-conf5[3]/sum(conf5[1,]), digits = 3)) 
paste0("Precision: ", round(conf5[4]/sum(conf5[,2]) , digits = 3)) 
paste0("Recall: ", round(conf5[4]/sum(conf5[2,]), digits = 3)) 
#k = 100
knn100.pred = knn(train.knn, test.knn, crime01.train, k = 100)
conf6 <- table(knn100.pred, crime01.test)
conf6
paste0("Accuracy: " , round(sum(diag(conf6))/sum(conf6), digits = 3))
paste0("Test Error Rate: ", round(1-sum(diag(conf6))/sum(conf6), digits = 3))
paste0("Specificity: ", round(1-conf6[3]/sum(conf6[1,]), digits = 3)) 
paste0("Precision: ", round(conf6[4]/sum(conf6[,2]) , digits = 3)) 
paste0("Recall: ", round(conf6[4]/sum(conf6[2,]), digits = 3)) 


