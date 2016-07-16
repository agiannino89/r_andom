# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 5
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# In Chapter 4, we used logistic regression to predict the probability of 
# default using income and balance on the Default data set. We will now 
# estimate the test error of this logistic regression model using the 
# validation set approach. Do not forget to set a random seed before beginning
# your analysis.
library("ISLR")
head(Default)
# (a) Fit a logistic regression model that uses income and balance to predict
# default.
log <- glm(default ~ income + balance, data = Default, family = binomial)
summary(log)
# (b) Using the validation set approach, estimate the test error of this model.
# In order to do this, you must perform the following steps:
## i. Split the sample set into a training set and a validation set.
smp_size1 <- floor(0.5 * nrow(Default))
set.seed(123)
train1.ind <- sample(seq_len(nrow(Default)), size = smp_size1)
train1 <- Default[train1.ind, ]
valid1 <- Default[-train1.ind, ]
default.train1 <- Default$default[train1.ind]
default.valid1 <- Default$default[-train1.ind]
## ii. Fit a multiple logistic regression model using only the training 
## observations.
log1.train <- glm(default ~ income + balance, data = train1, family = binomial)
summary(log1.train)
## iii. Obtain a prediction of default status for each individual in the 
## validation set by computing the posterior probability of default for that
## individual, and classifying the individual to the default category if the 
## posterior probability is greater than 0.5.
log1.probs <- predict(log1.train, valid1, type = "response")
log1.pred <- rep(0, length(log1.probs))
log1.pred[log1.probs > 0.5] <- 1
## iv. Compute the validation set error, which is the fraction of the 
## observations in the validation set that are misclassified.
conf1 <- table(log1.pred, default.valid1)
conf1
paste0("Validation Set Error: ", round(1-sum(diag(conf1))/sum(conf1), digits = 3))
# (c) Repeat the process in (b) three times, using three different splits of 
# the observations into a training set and a validation set. Comment on the 
# results obtained.
# Train = 70% of the DF
smp_size2 <- floor(0.7 * nrow(Default))
set.seed(123)
train2.ind <- sample(seq_len(nrow(Default)), size = smp_size2)
train2 <- Default[train2.ind, ]
valid2 <- Default[-train2.ind, ]
default.train2 <- Default$default[train2.ind]
default.valid2 <- Default$default[-train2.ind]
log2.train <- glm(default ~ income + balance, data = train2, family = binomial)
summary(log2.train)
log2.probs <- predict(log2.train, valid2, type = "response")
log2.pred <- rep(0, length(log2.probs))
log2.pred[log2.probs > 0.5] <- 1
conf2 <- table(log2.pred, default.valid2)
conf2
paste0("Validation Set Error: ", round(1-sum(diag(conf2))/sum(conf2), digits = 3))
# Train = 90% of the DF
smp_size3 <- floor(0.9 * nrow(Default))
set.seed(123)
train3.ind <- sample(seq_len(nrow(Default)), size = smp_size3)
train3 <- Default[train3.ind, ]
valid3 <- Default[-train3.ind, ]
default.train3 <- Default$default[train3.ind]
default.valid3 <- Default$default[-train3.ind]
log3.train <- glm(default ~ income + balance, data = train3, family = binomial)
log3.probs <- predict(log3.train, valid3, type = "response")
log3.pred <- rep(0, length(log3.probs))
log3.pred[log3.probs > 0.5] <- 1
conf3 <- table(log3.pred, default.valid3)
conf3
paste0("Validation Set Error: ", round(1-sum(diag(conf3))/sum(conf3), digits = 3))
# Train = 30% of the DF
smp_size4 <- floor(0.3 * nrow(Default))
set.seed(123)
train4.ind <- sample(seq_len(nrow(Default)), size = smp_size4)
train4 <- Default[train4.ind, ]
valid4 <- Default[-train4.ind, ]
default.train4 <- Default$default[train4.ind]
default.valid4 <- Default$default[-train4.ind]
log4.train <- glm(default ~ income + balance, data = train4, family = binomial)
log4.probs <- predict(log4.train, valid4, type = "response")
log4.pred <- rep(0, length(log4.probs))
log4.pred[log4.probs > 0.5] <- 1
conf4 <- table(log4.pred, default.valid4)
conf4
paste0("Validation Set Error: ", round(1-sum(diag(conf4))/sum(conf4), digits = 3))
# (d) Now consider a logistic regression model that predicts the probability of
# default using income, balance, and a dummy variable for student. Estimate the
# test error for this model using the validation set approach. Comment on 
# whether or not including a dummy variable for student leads to a reduction in
# the test error rate.
# Train = 90% of the DF
log5.train <- glm(default ~ income + balance + student, data = train1, 
                  family = binomial)
log5.probs <- predict(log5.train, valid1, type = "response")
log5.pred <- rep(0, length(log5.probs))
log5.pred[log5.probs > 0.5] <- 1
conf5 <- table(log5.pred, default.valid1)
conf5
paste0("Validation Set Error: ", round(1-sum(diag(conf5))/sum(conf5), digits = 3))

