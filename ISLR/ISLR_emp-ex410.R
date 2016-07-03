# ============================================
# An Introduction to Statistical Learning
#           Chapter 4
#
# R-Code for Applied Exercises 10
#
# Author: Antonio Giannino
# ============================================ 
rm(list = ls());
#
# This question should be answered using the Weekly data set, which is part of
# the ISLR package. This data is similar in nature to the Smarket data from 
# this chapter’s lab, except that it contains 1,089 weekly returns for 21 
# years, from the beginning of 1990 to the end of 2010.
library("ISLR")
head(Weekly)
#
# (a) Produce some numerical and graphical summaries of the Weekly data. Do 
# there appear to be any patterns?
attach(Weekly)
summary(Weekly)
cor(Weekly[,-9])
plot(Volume)
pairs(Weekly)
#
# (b) Use the full data set to perform a logistic regression with Direction as
# the response and the five lag variables plus Volume as predictors. Use the 
# summary function to print the results. Do any of the predictors appear to be
# statistically significant? If so, which ones?
log1 <- glm(Direction ~ Lag1 + Lag2 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
            data = Weekly, family = binomial)
summary(log1)
# Interept (**) and Lag2 (*).
#
# (c) Compute the confusion matrix and overall fraction of correct predictions.
# Explain what the confusion matrix is telling you about the types of mistakes 
# made by logistic regression.
#
# First, note that for accuracy with can either compute the confusion matrix
# and then use sum(diag(conf))/sum(conf) or mean(pred == Direction.test).
log1.probs <-  predict(log1, type = "response")
log1.pred  <-  rep("Down", length(log1.probs))
log1.pred[log1.probs > 0.5] = "Up"
conf1 <- table(log1.pred, Direction)
conf1
paste0("Accuracy: ", round(sum(diag(conf1))/sum(conf1), digits = 3))
paste0("Specificity: ", round(1-conf1[3]/sum(conf1[1,]), digits = 3)) 
paste0("Precision: ", round(conf1[4]/sum(conf1[,2]) , digits = 3)) 
paste0("Recall: ", round(conf1[4]/sum(conf1[2,]), digits = 3)) 

# (d) Now fit the logistic regression model using a training data period from
# 1990 to 2008, with Lag2 as the only predictor. Compute the confusion matrix
# and the overall fraction of correct predictions for the held out data (that 
# is, the data from 2009 and 2010).
train  <-  (Year < 2009)
Weekly.test <-  Weekly[!train, ]
log2 <-  glm(Direction ~ Lag2, data = Weekly, family = binomial, 
             subset = train)
log2.probs  <-  predict(log2, Weekly.test, type = "response")
log2.pred  <-  rep("Down", length(log2.probs))
log2.pred[log2.probs > 0.5] <-  "Up"
Direction.test <-  Direction[!train]
conf2 <- table(log2.pred, Direction.test)
conf2
paste0("Accuracy: ", round(sum(diag(conf2))/sum(conf2), digits = 3))
paste0("Specificity: ", round(1-conf2[3]/sum(conf2[1,]), digits = 3)) 
paste0("Precision: ", round(conf2[4]/sum(conf2[,2]) , digits = 3)) 
paste0("Recall: ", round(conf2[4]/sum(conf2[2,]), digits = 3)) 
#
# (e) Repeat (d) using LDA.
library("MASS")
lda <-  lda(Direction ~ Lag2, data = Weekly, subset = train)
lda.pred <-  predict(lda, Weekly.test)
conf3 <- table(lda.pred$class, Direction.test)
conf3
paste0("Accuracy: ", round(sum(diag(conf3))/sum(conf3), digits = 3))
paste0("Specificity: ", round(1-conf3[3]/sum(conf3[1,]), digits = 3)) 
paste0("Precision: ", round(conf3[4]/sum(conf3[,2]) , digits = 3)) 
paste0("Recall: ", round(conf3[4]/sum(conf3[2,]), digits = 3)) 
# 
# (f) Repeat (d) using QDA.
qda <-  qda(Direction ~ Lag2, data = Weekly, subset = train)
qda.pred <-  predict(qda, Weekly.test)$class
conf4 <- table(qda.pred, Direction.test)
conf4
paste0("Accuracy: ", round(sum(diag(conf4))/sum(conf4), digits = 3))
paste0("Specificity: ", round(1-conf4[3]/sum(conf4[1,]), digits = 3)) 
paste0("Precision: ", round(conf4[4]/sum(conf4[,2]) , digits = 3)) 
paste0("Recall: ", round(conf4[4]/sum(conf4[2,]), digits = 3)) 
#
# (g) Repeat (d) using KNN with K = 1.
library("class")
train.knn <-  as.matrix(Lag2[train])
test.knn <-  as.matrix(Lag2[!train])
train.dir  <-  Direction[train]
set.seed(1)
knn.pred <-  knn(train.knn, test.knn, train.dir, k = 1)
conf5 <- table(knn.pred, Direction.test)
conf5
paste0("Accuracy: ", round(sum(diag(conf5))/sum(conf5), digits = 3))
paste0("Specificity: ", round(1-conf5[3]/sum(conf5[1,]), digits = 3)) 
paste0("Precision: ", round(conf5[4]/sum(conf5[,2]) , digits = 3)) 
paste0("Recall: ", round(conf5[4]/sum(conf5[2,]), digits = 3)) 
#
# (h) Which of these methods appears to provide the best results on
# this data?
#
# (i) Experiment with different combinations of predictors, including possible 
# transformations and interactions, for each of the methods. Report the
# variables, method, and associated confusion matrix that appears to provide 
# the best results on the held out data. Note that you should also experiment
# with values for K in the KNN classifier

# Logistic regression with Lag1:Lag2
log.i <- glm(Direction ~ Lag1*Lag2, data = Weekly, family = binomial, 
             subset = train)
summary(log.i)
log.i.probs <- predict(log.i, Weekly.test, type = "response")
log.i.pred <-  rep("Down", length(log.i.probs))
log.i.pred[log.i.probs > 0.5] = "Up"
conf6 <- table(log.i.pred, Direction.test)
conf6
paste0("Accuracy: ", round(sum(diag(conf6))/sum(conf6), digits = 3))
paste0("Specificity: ", round(1-conf6[3]/sum(conf6[1,]), digits = 3)) 
paste0("Precision: ", round(conf6[4]/sum(conf6[,2]) , digits = 3)) 
paste0("Recall: ", round(conf6[4]/sum(conf6[2,]), digits = 3)) 

# LDA with Lag1 interaction with Lag2
lda.i <-  lda(Direction ~ Lag1*Lag2, data = Weekly, subset = train)
lda.i.pred <-  predict(lda.i, Weekly.test)
mean(lda.i.pred$class == Direction.test) # Accuracy

# QDA with abs(Lag2)
qda.a = qda(Direction ~ Lag2 + abs(Lag2), data = Weekly, subset = train)
qda.a.class = predict(qda.a, Weekly.test)$class
conf7 <-table(qda.a.class, Direction.test)
conf7
paste0("Accuracy: ", round(sum(diag(conf7))/sum(conf7), digits = 3))
paste0("Specificity: ", round(1-conf7[3]/sum(conf7[1,]), digits = 3)) 
paste0("Precision: ", round(conf7[4]/sum(conf7[,2]) , digits = 3)) 
paste0("Recall: ", round(conf7[4]/sum(conf7[2,]), digits = 3)) 

# KNN k = 10
knn10.pred <-  knn(train.knn, test.knn, train.dir, k = 10)
conf8 <- table(knn10.pred, Direction.test)
conf8
paste0("Accuracy: ", round(sum(diag(conf8))/sum(conf8), digits = 3))
paste0("Specificity: ", round(1-conf8[3]/sum(conf8[1,]), digits = 3)) 
paste0("Precision: ", round(conf8[4]/sum(conf8[,2]) , digits = 3)) 
paste0("Recall: ", round(conf8[4]/sum(conf8[2,]), digits = 3)) 

# KNN k = 10
knn100.pred <-  knn(train.knn, test.knn, train.dir, k = 100)
conf9 <- table(knn100.pred, Direction.test)
conf9
paste0("Accuracy: ", round(sum(diag(conf9))/sum(conf9), digits = 3))
paste0("Specificity: ", round(1-conf9[3]/sum(conf9[1,]), digits = 3)) 
paste0("Precision: ", round(conf9[4]/sum(conf9[,2]) , digits = 3)) 
paste0("Recall: ", round(conf9[4]/sum(conf9[2,]), digits = 3)) 
