# ===============================================
# Fumio Hayashi - Econometrics
#           Chapter 2
#
# R-Code for Empirical Exercises 2.2
#
# Author: Stefan Bruder and Antonio Giannino
# ===============================================

rm(list = ls());


# User input
data.file.name <- "NERLOVE.ASC";
data.file.dir <- _____________ ;  # Set your directory


# Load library
library(lmtest);   # testing linear regression models (functions: coeftest, bgtest)
library(sandwich); # robust covariance matrix estimators (function: vcovHC)


# Import Nerlove's data
data.df <- read.table(file=paste(data.file.dir,data.file.name,sep=""));
names(data.df) <- c("TC","Q","PL","PF","PK");



# Part (h)

# Estimate model 4 by OLS
fit.lm.h <- lm(formula=log(TC/PF)~1+log(Q)+I(log(Q)^2)+log(PL/PF)+log(PK/PF),data=data.df);
print(summary(object=fit.lm.h),digits=4)

# Draw diagnostic plots
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.h,which=1:4);
mtext(text="diagnostic plots for the regressoin of part (h)",cex=1.5,side=3,line=2,outer=T);




# Part (i)

# estimate auxiliary regression by OLS
e.squared <- residuals(fit.lm.h)^2;
fit.white.test <- lm(formula=e.squared~1+log(Q)+I(log(Q)^2)+I(log(Q)^3)+I(log(Q)^4)+log(PL/PF)+I(log(PL/PF)^2)+log(PK/PF)+I(log(PK/PF)^2)+I(log(Q)*log(PL/PF))+I(log(Q)^2*log(PL/PF))+I(log(Q)*log(PK/PF))+I(log(Q)^2*log(PK/PF))+I(log(PL/PF)*log(PK/PF)),data=data.df);
print(summary(object=fit.white.test),digits=4)

# White's test for conditional heteroskedasticity
white.test.statistic <- nrow(data.df)*summary(fit.white.test)$r.squared;
white.test.p.value <- pchisq(q=white.test.statistic,df=13,lower.tail=F);
print(white.test.statistic);
print(white.test.p.value);


# Draw diagnostic plots
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.white.test,which=1:4);
mtext(text="diagnostic plots for the regressoin of part (i)",cex=1.5,side=3,line=2,outer=T);


# Part (j)

# Step 1: estimate model 4 by OLS
fit.lm.j.step.1 <- lm(formula=log(TC/PF)~1+log(Q)+I(log(Q)^2)+log(PL/PF)+log(PK/PF),data=data.df);

# Step 2: estimate auxiliary model by OLS
e.squared <- residuals(fit.lm.j.step.1)^2;
fit.lm.j.step.2 <- lm(formula=e.squared~1+I(1/Q),data=data.df);

# Step 3: estimate model 4 by WLS
w <- 1/(coefficients(fit.lm.j.step.2)[1] + coefficients(fit.lm.j.step.2)[2]/data.df$Q);
fit.lm.h <- lm(formula=log(TC/PF)~1+log(Q)+I(log(Q)^2)+log(PL/PF)+log(PK/PF),data=data.df,weights=w);

print(summary(object=fit.lm.j.step.2),digits=4)



# Part (k)

# Calculate HC3 standard errors
fit.lm.h.HC.coef.cov <- vcovHC(x=fit.lm.h,type="HC3");
fit.lm.h.HC.coef.test <- coeftest(fit.lm.h,vcov=fit.lm.h.HC.coef.cov);
print(fit.lm.h.HC.coef.test);

