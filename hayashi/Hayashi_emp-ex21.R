# ===============================================
# Fumio Hayashi - Econometrics
#           Chapter 2
#
# R-Code for Empirical Exercises 2.1
#
# Author: Stefan Bruder and Antonio Giannino
# ===============================================

rm(list = ls());

# Load libraries
library(sandwich); # robust covariance matrix estimators (function: vcovHC)
library(lmtest);   # testing linear regression models (functions: coeftest, bgtest)
library(car);      # companion to applied regression (function: linearHypothesis)


# User input (name and directory of data file)
data.file.name <- "mishkin.asc";
data.file.dir <- _____________ ;  # Set your directory
data.var.names <- c("year","month","PAI1","PAI3","TB1","TB3","CPI");
output.dir <- paste(data.file.dir,"E.1.output/",sep="")



# Import data
data.df <- read.table(file=paste(data.file.dir,data.file.name,sep=""),
              header=F,
              sep="\t", 
              row.names=NULL,
              col.names=data.var.names,
              fill=T);



# Define new variables
n <- nrow(data.df);
data.df$P <- c(NA,data.df$CPI[-n]);
data.df$PAI <- c(NA,(((data.df$CPI[-1]/data.df$CPI[-n])^12)-1)*100);
data.df$r <- data.df$TB1-data.df$PAI;
data.df$dummy.month. <- factor(x=data.df$month,levels=1:12,labels=paste("M",1:12,sep=""));


# Define data sets for January 1953-July 1971 and for post-October 1979 period
data.1953.1.to.1971.7.df <- data.df[(data.df$year+data.df$month/100 >= 1953.01) & (data.df$year+data.df$month/100 <= 1971.07),];
data.post.1979.10.df <- data.df[data.df$year+data.df$month/100 >= 1979.11,];



# Part (d)

# Reproducing Table 2.1
lag.max <- 12;
Ljung.Box.Test <- matrix(data=rep(NA,2*lag.max),ncol=2);
for (l in 1:lag.max)
{
   temp <- Box.test(x=data.1953.1.to.1971.7.df$r,lag=l,type="Ljung-Box");
   Ljung.Box.Test[l,] <- c(temp$statistic,temp$p.value);
}
table.2.1.df <- data.frame(
                   lag=1:lag.max,
                   rho.hat=round(x=as.numeric(acf(x=data.1953.1.to.1971.7.df$r,lag.max=lag.max,type="correlation",plot=F,demean=T)$acf)[-1],digits=3),
                   std.error=round(x=1/sqrt(nrow(data.1953.1.to.1971.7.df)),digits=3),
                   Ljung.Box.Q=round(x=Ljung.Box.Test[,1],digits=1),
                   p.value=paste(100*round(x=Ljung.Box.Test[,2],digits=3),"%",sep=""));


cat("Table 2.1: Real Interest Rates, January 1953-July 1971\n\n");
cat("mean               = ",round(x=mean(data.1953.1.to.1971.7.df$r,na.rm=T),digits=2),"% \n",sep="");
cat("standard deviation = ",round(x=sd(data.1953.1.to.1971.7.df$r,na.rm=T),digits=3),"% \n",sep="");
cat("sample size        = ",nrow(data.1953.1.to.1971.7.df),"\n\n",sep="");
print(table.2.1.df,row.names=F);

# Part (e)

# Reproducing (2.11.9)
fit.e.OLS <- lm(formula=PAI~1+TB1,data=data.1953.1.to.1971.7.df);
fit.e.OLS.summary <- summary(object=fit.e.OLS,correlation=F);
fit.e.OLS.HC0.coef.cov <- vcovHC(x=fit.e.OLS,type="HC0");
fit.e.OLS.HC0.coef.test <- coeftest(fit.e.OLS,vcov=fit.e.OLS.HC0.coef.cov);


# Print output from estimating (2.11.4)
print(fit.e.OLS.summary);

# Heteroskedasticity consistent coefficient tests
print(fit.e.OLS.HC0.coef.test);

# Test hypothesis H0: beta2=1 HA: beta2<>1
fit.e.OLS.lin.hypothesis <- linearHypothesis(fit.e.OLS,hypothesis.matrix="TB1=1",vcov.=fit.e.OLS.HC0.coef.cov);
print(fit.e.OLS.lin.hypothesis);

#Interpretation of (Intercept)
# Under H0: beta2 = 1, the intercept in (2.11.4) is known to be -r, i.e.\nminus the ex-ante real interest rate (which is constant by assumption)


# Plot diagramms for diagnosic checks
opengraphicswindow();
par(oma=c(0,2,4,2));
par(mfrow=c(2,2));
plot(x=fit.e.OLS,which=1:4,sub.caption="");
mtext(text="part (e) - diagrams for diagnostic checks",side=3,line=0,outer=T);

# Part (f)


for (h in 0:3){
   
  fit.f.OLS.HC.coef.cov <- vcovHC(x=fit.e.OLS,type=paste("HC",h,sep=""));
  fit.f.OLS.HC.coef.test <- coeftest(fit.e.OLS,vcov=fit.f.OLS.HC.coef.cov);
  cat("heteroskedasticity consistent coefficient tests (uses HC",h," estimator\nfor covariance matrix of error terms):\n",sep="");
  print(fit.f.OLS.HC.coef.test);
}
   
# Part (g)
# Estimating (2.11.4) under conditional homoskedasticity
fit.g.OLS <- lm(formula=PAI~1+TB1,data=data.1953.1.to.1971.7.df);
fit.g.OLS.summary <- summary(object=fit.g.OLS,correlation=F);
fit.g.OLS.cond.homo.coef.cov <- vcovHC(x=fit.g.OLS,type="const");
fit.g.OLS.cond.homo.coef.test <- coeftest(fit.g.OLS,vcov=fit.g.OLS.cond.homo.coef.cov);
print(fit.g.OLS.summary);

# Coefficient tests (under conditional homoskedasticity)- this yields the same output as from 'fit.g.OLS.summary'
cat("coefficient tests (under conditional homoskedasticity):\n");
print(fit.g.OLS.cond.homo.coef.test);


# Test hypothesis H0: beta2=1 HA: beta2<>1 (under conditional homoskedasticity)
fit.g.OLS.lin.hypothesis <- linearHypothesis(fit.g.OLS,hypothesis.matrix="TB1=1",vcov.=fit.g.OLS.cond.homo.coef.cov);
print(fit.g.OLS.lin.hypothesis);


# Plot diagramms for diagnosic checks
par(oma=c(0,2,4,2));
par(mfrow=c(2,2));
plot(x=fit.g.OLS,which=1:4,sub.caption="");
mtext(text="part (g) - diagrams for diagnostic checks",side=3,line=0,outer=T);



# Part (h)

# Testing for serial correlation in residuals
Breusch.Godfrey.test <- bgtest(formula=fit.g.OLS,order=12,type="Chisq");
print(Breusch.Godfrey.test);




# Part (j)

# Estimating (2.11.4) with seasonal dummies under conditional homoskedasticity
fit.j.OLS <- lm(formula=PAI~-1+dummy.month.+TB1,data=data.1953.1.to.1971.7.df);
fit.j.OLS.summary <- summary(object=fit.j.OLS,correlation=F);
fit.j.OLS.cond.homo.coef.cov <- vcovHC(x=fit.j.OLS,type="const");
print(fit.j.OLS.summary);


# Test hypothesis H0: beta2=1 HA: beta2<>1 (with seasonal dummies under conditional homoskedasticity)
fit.j.OLS.lin.hypothesis <- linearHypothesis(fit.j.OLS,hypothesis.matrix="TB1=1",vcov.=fit.j.OLS.cond.homo.coef.cov);
print(fit.j.OLS.lin.hypothesis);


# Plot diagramms for diagnosic checks
par(oma=c(0,2,4,2));
par(mfrow=c(2,2));
plot(x=fit.j.OLS,which=1:4,sub.caption="");
mtext(text="part (j) - diagrams for diagnostic checks",side=3,line=0,outer=T);


# Part (k)

# Estimating (2.11.4) with PAI1 instead of PAI
fit.k.OLS <- lm(formula=PAI1~1+TB1,data=data.1953.1.to.1971.7.df);
fit.k.OLS.summary <- summary(object=fit.k.OLS,correlation=T);
fit.k.OLS.HC0.coef.cov <- vcovHC(x=fit.k.OLS,type="HC0");
fit.k.OLS.HC0.coef.test <- coeftest(fit.k.OLS,vcov=fit.k.OLS.HC0.coef.cov);
print(fit.k.OLS.summary)
print(fit.k.OLS.HC0.coef.test);

# Test hypothesis H0: beta2=1 HA: beta2<>1 (uses HC0 estimator for covariance matrix of error terms)
fit.k.OLS.lin.hypothesis <- linearHypothesis(fit.k.OLS,hypothesis.matrix="TB1=1",vcov.=fit.k.OLS.HC0.coef.cov);
print(fit.k.OLS.lin.hypothesis);

# Plot diagramms for diagnosic checks
par(oma=c(0,2,4,2));
par(mfrow=c(2,2));
plot(x=fit.k.OLS,which=1:4,sub.caption="");
mtext(text="part (k) - diagrams for diagnostic checks",side=3,line=0,outer=T);



# Part (l)

# Estimating (2.11.4) with PAI1 instead of PAI  for the post-October 1979 period
fit.l.OLS <- lm(formula=PAI1~1+TB1,data=data.post.1979.10.df);
fit.l.OLS.summary <- summary(object=fit.l.OLS,correlation=T);
fit.l.OLS.HC0.coef.cov <- vcovHC(x=fit.l.OLS,type="HC0");
fit.l.OLS.HC0.coef.test <- coeftest(fit.l.OLS,vcov=fit.l.OLS.HC0.coef.cov)
print(fit.l.OLS.summary);
print(fit.l.OLS.HC0.coef.test);


# Test hypothesis H0: beta2=1 HA: beta2<>1 (uses HC0 estimator for covariance matrix of error terms)
fit.l.OLS.lin.hypothesis <- linearHypothesis(fit.l.OLS,hypothesis.matrix="TB1=1",vcov.=fit.l.OLS.HC0.coef.cov);
print(fit.l.OLS.lin.hypothesis);



# Plot diagramms for diagnosic checks
par(oma=c(0,2,4,2));
par(mfrow=c(2,2));
plot(x=fit.l.OLS,which=1:4,sub.caption="");
mtext(text="part (l) - diagrams for diagnostic checks",side=3,line=0,outer=T);
