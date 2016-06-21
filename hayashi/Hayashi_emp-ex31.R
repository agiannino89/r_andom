# ===============================================
# Fumio Hayashi - Econometrics
#           Chapter 3
#
# R-Code for Empirical Exercises 3.1
#
# Author: Stefan Bruder and Antonio Giannino
# ===============================================

rm(list = ls());

# Import Data
data.file <- "GRILIC.asc" # Set your directory
data.names <- c("RNS","RNS80","MRT","MRT80","SMSA","SMSA80","MED","IQ","KWW","YEAR","AGE","AGE80","S","S80","EXPR","EXPR80","TENURE","TENURE80","LW","LW80");
data.df <- read.table(file=data.file,header=F,sep="\t", row.names=NULL,col.names=data.names,fill=T);


# Load libraries
library(car); # function: vif
library(cba); # function: as.dummy
library(sem); # function: tsls


# Part (a)

# Create Table 
table.a <- data.frame(mean=apply(X=data.df,MARGIN=2,FUN=mean),s.d=apply(X=data.df,MARGIN=2,FUN=sd));
# table.a[c("MRT","MRT80","RNS","RNS80","SMSA","SMSA80"),2] <- NA;
# table.a <- table.a[!substr(x=rownames(table.a),start=1,stop=4)=="YEAR",];
table.a <- table.a[!(rownames(table.a)=="YEAR"),];
table.a <- table.a[sort(rownames(table.a)),];
print(round(x=table.a,digits=2));

# Sample Size 
n <- nrow(data.df)

# Correlation between IQ and S
print(cor(data.df$IQ,data.df$S))


# Part (b)

# Create dummy variables for variable YEAR
year.dummies <- apply(X=as.dummy(x=data.frame(YEAR=as.factor(data.df$YEAR)),sep=".",drop=F),MARGIN=2,FUN=as.integer);
data.df <- cbind(data.df,year.dummies);

# Define h as on p.251
h.names <- c("EXPR","TENURE","RNS","SMSA",colnames(year.dummies));

# Model 1: estimate wage equation by OLS (without regressor IQ)
#Define Regression formula
reg.formula <- as.formula(paste(c("LW~1+S",h.names),sep="+",collapse="+")); "Include constant to obtain the centered R^2"    
#Estimate the model
model.b1.fit <- lm(formula=reg.formula,data=data.df);
model.b1.fit.summary <- summary(model.b.1.fit);
print(summary(model.b1.fit));

# Model 2: estimate wage equation by OLS (including IQ)
reg.formula <- as.formula(paste(c("LW~1+S+IQ",h.names),sep="+",collapse="+")); "Include constant to obtain the centered R^2"
model.b2.fit <- lm(formula=reg.formula,data=data.df);
model.b2.fit.summary <- summary(model.b.2.fit);
print(summary(model.b2.fit));

# Model 3: estimate wage equation by 2SLS (endogenous variable: IQ)
reg.names <- c("S","IQ",h.names);
instr.names  <- c("S","MED","KWW","MRT","AGE",h.names);
reg.formula <- as.formula(paste(c("LW~-1",reg.names),sep="+",collapse="+"));
instr.formula <- as.formula(paste(c("~-1",instr.names),sep="+",collapse="+"));
model.b3.fit <- tsls(formula=reg.formula,instruments=instr.formula,data=data.df);
print(summary(model.b3.fit));


# Part (c)

# Test of overidentifying restrictions (Sargan's statistic)
X.model.b3 <- as.matrix(data.df[,instr.names]);
sargan <- n*t(model.b3.fit$residuals)%*%X.model.b3%*%solve(t(X.model.b3)%*%X.model.b3)%*%t(X.model.b3)%*%model.b3.fit$residuals/(sum(model.b3.fit$residuals^2))
print(sargan) #identical to Hayashi
df <- length(instr.names)-length(reg.names);
p.value.b3 <- pchisq(q=sargan,df=df,lower.tail=F);
print(p.value.b3)


# Part (d)

# Model b3: Estimate wage equation by 2SLS (endogenous variables: IQ)
reg.names <- c("S","IQ",h.names);
instr.names  <- c("S","MED","KWW","MRT","AGE",h.names);
reg.formula <- as.formula(paste(c("LW~-1",reg.names),sep="+",collapse="+"));
# Create data matrices
X <- as.matrix(data.df[,instr.names]);
Z <- as.matrix(data.df[,reg.names]);
Z.hat <- X%*%solve(t(X)%*%X)%*%t(X)%*%Z;
model.b3.2sls.fit <- lm(formula=reg.formula,data=data.frame(Z.hat,LW=data.df$LW));
print(summary(model.b3.2sls.fit));

# Comparing of standard errors
se <- data.frame(sqrt(diag(vcov(model.b3.fit))),sqrt(diag(vcov(model.b3.2sls.fit))));
names(se) <- c("2SLS","Two regressions");
print(se);


# Part (e)

# Model e: estimate wage equation by 2SLS (endogenous variables: IQ, S)
reg.names <- c("S","IQ",h.names);
instr.names  <- c("MED","KWW","MRT","AGE",h.names);
reg.formula <- as.formula(paste(c("LW~-1",reg.names),sep="+",collapse="+"));
instr.formula <- as.formula(paste(c("~-1",instr.names),sep="+",collapse="+"));
model.e.fit <- tsls(formula=reg.formula,instruments=instr.formula,data=data.df);
print(summary(model.e.fit));

# Test of overidentifying restrictions 
X.model.e <- as.matrix(data.df[,instr.names]);
sargan.model.e <- n*t(model.e.fit$residuals)%*%X.model.e%*%solve(t(X.model.e)%*%X.model.e)%*%t(X.model.e)%*%model.e.fit$residuals/(sum(model.e.fit$residuals^2))
print(sargan.model.e)
df <- length(instr.names)-length(reg.names);
p.value <- pchisq(q=sargan.stat.model.e,df=df,lower.tail=F);
print(p.value)


# Part (f)

dep.name <- "LW";
reg.names <- c("S","IQ",h.names);

# Model f.1: estimate wage equation by GMM (endogenous variables: IQ)
instr.names  <- c("S","MED","KWW","MRT","AGE",h.names);
reg.formula <- as.formula(paste(c("LW~-1",reg.names),sep="+",collapse="+"));
instr.formula <- as.formula(paste(c("~-1",instr.names),sep="+",collapse="+"));
#Estimate the wage equation by GMM using the lmgmm-function written by Marc Sommer
source(file="~/Econometrics for Research Students/lm.egmm.R");
model.f.1.fit <- lm.egmm(arg.reg.formula=reg.formula,arg.instr.formula=instr.formula,arg.data=data.df,arg.W.hat.1.type="2SLS");
print(summary(model.f.1.fit));

# Comparing standard errors
delta.se.df <- data.frame(model.f.1.fit$Ase,sqrt(diag(vcov(model.b3.fit))));
names(delta.se.df) <- c("GMM","2SLS")
print(round(x=delta.se.df,digits=5));

# Model f.2: estimate wage equation by GMM (endogenous variables: IQ, S; uses W.hat from model.f.1)
instr.names  <- c("MED","KWW","MRT","AGE",h.names);
reg.formula <- as.formula(paste(c("LW~-1",reg.names),sep="+",collapse="+"));
instr.formula <- as.formula(paste(c("~-1",instr.names),sep="+",collapse="+"));
W.hat.1 <- model.f.1.fit$W.hat.1[instr.names,instr.names];
model.f.2.fit <- lm.egmm(arg.reg.formula=reg.formula,arg.instr.formula=instr.formula,arg.data=data.df,arg.W.hat.1=W.hat.1);
print(summary(model.f.2.fit));

# Comparing standard errors
delta.se.df <- data.frame(model.f.2.fit$Ase,sqrt(diag(vcov(model.e.fit))));
names(delta.se.df) <- c("GMM","2SLS")
print(round(x=delta.se.df,digits=5));

# Model f.2: estimate wage equation by GMM (endogenous variables: IQ, S; reproduces line 5 of Table 3.3)
model.f.3.fit <- lm.egmm(arg.reg.formula=reg.formula,arg.instr.formula=instr.formula,arg.data=data.df,arg.W.hat.1.type="2SLS");
print(summary(model.f.3.fit));

# Test whether schooling is predetermined
C <- model.f.1.fit$J.hat-model.f.2.fit$J.hat;
C.p.value <- pchisq(q=C,df=1,lower.tail=F);


# Comparing of standard errors
delta.se.df <- data.frame(model.f.2.fit$Ase,sqrt(diag(vcov(model.e.fit))));
names(delta.se.df) <- c("GMM","2SLS")
print(round(x=delta.se.df,digits=5));



# Part (g)

# Model g: estimate wage equation by 2SLS (endogenous variables: IQ, S)
reg.names <- c("S","IQ",h.names[-length(h.names)]);
reg.formula <- as.formula(paste(c("LW~+1",reg.names),sep="+",collapse="+"));
instr.names  <- c("MRT","AGE",h.names[-length(h.names)]);
instr.formula <- as.formula(paste(c("~+1",instr.names),sep="+",collapse="+"));
model.g.fit <- tsls(formula=reg.formula,instruments=instr.formula,data=data.df);
print(summary(model.g.fit));

# Model g: first-stage regressions in 2SLS
reg.formula <- as.formula(paste(c("S~1",instr.names[-length(instr.names)]),sep="+",collapse="+"));
model.g.1st.S <- lm(formula=reg.formula,data=data.df);
reg.formula <- as.formula(paste(c("IQ~1",instr.names[-length(instr.names)]),sep="+",collapse="+"));
model.g.1st.IQ <- lm(formula=reg.formula,data=data.df);
print(formula(model.g.1st.S));
print(summary(model.g.1st.S));
print(formula(model.g.1st.IQ));
print(summary(model.g.1st.IQ));

# Model g: estimate wage equation by 2SLS (by hand) (endogenous variables: IQ, S)
reg.names <- c("S","IQ",h.names[-length(h.names)]);
instr.names  <- c("MRT","AGE",h.names[-length(h.names)]);
reg.formula <- as.formula(paste(c("LW~+1",reg.names),sep="+",collapse="+"));
X.model.g <- as.matrix(data.frame("Constant"=rep(1,n),data.df[,instr.names]));
Z.model.g <- as.matrix(data.frame("Constant"=rep(1,n),data.df[,reg.names]));
Z.hat.model.g <- X.model.g%*%solve(t(X.model.g)%*%X.model.g)%*%t(X.model.g)%*%Z.model.g; # see formula (3.8.13) in Hayashi's book)
model.g.bh.fit <- lm(formula=reg.formula,data=data.frame(Z.hat.model.g,LW=data.df$LW));
print(summary(model.g.bh.fit));

# Variance Inflation Factors
VIF.1st=vif(model.g.1st.S)
VIF.g=vif(model.g.bh.fit)        
print(VIF.1st)
print(VIF.g)

