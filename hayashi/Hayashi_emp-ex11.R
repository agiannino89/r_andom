# Fumio Hayashi - Econometrics
#           Chapter 1
#
# R-Code for Empirical Exercises 1
#
# Author: Stefan Bruder and Antonio Giannino

rm(list = ls());

# Import Nerlove's data: set the right path!
data.df <- read.table("NERLOVE.ASC", quote="\"");
names(data.df) <- c("TC","Q","PL","PF","PK");


# Part (b)

# Estimate unrestricted model (1.7.4) by OLS
fit.lm.b <- lm(formula=log(TC)~1+log(Q)+log(PL)+log(PK)+log(PF),data=data.df);
print(summary(object=fit.lm.b),digits=4)

# draw diagnostic plots
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.b,which=1:4);
mtext(text="Diagnostic plots for the regression of part (b)",cex=1.5,side=3,line=2,outer=T);


# Part (c)

# Estimate restricted model (1.7.6) by OLS
fit.lm.c <- lm(formula=log(TC/PF)~1+log(Q)+log(PL/PF)+log(PK/PF),data=data.df);
print(summary(object=fit.lm.c),digits=4)


# Draw diagnostic plots
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.c,which=1:4);
mtext(text="Diagnostic plots for the regression of part (c)",cex=1.5,side=3,line=2,outer=T);


# Part (d)

for (j in 0:4){

   # Define subsample
   subsample <- rep(F,nrow(data.df));
   subsample[(1+29*j):(29+29*j)] <- rep(T,29);

   # Estimate model 1
   fit.lm.d <- lm(formula=log(TC/PF)~1+log(Q)+log(PL/PF)+log(PK/PF),data=data.df,subset=subsample );
   assign(paste("fit.lm.d.",j+1,sep=""),fit.lm.d);

   # Write output of regression analysis to file
   print(summary(object=fit.lm.d),digits=4)
  

   # Draw diagnostic plots
   par(mfrow=c(2,2));
   par(oma=c(0,2,4,2));
   plot(x=fit.lm.d,which=1:4);
   mtext(text=paste("Diagnostic plots for the regression of part (d), group ",j+1,sep=""),cex=1.5,side=3,line=2,outer=T);
   
}



# Part (e)

# Create dummy variables for groups
for (j in 0:4)
{
   temp <- rep(0,nrow(data.df));
   temp[(1+29*j):(29+29*j)] <- rep(1,29);
   assign(paste("D",j+1,sep=""),temp);
}

# Estimate model 2
fit.lm.e <- lm(formula=log(TC/PF)~-1+D1+I(D1*log(Q))+I(D1*log(PL/PF))+I(D1*log(PK/PF))+D2+I(D2*log(Q))+I(D2*log(PL/PF))+I(D2*log(PK/PF))+D3+I(D3*log(Q))+I(D3*log(PL/PF))+I(D3*log(PK/PF))+D4+I(D4*log(Q))+I(D4*log(PL/PF))+I(D4*log(PK/PF))+D5+I(D5*log(Q))+I(D5*log(PL/PF))+I(D5*log(PK/PF)),data=data.df);
print(summary(object=fit.lm.e),digits=4)

SSRsum <- 0
for (j in 0:4)
{
   temp <- get(paste("fit.lm.d.",j+1,sep=""));
   SSR <- sum(residuals(temp)^2);
   cat("SSR form Part (d), Group",j+1,":       ",round(x=SSR,digits=4),"\n");
   SSRsum <- SSRsum + SSR;
}
cat("SSR form Part (d), Group 1 to 5 : ",round(x=SSRsum,digits=4),"\n");
cat("SSR form Part (e) :               ",round(x=sum(residuals(fit.lm.e)^2),digits=4),"\n\n");


# Draw diagnostic plots
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.e,which=1:4);
mtext(text="Diagnostic plots for the regression of part (e)",cex=1.5,side=3,line=2,outer=T);




# Part (f)

# Chow test
K <- 20;
n <- nrow(data.df);
no.restrictions <- 16;
SSR.R <- sum(residuals(fit.lm.c)^2);
SSR.U <- sum(residuals(fit.lm.e)^2);
F.ratio <- (SSR.R-SSR.U)/no.restrictions*(n-K)/SSR.U;
p.value <- pf(q=F.ratio,df1=no.restrictions,df2=n-K,lower.tail=F);

cat("F-Test (Unrestricted Model -> Par (e), Restricted Model -> Part (c))\n\n");
cat("F-statistic = ",signif(x=F.ratio,digits=3),"\n");
cat("p-value     = ",signif(x=p.value,digits=4),"\n\n");



# Part (g)

# Estimate model 3
fit.lm.g <- lm(formula=log(TC/PF)~-1+D1+I(D1*log(Q))+D2+I(D2*log(Q))+D3+I(D3*log(Q))+D4+I(D4*log(Q))+D5+I(D5*log(Q))+log(PL/PF)+log(PK/PF),data=data.df);

# Chow test
K <- 20;
n <- nrow(data.df);
no.restrictions <- 8;
SSR.R <- sum(residuals(fit.lm.g)^2);
SSR.U <- sum(residuals(fit.lm.e)^2);
F.ratio <- (SSR.R-SSR.U)/no.restrictions*(n-K)/SSR.U;
p.value <- pf(q=F.ratio,df1=no.restrictions,df2=n-K,lower.tail=F);


print(summary(object=fit.lm.g),digits=4)
cat("F-Test (Unrestricted Model -> Par (e), Restricted Model -> Part (g))\n\n");
cat("F-statistic = ",signif(x=F.ratio,digits=3),"\n");
cat("p-value     = ",signif(x=p.value,digits=4),"\n\n");

# Draw diagnostic plots

par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.g,which=1:4);
mtext(text="Diagnostic plots for the regression of part (g)",cex=1.5,side=3,line=2,outer=T);




# Part (h)

# Estimate model 4 by WLS
w <- 1/(0.0565 + 2.1377/data.df$Q);
fit.lm.h <- lm(formula=log(TC/PF)~1+log(Q)+I(log(Q)^2)+log(PL/PF)+log(PK/PF),data=data.df,weights=w);

# Write output of regression analysis to file
cat(delimiter.star);
cat("Software Output for Part (h)\n\n");
print(summary(object=fit.lm.h),digits=4)

# Draw diagnostic plots
opengraphicswindow();
par(mfrow=c(2,2));
par(oma=c(0,2,4,2));
plot(x=fit.lm.h,which=1:4);
mtext(text="diagnostic plots for the regressoin of part (h)",cex=1.5,side=3,line=2,outer=T);
saveimage(paste(output.dir,"HW.03.E.1 - diagnostic plots for part (h).",graph.export.type,sep=""),type=graph.export.type);



# Close connection to write output to file and display file
sink();
file.show(output.file,title=paste("Hayashi_emp-ex11 - software output",".txt",sep=""));

