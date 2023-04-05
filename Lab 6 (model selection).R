library(ISLR2)
attach(Credit)
names(Credit)
dim(Credit)

## 6-1 SUBSET SELECTION METHODS

# Best subset selection
library(leaps)
all.fit=regsubsets(Balance~.,data=Credit,nvmax=11)
all.sum=summary(all.fit)
names(all.sum)
all.sum$outmat
par(mfrow=c(1,2))
plot(all.sum$rss,type="b") #each dot shows the best combination predictors based on what ever number of predictors is on the x-axis
plot(all.sum$rsq,type="b")

par(mfrow=c(1,3))
plot(all.sum$cp,type="b")
mincp=which.min(all.sum$cp)
points(mincp,all.sum$cp[mincp],pch=4,cex=2,col=2)

plot(all.sum$bic,type="b")
minbic=which.min(all.sum$bic)
points(minbic,all.sum$bic[minbic],pch=4,cex=2,col=2)

plot(all.sum$adjr2,type="b")
maxadjr2=which.max(all.sum$adjr2)
points(maxadjr2,all.sum$adjr2[maxadjr2],pch=4,cex=2,col=2)
all.sum
par(mfrow=c(1,1))

# Forward stepwise selection
fwd.fit=regsubsets(Balance~.,Credit,nvmax=11, method="forward")
fwd.sum=summary(fwd.fit)
fwd.sum$outmat
fwd.sum$cp

# Backward stepwise selection
bwd.fit=regsubsets(Balance~.,data=Credit,nvmax=11, method="backward")
bwd.sum=summary(bwd.fit)
bwd.sum$outmat
bwd.sum$cp

# (optional) Using "bestglm" to perform subset selection based on CV
library(bestglm)
x=model.matrix(Balance~., Credit)[,-1]
xy=data.frame(x,Balance)
all.CV.bg=bestglm(xy,family=gaussian, IC="CV",CVArgs=list(Method="HTF", K=10, REP=1),method="forward")
all.CVd.bg=bestglm(xy,family=gaussian, IC="CV",method="forward")
all.LOOCV.bg=bestglm(xy,family=gaussian, IC="LOOCV",method="forward")
                  
## 6-2 SHRINKAGE (REGULARIZATION) METHODS

# glmnet() single run with a given lambda
library(glmnet)
x=model.matrix(Balance~., Credit)[,-1]
y=Balance

# ridge regression
ridge.mod=glmnet(x, y, alpha=0, lambda=100)
coef(ridge.mod)

# LASSO
lasso.mod=glmnet(x, y, alpha=1, lambda=100)
coef(lasso.mod)

# specify a grid of values for lambda
grid=exp(1)^seq(10,-5,length=100)

# Ridge Regression: use CV to find the best lambda (default: nfolds=10)
set.seed(20)
cv.out=cv.glmnet(x,y,lambda=grid,alpha=0)
plot(cv.out)
names(cv.out)
bestlam.r=cv.out$lambda.min
bestlam.r
min.r=which.min(cv.out$cvm)
cv.out$cvm[min.r]

ridge.mod=glmnet(x,y,lambda=grid,alpha=0)
ridge.c=predict(ridge.mod,s=bestlam.r,type="coefficients")
ridge.c

# LASSO: use CV to find the best lambda
set.seed(20)
cv.out=cv.glmnet(x,y,lambda=grid,alpha=1)
plot(cv.out)
bestlam.l=cv.out$lambda.min
bestlam.l
min.l=which.min(cv.out$cvm)
cv.out$cvm[min.l]

names(cv.out)
lasso.mod=glmnet(x,y,lambda=grid,alpha=1)
lasso.c=predict(lasso.mod,s=bestlam.l,type="coefficients")
lasso.c

# Comparison with Linear Regression Models
library(boot)
all.glm=glm(Balance~.,data=Credit)
summary(all.glm)
cv.err=cv.glm(Credit, all.glm, K=10)
cv.err$delta
all.c=coef(all.glm)

# Best Subset Selection
best.glm=glm(Balance~Income+Limit+Rating+Cards+Age+Student)
summary(best.glm)
cv.err2=cv.glm(Credit, best.glm, K=10)
cv.err2$delta
best.coef=coef(best.glm)
best.c=rep(0,12)
# assigning the first six coefficients including intercept
for (i in 1:6){best.c[i]=best.coef[i]}
# assigning the coefficient of "StudentYes"
best.c[9]=best.coef[7]

# coefficient estimates
coef.est=cbind(ridge.c,lasso.c,best.c,all.c)
coef.est
colnames(coef.est)=c("Ridge","LASSO","Best Subset",'Linear Reg')
round(coef.est, digits=2)

## 6-3 DIMENSION REDUCTION METHODS, PCR and PLS

# principal compoenents regression
library(pls)
set.seed(3)
pcr.fit=pcr(Balance~., data=Credit, scale=T, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSE", legendpos="topright")
plot(pcr.fit, plottype="validation", val.type="MSE", legendpos="topright")
plot(pcr.fit, plottype="loadings", comps=1:2, legendpos="bottomleft")
pcr.fit$loadings

# partial least squares
set.seed(3)
pls.fit=plsr(Balance~., data=Credit, scale=T, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP", legendpos="topright")
plot(pls.fit, plottype="validation", val.type="MSEP", legendpos="topright")
plot(pls.fit, plottype="loadings", comps=1:4, legendpos="bottomleft")
pls.fit$loadings

