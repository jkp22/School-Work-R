library(ISLR2)
library(boot)
library(MASS)
library(class)
library(ROCR)

## REGRESSION
attach(Auto)
dim(Auto)

# validation set approach
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower, subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
mean((mpg[-train]-predict(lm.fit,Auto[-train,]))^2)
lm.fit2=lm(mpg~poly(horsepower,2), subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)

# LOOCV
glm.fit=glm(mpg~horsepower)
cv.err=cv.glm(Auto, glm.fit)
names(cv.err)
cv.err$delta

# using for() loop
cv.error=rep(0,10)
for(i in 1:10){
glm.fit=glm(mpg~poly(horsepower,i))
cv.error[i]=cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

# k-fold CV
set.seed(17)
cv.error=rep(0,10)
for(i in 1:10){
glm.fit=glm(mpg~poly(horsepower,i))
cv.error[i]=cv.glm(Auto, glm.fit, K=5)$delta[1]
}
cv.error


### CLASSIFICATION
attach(Default)
dim(Default)

# k-fold CV for logistic regression
# define the cost function for cv.glm()
costfunction=function(r, pi){
err1=ifelse(r==1 & pi<0.5, 1, 0) 
err0=ifelse(r==0 & pi>0.5, 1, 0)
return(mean(err1+err0))}

# (WARNING) The cv.glm() without K, which is LOOCV, for the Default data takes about 15 minutes.
set.seed(8)
glm.fit=glm(default~student+balance, data=Default, family=binomial)
cv.err=cv.glm(Default, glm.fit, cost=costfunction, K=10)
cv.err$delta

# CV for lda and qda
lda.fit=lda(default~student+balance, CV=T)
names(lda.fit)
table(lda.fit$class, default)
sum(lda.fit$class!=default)/nrow(Default)

qda.fit=qda(default~student+balance, CV=T)
names(qda.fit)
table(qda.fit$class, default)
sum(qda.fit$class!=default)/nrow(Default)

# Cv for knn
x.train=cbind(student,balance)
knn.pred=knn.cv(x.train, default, k=15, prob=T)
table(knn.pred, default)
sum(default!=knn.pred)/nrow(Default)

# finding the best k using for() loop
knn.err=rep(0, 20)
for (i in 1:20){
knn.pred=knn.cv(x.train, default, k=i, prob=T)
knn.err[i]=sum(default!=knn.pred)/nrow(Default)
}
knn.err
min.k=which.min(knn.err)
min.k
knn.pred=knn.cv(x.train, default, k=min.k, prob=T)


### GENERAL APPROACH

# To construct an ROC curve for logistic regression, posterior probabilities are required.
# The cv.glm() function does not provide the posterior probabilities. 
# Below are general approaches to obtain the posterior probabilities of a classification method.  
# This approach can be applied to any classification methods.

# LOOCV

# LOOCV on a sample data set of 500 observations
set.seed(3)
D500=Default[sample(nrow(Default),500),]
dim(D500)
prob=rep(0,500) 
class=rep("No",500)

for (i in 1:500){
  glm.fit=glm(default~., data=D500[-i,], family=binomial)
  prob[i]=predict(glm.fit, D500[i,], type="response")
  if(prob[i]>0.5){class[i]="Yes"}
}
table(class,D500$default)
cv.err=sum(class!=D500$default)/500
cv.err

# LOOCV on the entire data set "Default"
# (WARNING) Total run time of the following for() loop is about 15 minutes.
prob=rep(0,nrow(Default)) 
class=rep("No",nrow(Default))
for (i in 1:nrow(Default)){
  glm.fit=glm(default~., data=Default[-i,], family=binomial)
  prob[i]=predict(glm.fit, Default[i,], type="response")
  if(prob[i]>0.5){class[i]="Yes"}
}
table(class,default)
cv.err=sum(class!=default)/nrow(Default)
cv.err

# k-fold CV

# shuffle the data set randomly
set.seed(10)
Default2=Default[sample(nrow(Default)),]

# determine k and define vectors
k=10
default.prob=rep(0, nrow(Default))
default.pred=rep("No", nrow(Default))

# assign fold numbers to all observations
folds=rep(1:k, length=nrow(Default))

# perform k-fold cv
for(i in 1:k){
  fold=which(folds==i)
  default.fit=glm(default~student+balance, data=Default2[-fold,], family=binomial)
  default.prob[fold]=predict(default.fit, newdata=Default2[fold,], type="response")
  default.pred[fold]=ifelse(default.prob[fold]>0.5,"Yes","No")
}
table(default.pred,Default2$default)
cv.err=sum(Default2$default!=default.pred)/nrow(Default)
cv.err


## ROC CURVES & AUC

# rocplot function
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf,...)}

# ROC curves
rocplot(default.prob, Default2$default, col="blue")
legend(x=0.6, y=0.1,legend="logistic",col="blue",lwd=1,bty="n")

rocplot(lda.fit$posterior[,2], default, col="red", add=T)
legend(x=0.6, y=0.17,legend="LDA",col="red",lwd=1,bty="n")

rocplot(-attr(knn.pred,"prob"), default, col="green", add=T)
legend(x=0.6, y=0.24,legend="KNN",col="green",lwd=1,bty="n")

# auc function
auc=function(pred, truth){
  predob=prediction(pred, truth)
  perf=performance(predob, "auc")
  return(perf@y.values[[1]])}

# AUCs
auc.log=auc(default.prob, Default2$default)
legend(x=0.8, y=0.1,legend=round(auc.log,4),bty="n")

auc.lda=auc(lda.fit$posterior[,2], default)
legend(x=0.8, y=0.17,legend=round(auc.lda,4),bty="n")

auc.knn=auc(-attr(knn.pred,"prob"), default)
legend(x=0.8, y=0.24,legend=round(auc.knn,4),bty="n")


## BOOTSTRAP 

# bootstrap example of estimating standard error (optional)
boot.fn=function(data,index){
  coef(lm(mpg~horsepower,data=data,subset=index))}
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower))$coef
