# LAB 4 classification: Logistic Regression, LDA, QDA, Naive Bayes, KNN, and Poisson

## 4-1 LOGISTIC REGRESSION

library(ISLR2)
attach(Default)
names(Default)
str(Default)

# linear regression
d01=ifelse(default=="Yes",1,0)
lm.fit=lm(d01~balance)
summary(lm.fit)
plot(balance,d01)
abline(lm.fit,col="blue",lwd=2)

# glm fit with family=binomial
# (cf) for probit regression, use family=binomial(link="probit")
default.fit=glm(default~balance, family=binomial)
summary(default.fit)
predict(default.fit, data.frame(balance=c(1000,2000)), type="response")

default.fit2=glm(default~student, family=binomial)
summary(default.fit2)
predict(default.fit2, data.frame(student=c("Yes","No")), type="response")

default.fit3=glm(default~., data=Default, family=binomial)
summary(default.fit3)

default.fit4=glm(default~.-income, data=Default, family=binomial)
summary(default.fit4)

# prediction and confusion matrix
default.prob=predict(default.fit4, type="response")
default.pred=ifelse(default.prob>0.5,"Yes","No")
table(default.pred, default)
sum(default.pred!=default)/length(default) 

# validation set approach
set.seed(20)
train=sample(nrow(Default),nrow(Default)/2)

default.fit5=glm(default~student+balance, data=Default, subset=train, family=binomial)
summary(default.fit5)
default.fit5=glm(default~student+balance, data=Default[train,], family=binomial)

default.prob5=predict(default.fit5, newdata=Default[-train,], type="response")
default.pred5=ifelse(default.prob5>0.5,"Yes","No")
table(default.pred5, default[-train])
sum(default.pred5!=default[-train])/length(default[-train]) # Overall error rate
sum((default.pred5=="Yes")&(default[-train]=="No"))/sum(default[-train]=="No") # Type I error rate
sum((default.pred5=="No")&(default[-train]=="Yes"))/sum(default[-train]=="Yes") # Type II error rate

default.pred6=ifelse(default.prob5>0.1,"Yes","No")
table(default.pred6, default[-train])
sum(default.pred6!=default[-train])/length(default[-train]) # Overall error rate
sum((default.pred6=="Yes")&(default[-train]=="No"))/sum(default[-train]=="No") # Type I error rate
sum((default.pred6=="No")&(default[-train]=="Yes"))/sum(default[-train]=="Yes") # Type II error rate

# undersampling of non-defaults
index.yes=which(default=="Yes")
index.no=which(default=="No")
set.seed(20)
train.yes=sample(index.yes,length(index.yes)/2)
train.no=sample(index.no,length(index.yes)/2)
train.us=c(train.yes,train.no)

default.fit7=glm(default~student+balance, data=Default, family=binomial, subset=train.us)
summary(default.fit7)

default.prob7=predict(default.fit7, newdata=Default[-train.us,], type="response")
default.pred7=ifelse(default.prob7>0.5,"Yes","No")
table(default.pred7, default[-train.us])
sum(default.pred7!=default[-train.us])/length(default[-train.us]) # Overall error rate
sum((default.pred7=="Yes")&(default[-train.us]=="No"))/sum(default[-train.us]=="No") # Type I error rate
sum((default.pred7=="No")&(default[-train.us]=="Yes"))/sum(default[-train.us]=="Yes") # Type II error rate

# ROC curves
library(ROCR)

# rocplot function
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf,...)}

rocplot(default.prob5, default[-train], col="blue")

# auc function
auc=function(pred, truth){
  predob=prediction(pred, truth)
  perf=performance(predob, "auc")
  return(perf@y.values[[1]])}

auc(default.prob5, default[-train])

## 4-2 GENERATIVE MODELS AND KNN

# LDA
library(MASS)
default.lda=lda(default~student+balance,subset=train)
default.lda
plot(default.lda)

default.pred=predict(default.lda, data.frame(student="Yes", balance=2000))
default.pred
names(default.pred)

default.pred=predict(default.lda, Default[-train,])
table(default.pred$class, default[-train])

default.pc=ifelse(default.pred$posterior[,2]>0.1,"Yes","No")
table(default.pc, default[-train])

rocplot(default.pred$posterior[,2], default[-train], col="red")
legend(x=0.67, y=0.1,legend="LDA",col="red",lwd=1,bty="n")
auc.lda=auc(default.pred$posterior[,2], default[-train])
auc.lda
legend(x=0.85, y=0.1,legend=round(auc.lda,5),bty="n")

# QDA
default.qda=qda(default~student+balance,subset=train)
default.qda

default.qpred=predict(default.qda, Default[-train,])
table(default.qpred$class, default[-train])

default.qpc=ifelse(default.qpred$posterior[,2]>0.1,"Yes","No")
table(default.qpc, default[-train])

# naive bayes
library(e1071)
default.nb=naiveBayes(default~., data=Default, subset=train)
default.nb

nb.class=predict(default.nb, Default[-train,])
table(nb.class, default[-train])
1-sum(nb.class==default[-train])/nrow(Default[-train,])

nb.preds=predict(default.nb, Default[-train,], type="raw")
nb.preds[1:5, ]

rocplot(nb.preds[,2], default[-train], col="brown")
legend(x=0.67, y=0.31,legend="naiveB",col="brown",lwd=1,bty="n")
auc.nb=auc(nb.preds[,2], default[-train])
auc.nb
legend(x=0.85, y=0.31,legend=round(auc.nb,5),bty="n")

# KNN
library(class)
x.train=cbind(student,balance)[train,]
x.test=cbind(student,balance)[-train,]
y.train=default[train]
knn.pred3=knn(x.train,x.test,y.train,k=3)
knn.pred3[1:10]
table(knn.pred3,default[-train])

knn.pred10=knn(x.train,x.test,y.train,k=10, prob=T)
knn.pred10[1:10]
attr(knn.pred10,"prob")[1:10]
table(knn.pred10,default[-train])

rocplot(-attr(knn.pred10,"prob"), default[-train], col="grey")
legend(x=0.67, y=0.38,legend="KNN",col="grey",lwd=1,bty="n")
auc.knn=auc(-attr(knn.pred10,"prob"), default[-train])
auc.knn
legend(x=0.85, y=0.38,legend=round(auc.knn,5),bty="n")

# ROC curves for KNN, LDA, QDA, and logistic
rocplot(default.pred$posterior[,2], default[-train], col="red")
legend(x=0.67, y=0.1,legend="LDA",col="red",lwd=1,bty="n")
auc.lda=auc(default.pred$posterior[,2], default[-train])
auc.lda
legend(x=0.85, y=0.1,legend=round(auc.lda,5),bty="n")

rocplot(default.qpred$posterior[,2], default[-train], col="blue", add=T)
legend(x=0.67, y=0.17,legend="QDA",col="blue",lwd=1,bty="n")
auc.qda=auc(default.qpred$posterior[,2], default[-train])
auc.qda
legend(x=0.85, y=0.17,legend=round(auc.qda,5),bty="n")

rocplot(default.prob5, default[-train], col="green", add=T)
legend(x=0.67, y=0.24,legend="logistic",col="green",lwd=1,bty="n")
auc.log=auc(default.prob5, default[-train])
auc.log
legend(x=0.85, y=0.24,legend=round(auc.log,5),bty="n")

rocplot(nb.preds[,2], default[-train], col="brown", add=T)
legend(x=0.67, y=0.31,legend="naiveB",col="brown",lwd=1,bty="n")
auc.nb=auc(nb.preds[,2], default[-train])
auc.nb
legend(x=0.85, y=0.31,legend=round(auc.nb,5),bty="n")

rocplot(-attr(knn.pred10,"prob"), default[-train], col="grey",add=T)
legend(x=0.67, y=0.38,legend="KNN",col="grey",lwd=1,bty="n")
auc.knn=auc(-attr(knn.pred10,"prob"), default[-train])
auc.knn
legend(x=0.85, y=0.38,legend=round(auc.knn,5),bty="n")

## 4-3 GLM - Poisson Regression

# Bikeshare Data
attach(Bikeshare)
dim(Bikeshare)
summary(Bikeshare)

#linear model
bikers.lm=lm(bikers~mnth+hr+workingday+temp+weathersit, data=Bikeshare)
summary(bikers.lm)

contrasts(Bikeshare$hr)=contr.sum(24)
contrasts(Bikeshare$mnth)=contr.sum(12)
bikers.lm2=lm(bikers~mnth+hr+workingday+temp+weathersit, data=Bikeshare)
summary(bikers.lm2)

sum((predict(bikers.lm)-predict(bikers.lm2))^2)
all.equal(predict(bikers.lm), predict(bikers.lm2))

coef.mo.lm2=c(coef(bikers.lm2)[2:12], -sum(coef(bikers.lm2)[2:12]))
plot(coef.mo.lm2,xlab="Month",ylab="Coefficient",xaxt="n",col="blue",pch=19,type="o")
axis(side=1, at=1:12, labels=c("J", "F", "M", "A","M", "J", "J", "A", "S", "O", "N", "D"))

coef.hr.lm2=c(coef(bikers.lm2)[13:35], -sum(coef(bikers.lm2)[13:35]))
plot(coef.hr.lm2, xlab="Hour", ylab="Coefficient", col="blue", pch=19, type="o")

# poisson model
bikers.pois=glm(bikers~mnth+hr+workingday+temp+weathersit,data=Bikeshare,family=poisson)
summary(bikers.pois)

coef.mo.pois=c(coef(bikers.pois)[2:12], -sum(coef(bikers.pois)[2:12]))
plot(coef.mo.pois, xlab="Month", ylab="Coefficient", xaxt="n", col="blue", pch=19, type="o")
axis(side=1, at=1:12, labels=c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))

coef.hr.pois=c(coef(bikers.pois)[13:35], -sum(coef(bikers.pois)[13:35]))
plot(coef.hr.pois, xlab="Hour", ylab="Coefficient", col="blue", pch=19, type="o")

plot(predict(bikers.lm2), predict(bikers.pois, type="response"))
abline(0, 1, col=2, lwd=3)
