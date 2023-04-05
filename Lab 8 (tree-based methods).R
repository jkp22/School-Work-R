## packages
library(tree)
library(randomForest)
library(gbm)
library(bartMachine)
library(ISLR2)

## 8-1 CLASSIFICATION AND REGRESSION TREES

## data & plots
# Hitters
Hitters=na.omit(Hitters)
attach(Hitters)
names(Hitters)
table(cut(log(Salary),5))
plot(Years,Hits,col=c("blue","green","yellow","orange","red")[as.numeric(cut(log(Salary),5))],pch=16)
hist(Salary, col=2, breaks=20)
hist(log(Salary), col=3, breaks=20)

## regression tree
# building a tree
set.seed(12)
train=sample(nrow(Hitters),nrow(Hitters)/2)
hitters.tree=tree(log(Salary)~.,data=Hitters[train,-(8:13)])
plot(hitters.tree)
text(hitters.tree, cex=0.7)
hitters.tree

# tree pruning using cross validation
set.seed(11)
cvout=cv.tree(hitters.tree)
names(cvout)
cvout
plot(cvout$size,cvout$dev,type="b")
mindev=which.min(cvout$dev)
minsize=cvout$size[mindev]
points(minsize,cvout$dev[mindev],pch=4,cex=2,col=2)
hitters.pruned=prune.tree(hitters.tree,best=minsize)
plot(hitters.pruned)
text(hitters.pruned)
hitters.pruned

# prediction based on a pruned tree
log.sal.pred=predict(hitters.pruned,Hitters[-train,])
mse=mean((log(Salary[-train])-log.sal.pred)^2)
rse=sqrt(mse)
rse.converted=exp(rse)
rse.converted
plot(log.sal.pred,log(Salary[-train]))

## classification trees
# data - Heart
Heart=read.csv("Heart.csv")
Heart=Heart[,-1]
Heart=na.omit(Heart)
summary(Heart)
Heart$AHD=as.factor(Heart$AHD)
Heart$ChestPain=as.factor(Heart$ChestPain)
Heart$Thal=as.factor(Heart$Thal)

library(dplyr)
Heart=mutate_if(Heart, is.character, as.factor)
summary(Heart)
attach(Heart)

# building a tree
set.seed(11)
train2=sample(nrow(Heart),nrow(Heart)/2)
heart.tree=tree(AHD~.,data=Heart[train2,])
plot(heart.tree)
text(heart.tree,cex=0.7)
heart.tree

# tree pruning using cross validation
set.seed(9)
cvout=cv.tree(heart.tree,FUN=prune.misclass)
names(cvout)
cvout
plot(cvout$size,cvout$dev,type="b",cex.lab=1.5,cex.axis=1.5)
mindev=which.min(cvout$dev)
minsize=cvout$size[mindev]
points(minsize,cvout$dev[mindev],pch=4,col=2,cex=2)
heart.pruned=prune.misclass(heart.tree,best=minsize)
heart.pruned=prune.tree(heart.tree,method="misclass", best=minsize)
plot(heart.pruned)
text(heart.pruned)
heart.pruned

# prediction based on pruned tree
ahd.pred=predict(heart.pruned,Heart[-train2,],type="class")
table(ahd.pred,AHD[-train2])
sum(ahd.pred!=AHD[-train2])/length(AHD[-train2])

# prediction of posterior probabilities and construction of roc curve
ahd.prob=predict(heart.pruned,Heart[-train2,])

library(ROCR)
rocplot=function(pred, truth, ...){
  predob=prediction(pred, truth)
  perf=performance(predob, "tpr", "fpr")
  plot(perf,...)}

rocplot(ahd.prob[,2],AHD[-train2], col="blue")
legend(x=0.65, y=0.1,legend="tree",col="blue",lwd=1, bty="n")

auc=function(pred, truth){
  predob=prediction(pred, truth)
  perf=performance(predob, "auc")
  return(perf@y.values[[1]])}
auc.tree=auc(ahd.prob[,2],AHD[-train2])
legend(x=0.85, y=0.1,legend=round(auc.tree,4),bty="n")

## 8-2 BAGGING, RANDOM FORESTS, BOOSTING, AND BART

## bagging
set.seed(1)
heart.bag=randomForest(AHD~.,data=Heart,mtry=13,ntree=3000,importance=T)
heart.bag
varImpPlot(heart.bag)
rocplot(-heart.bag$votes[1:nrow(Heart)],AHD, col="blue")
rocplot(heart.bag$votes[,2],AHD, col="red")
legend(x=0.65, y=0.18,legend="bagging",col="red",lwd=1, bty="n")
auc.bag=auc(-heart.bag$votes[1:nrow(Heart)],AHD)
auc.bag=auc(heart.bag$votes[,2],AHD)
legend(x=0.85, y=0.18,legend=round(auc.bag,4),bty="n")

## random forests
set.seed(1)
heart.rf=randomForest(AHD~.,data=Heart,mtry=4,ntree=3000,importance=T)
heart.rf
varImpPlot(heart.rf)
rocplot(-heart.rf$votes[1:nrow(Heart)],AHD, col="green")
rocplot(heart.rf$votes[,2],AHD, col="green")
legend(x=0.65, y=0.26,legend="randomF",col="green",lwd=1, bty="n")
auc.rf=auc(-heart.rf$votes[1:nrow(Heart)],AHD)
auc.rf=auc(heart.rf$votes[,2],AHD)
legend(x=0.85, y=0.26,legend=round(auc.rf,4),bty="n")

## boosting - regression
set.seed(100)
hitters.gbm=gbm(log(Salary)~.,Hitters,distribution="gaussian",n.trees=3000,interaction.depth=1,shrinkage=0.01,cv.fold=10)
names(hitters.gbm)
plot(hitters.gbm$cv.error)
plot(hitters.gbm$cv.error[500:3000])
min.tree=which.min(hitters.gbm$cv.error)
min.tree
hitters.gbm$cv.error[min.tree]

set.seed(100)
hitters.gbm2=gbm(log(Salary)~.,Hitters,distribution="gaussian",n.trees=30000,interaction.depth=1,shrinkage=0.001,cv.fold=10)
plot(hitters.gbm2$cv.error)
plot(hitters.gbm2$cv.error[5000:30000])
min.tree2=which.min(hitters.gbm2$cv.error)
min.tree2
hitters.gbm2$cv.error[min.tree2]

par(ps=11)
summary(hitters.gbm,n.trees=min.tree,las=1)
par(ps=12)

plot(hitters.gbm, n.trees=min.tree, i="CAtBat")
plot(hitters.gbm, n.trees=min.tree, i="CRuns")
plot(hitters.gbm, n.trees=min.tree, i="CRBI")
plot(hitters.gbm, n.trees=min.tree, i="Walks")
plot(hitters.gbm, n.trees=min.tree, i="Hits")
plot(hitters.gbm, n.trees=min.tree, i="Years")
plot(hitters.gbm, n.trees=min.tree, i="CHits")

## boosting - classification
AHD01=ifelse(AHD=="Yes",1,0)
set.seed(1)
heart.gbm=gbm(AHD01~.-AHD,data=Heart,distribution="bernoulli",n.trees=2000,interaction.depth=1,shrinkage=0.01,cv.fold=10)
plot(heart.gbm$cv.error)
min.tree3=which.min(heart.gbm$cv.error)
min.tree3
points(min.tree3,heart.gbm$cv.error[min.tree3],pch=8,col=2,cex=3)
heart.gbm$cv.error[min.tree3]
par(ps=10)
summary(heart.gbm, n.trees=min.tree3,las=1)
par(ps=12)
plot(heart.gbm, n.trees=min.tree3, i="Thal")
plot(heart.gbm, n.trees=min.tree3, i="Ca")
plot(heart.gbm, n.trees=min.tree3, i="ChestPain")
plot(heart.gbm, n.trees=min.tree3, i="Age")

ahd.pred=ifelse(heart.gbm$cv.fitted>0,1,0)
table(AHD01,ahd.pred)
sum(ahd.pred!=AHD01)/nrow(Heart)
rocplot(heart.gbm$cv.fitted,AHD, col="blue")
legend(x=0.65, y=0.34,legend="boosting",col="blue",lwd=1, bty="n")
auc.boosting.cv=auc(heart.gbm$cv.fitted,AHD)
legend(x=0.85, y=0.34,legend=round(auc.boosting.cv,4),bty="n")

# BART: bayesian additive regression trees - regression using bartMachine
set.seed(100)
hitters.bart=bartMachine(Hitters[,-19], log(Salary), num_trees=50, num_burn_in=250, num_iterations_after_burn_in=1000)
summary(hitters.bart)
yhat.bart=predict(hitters.bart, Hitters[,-19])
mean((log(Salary)-yhat.bart)^2)
hitters.bart$rmse_train
hitters.bart$rmse_train^2

set.seed(100)
hitters.bart.cv= k_fold_cv(Hitters[,-19], log(Salary), k_folds=10, num_trees=50, num_burn_in=250, num_iterations_after_burn_in=1000)
names(hitters.bart.cv)
hitters.bart.cv$rmse
hitters.bart.cv$rmse^2
mean((log(Salary)-hitters.bart.cv$y_hat)^2)

hitters.var.imp=investigate_var_importance(hitters.bart)
hitters.var.imp$avg_var_props
pd_plot(hitters.bart, "Years")
pd_plot(hitters.bart, "CHits")
pd_plot(hitters.bart, "CAtBat")
pd_plot(hitters.bart, "Walks")

## BART - classification using bartMachine
heart.bart=bartMachine(Heart[,-14], AHD)
summary(heart.bart)

set.seed(1)
heart.bart.cv= k_fold_cv(Heart[,-14], AHD, k_folds=10)
names(heart.bart.cv)
heart.bart.cv$confusion_matrix
heart.bart.cv$misclassification_error

heart.var.imp=investigate_var_importance(heart.bart)
heart.var.imp$avg_var_props
pd_plot(heart.bart, "Ca")
pd_plot(heart.bart, "Oldpeak")

rocplot(-heart.bart.cv$phat, AHD, col="brown")
legend(x=0.65, y=0.42,legend="bart",col="brown",lwd=1, bty="n")
auc.bart.cv=auc(-heart.bart.cv$phat,AHD)
legend(x=0.85, y=0.42,legend=round(auc.bart.cv,4),bty="n")

## ROC curves of all five tree-based models
# A single tree
rocplot(ahd.prob[,2],AHD[-train2], col="grey")
legend(x=0.65, y=0.1,legend="tree",col="grey",lwd=1, bty="n")
legend(x=0.85, y=0.1,legend=round(auc.tree,4),bty="n")
# bagging
rocplot(heart.bag$votes[,2],AHD, col="red", add=T)
legend(x=0.65, y=0.18,legend="bagging",col="red",lwd=1, bty="n")
legend(x=0.85, y=0.18,legend=round(auc.bag,4),bty="n")
# random forests
rocplot(heart.rf$votes[,2],AHD, col="green", add=T)
legend(x=0.65, y=0.26,legend="randomF",col="green",lwd=1, bty="n")
legend(x=0.85, y=0.26,legend=round(auc.rf,4),bty="n")
# boosting
rocplot(heart.gbm$cv.fitted,AHD, col="blue", add=T)
legend(x=0.65, y=0.34,legend="boosting",col="blue",lwd=1, bty="n")
legend(x=0.85, y=0.34,legend=round(auc.boosting.cv,4),bty="n")
# bart
rocplot(-heart.bart.cv$phat, AHD, col="brown", add=T)
legend(x=0.65, y=0.42,legend="bart",col="brown",lwd=1, bty="n")
legend(x=0.85, y=0.42,legend=round(auc.bart.cv,4),bty="n")
