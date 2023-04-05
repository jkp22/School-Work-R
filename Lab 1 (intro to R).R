# Lab 1: Introduction to R

## SOME BASIC R COMMANDS

# workspace
a=5
a
a*5
a=a+10
a

# scalars, vectors, matrices
b=c(1,2,3)
b
length(b)
c=c(4,5,6)
b+c
ls()
rm(list=ls())
ls()
x=matrix(data=c(1,2,3,4,5,6), nrow=2)
x
dim(x)
x=matrix(c(1,2,3,4,5,6), 2)
x
x=matrix(c(1,2,3,4,5,6), 2, byrow=T)
x

# indexing
x[1,]
x[,1:3]
x[,-1]
x[[1]]
x[[2]]

# data frame
t=data.frame(c1=c(1,2,3), c2=c(4,5,6), c3=c(7,8,9))
t
t[1,]
t[,2]
t[[2]]
t[["c2"]]
t$c2 

# functions
x^2
sqrt(x)
t^2
mean(x)
mean(t)
mean(t$c1)
mean(c(t$c1, t$c2, t$c3))
lapply(t,mean)#Long apply
sapply(t,mean)#Short apply
sapply(t,sd)
summary(t)
summary(x)
summary(c(x))
str(t)
x=rnorm(10)
x
set.seed(1201)
x=rnorm(10)
x
y=rnorm(1000)
mean(y)
sd(y)
y=rnorm(1000, mean=100, sd=10)
mean(y)
sd(y)

# graphics
hist(y)
hist(y, col=2)
hist(y, col=3, breaks=30)
plot(x)
plot(x, type="l")
plot(x, type="b", col=4)
plot(1:20, col=1:20, pch=1:20, cex=2)
x=seq(from=1, to=100, by=1)
x
x=seq(1, 100, 1)
x
err=rnorm(100)*10
err
y=x+err
plot(x,y)
par(mfrow=c(2,2))
plot(x,y)
hist(y)
par(mfcol=c(2,2))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

# loading data
Auto=read.table("Auto.data")
dim(Auto)
View(Auto)
Auto=read.table("Auto.data", header=T)
dim(Auto)
View(Auto)
Auto=read.table("Auto.data", header=T, na.strings="?")
View(Auto)
Auto=read.csv("Auto.csv", header=T, na.strings="?")
View(Auto)
Auto=na.omit(Auto)
dim(Auto)
names(Auto)
str(Auto)
summary(Auto)
plot(Auto[,1:7])
pairs(Auto[,1:7])

plot(Auto$cylinders, Auto$mpg)
attach(Auto)
search()
plot(cylinders, mpg)
cylinders=as.factor(Auto$cylinders)
plot(cylinders, mpg)
plot(Auto$cylinders, mpg)
rm(Auto)
detach(Auto)
library(ISLR2)
attach(Auto)
search()

## Environments

# global environment
a=c(1,2,3)
b="qualitative variable"
ls()

# package environments
search()
ls("package:graphics")
library(ISLR)
search()
ls("package:utils")

# local environments
youthwords=matrix(data=c("OMG","LOL","FOMO","YOLO"),nrow=2)
youthwords
data
nrow

# relational operators
# == equal to
# != not equal to
# > greater than
# < less than
# >= greater than or equal to
# <= less than or equal to
4==4
4==5
4!=5

# logical operators
# & AND
# | OR
T&T
T|F

# making a subset using conditions
names(Auto)
summary(Auto)
Auto.3cyl=subset(Auto, cylinders==3)
dim(Auto.3cyl)
Auto.large=subset(Auto,horsepower>median(horsepower))
dim(Auto.large)
Auto1=subset(Auto, (mpg>=20)&(year>=80))
dim(Auto1)
Auto1=subset(Auto, (mpg>=20)|(year>=80))
dim(Auto1)

# reserved and protected names
# if and else
if=5
IF=5
# for, while, and in
# function
# repeat, break, and next
# TRUE and FALSE
# Inf and -Inf
# NA, NaN, and NULL


## PROGRAMMING: CONDITIONS AND LOOPS

# if statement
a=3
threshold=4
if(a<=threshold){
  a=a^2
}
a

# else statement
if(a<=threshold){
  a=a^2
} else {
  a=sqrt(a)
}
a

# ifelse statement
ifelse(a<=threshold, "T", "F")
a=ifelse(a<=threshold, a^2, sqrt(a))
a

# for loops
for (i in 1:3){
  print(2*i)
}
x=c(0.4,1.1,0.3,1.6)
for (i in x){
  print(i)
}
for (i in 1:length(x)){
  print(x[i])
}

# while loops
a=2
while(a<=10000){
  a=a^2
  print(a)
}
