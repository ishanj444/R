#resource:https://rstudio-pubs-static.s3.amazonaws.com/224739_0b86cafbb6d5421ab92c9dc1dd982690.html
##VISIT FOR DATABASE INFO:https://archive.ics.uci.edu/ml/datasets/Auto+MPG
##----------------------------------------------------------------------------------------

##the use of the validation set approach in order to estimate the test error rates that result from fitting various linear models on the Auto data set.

library(ISLR)
library(boot)
library(dplyr)
library(e1071)
##Data set summary
names(Auto)
summary(Auto)
print(Auto)
##To get precisely the same results
set.seed (1)
#---DATA PLOTS--------------------------
y=Auto$mpg
x=Auto$horsepower
plot(x,y)
#--------------------------------------
n=nrow(Auto) # To find number of data records

##to split the set of samples into sample() two halves, by selecting a random subset of 196 observations out of the original 392 observations.
#--Validation set approach for CV
train=sample(392,196)
##o fit a linear regression using only the observations corresponding to the training set.
lm.fit=lm(mpg~horsepower,data=Auto,subset =train)
summary(lm.fit)
plot(x,y)
abline(lm.fit)
#Predict on test data and compute MSE
attach (Auto)
TestError1= mean((mpg-predict(lm.fit,Auto))[-train ]^2)
#-------EVALUATE MODEL------------------------------------------------------
par(mfrow=c(2,2))
plot(lm.fit)
#-------CROSS VALIDATION-------------------------
TestError1= rep(0,100)
TestError2= rep(0,100)
TestError3= rep(0,100)
for (i in 1:100){
  train=sample(392,196)
# Fit a linear regression\First ordet polynomial to the data
  lm.fit=lm(mpg~horsepower,data=Auto,subset =train)
  TestError1[i]= mean((Auto$mpg-predict(lm.fit,Auto))[-train ]^2)
# Fit a second order polynomial to the data
  lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train) 
# Compute the test error
  TestError2[i]= mean((Auto$mpg -predict(lm.fit2,Auto) )[-train ]^2)
# Fit a third order polynomial to the data
  lm.fit3=lm(mpg~poly(horsepower,3), data=Auto,subset=train)
# Compute the test error
  TestError3[i]= mean((Auto$mpg-predict(lm.fit3,Auto) )[-train ]^2)
}
TestError= cbind(TestError1,TestError2,TestError3)
boxplot(TestError)
##Fit the best model to data
lm.fit3=lm(mpg~poly(horsepower,3), data=Auto)
summary(lm.fit3)
#LOOCV

glm.fit=glm (mpg~horsepower,data=Auto)
summary(glm.fit)

lm.fit=lm(mpg~horsepower,data=Auto)
summary(lm.fit)


glm.fit=glm (mpg~horsepower , data=Auto)
cv.err1=cv.glm(Auto,glm.fit)
cv.err1$delta
#To plot LOOCV error
cv.error =rbind(rep (0,4), rep(0,4))
for (i in 1:4) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.errortmp= cv.glm(Auto,glm.fit)$delta
  cv.error[1,i]=cv.errortmp[1]
  cv.error[2,i]=cv.errortmp[2]
}
plot(cv.error[1,],type="l")
lines(cv.error[2,])



# K-fold cross validation
glm.fit=glm (mpg~horsepower , data=Auto)
cv.err1=cv.glm(Auto,glm.fit)
cv.err1$delta

cv.err10= cv.glm(Auto,glm.fit,K=10)
cv.err10$delta

cv.err20= cv.glm(Auto,glm.fit,K=20)
cv.err20$delta



##Use cross-validation to select among polynomial models of order 1 to 4
#Determine the best polynomial fit
cv.error =rbind(rep (0,4), rep(0,4))
for (i in 1:4) {
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.errortmp= cv.glm(Auto,glm.fit,K=10)$delta
  cv.error[1,i]=cv.errortmp[1]
  cv.error[2,i]=cv.errortmp[2]
}
plot(cv.error[1,],type="l")
lines(cv.error[2,])
