library(dplyr)
library(boot)
library(ISLR)
library(e1071)
y<-read.csv('D:/RealEstateValuationDataSet.csv')
anyNA(y)
cleandata<-y[complete.cases(y),]
data<-cleandata
print(data)
View(data)
set.seed(101)
rnorm(101)
n = nrow(data)
#train=sample(414,300,replace = FALSE )
#test=sample(414,114, replace = FALSE)
train <-sample(1:nrow(data),0.7*nrow(data))
traind <- data[train,]
testd <- data[-train,]
#lm.fit=lm(data$Y~data$X2,subset =train)
#summary(lm.fit)
#plot(x,y)
#abline(lm.fit)
#data1 = sort(sample(nrow(data), nrow(data)*.7))
#train<-data[data1,]
#test<-data[-data1,]
cor(data$X1,data$Y)
cor(data$X2,data$Y)
cor(data$X3,data$Y)
cor(data$X4,data$Y)
cor(data$X5,data$Y)
cor(data$X6,data$Y)
plot(traind$X2,traind$Y,main="Scatterplot")
lm.fit=lm(Y~X2,data = traind)
summary(lm.fit)
abline(lm.fit)

plot(density(traind$X1))

polygon(density(traind$X1), col="red")
        
lmf <- lm(Y~X2,data=traind)
pred <- predict(lmf,testd)


attach(data)
TestError= mean((Y-predict(lm.fit,data))[-train]^2)
print(TestError)
par(mfrow=c(3,2))
plot(lm.fit)

TestError1= rep(0,100)
TestError2= rep(0,100)
TestError3= rep(0,100)
TestError4= rep(0,100)
TestError5= rep(0,100)

print(TestError1)

for (i in 1:100){
  lm.fit=lm(Y~X2,data=traind)
  TestError1[i]= mean((Y-predict(lm.fit,data))[-train ]^2)
  lm.fit2=lm(Y~poly(X2,2),data=traind) 
  TestError2[i]= mean((Y -predict(lm.fit2,data) )[-train ]^2)
  lm.fit3=lm(Y~poly(X2,3), data=traind)
  TestError3[i]= mean((Y-predict(lm.fit3,data) )[-train ]^2)
  lm.fit4=lm(Y~poly(X2,4), data=traind)
  TestError4[i]= mean((Y-predict(lm.fit4,data) )[-train ]^2)
  lm.fit5=lm(Y~poly(X2,5), data=traind)
  TestError5[i]= mean((Y-predict(lm.fit5,data) )[-train ]^2)
}
TestErr= cbind(TestError1,TestError2,TestError3,TestError4,TestError5)
boxplot(TestErr)

lm.fit2=lm(Y~poly(X2,2), data=traind)
summary(lm.fit2)

glm.fit=glm (Y~X2,data=data)
summary(glm.fit)

lm.fit=lm(Y~X2,data=data)
summary(lm.fit)

glm.fit=glm (Y~X2, data=data)
cv.err1=cv.glm(data,glm.fit)
cv.err1$delta
cv.error =rbind(rep(0,4), rep(0,4))
for (i in 1:4) {
  glm.fit=glm(Y~poly(X2,i),data=data)
  cv.errortmp= cv.glm(data,glm.fit)$delta
  cv.error[1,i]=cv.errortmp[1]
  cv.error[2,i]=cv.errortmp[2]
}
plot(cv.error[1,],type="p")
lines(cv.error[2,])


glm.fit=glm (Y~X2 , data=data)
cv.err1=cv.glm(Auto,glm.fit)
cv.err1$delta

cv.err10= cv.glm(data,glm.fit,K=10)
cv.err10$delta

cv.err20= cv.glm(data,glm.fit,K=20)
cv.err20$delta

cv.err100= cv.glm(data,glm.fit,K=100)
cv.err100$delta

cv.error =rbind(rep (0,4), rep(0,4))
for (i in 1:4) {
  glm.fit=glm(Y~poly(X2,i),data=data)
  cv.errortmp= cv.glm(data,glm.fit,K=10*i)$delta
  cv.error[1,i]=cv.errortmp[1]
  cv.error[2,i]=cv.errortmp[2]
}
plot(cv.error[1,],type="p")
lines(cv.error[2,])
print(cv.error)
print(cv.errortmp)


mod1<-lm(traind$Y~traind$X1+traind$X2+traind$X3+traind$X4+traind$X5+traind$X6)
summary(mod1)
p <- predict(mod1, testd)
summary(p)
print(mod1)
print(p)
print(testd)
plot(mod1)
plot(p)
#plot(testd)
#plot(testd$Y)