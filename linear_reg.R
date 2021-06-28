library(ggplot2)
library(dplyr)
library(boot)
library(ISLR)
library(e1071)
library(caTools)
r<-read.csv('C:/Users/jain/Desktop/reg.csv')
anyNA(r)
cleandata<-r[complete.cases(r),]
q<-cleandata
View(q)
sample.split(data$Overall, SplitRatio = 0) ->splitr
subset(q,splitr==T) -> train
subset(q,splitr==F) ->  test
lm(Overall~Potential,data = train)-> mod1
predict(mod1,test)->res1
cbind(actual=test$Overall,predicted=res1)->final_data
plot(final_data)
lm.fit = lm(test$Overall~res1 , data = testd)
abline(lm.fit)
as.data.frame(final_data)->final_data
final_data$actual-final_data$predicted->error1
View(error1)
cbind(final_data,error1)->final_data1
View(final_data1)
sqrt(mean((final_data1$error1)^2))->rms1
View(rms1)
plot(final_data1)
