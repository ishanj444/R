library(ggplot2)
library(dplyr)
data(diamonds)
summary(diamonds)
View(diamonds)
diamonds %>% filter(cut == "Ideal") -> Ideal_cut
Ideal_cut %>% filter(price>15000)->high_price
Ideal_cut %>% filter(price>10000 & price<15000)->medium_price
Ideal_cut %>% filter(price<10000)->low_price
 diamonds %>% select('x','y','z')-> diamond_dims
diamond_dims %>% filter(x>5 & y > 40)
diamonds %>% filter(color == "E") ->bestcol
bestcol %>% filter (clarity == "VVS1") ->bestclar
View(bestclar)
bestclar %>% summarise(mp = mean(price))
ggplot(data = diamonds,aes(x=cut)) +geom_bar(fill = "red")
ggplot(data = diamonds,aes(x=cut,fill = cut)) +geom_bar()
ggplot(data = diamonds,aes(y=price,x=carat))+ geom_point()
ggplot(data = diamonds,aes(y=price,x=carat))+ geom_point(col = "blue")
ggplot(data = diamonds,aes(y=price,x=clarity))+ geom_point(col = "blue")
ggplot(data = bestclar,aes(x=cut)) +geom_bar(fill = "red")

ggplot(data = diamonds,aes(y=price,x=carat))+ piechart()

#linear_regression
library(caTools)
sample.split(diamonds$depth, SplitRatio = 0.8) ->splitr
subset(diamonds,splitr==T) -> train
subset(diamonds,splitr==F) ->  test
lm(depth~carat+clarity+table+price,data = train)-> mod1
predict(mod1,test)->res1
cbind(actual=test$depth,predicted=res1)->final_data
as.data.frame(final_data)->final_data
final_data$actual-final_data$predicted->error1
View(error1)
cbind(final_data,error1)->final_data
View(final_data)
sqrt(mean((final_data$error1)^2))->rms1


sample.split(diamonds$depth, SplitRatio = 0.8) ->splitr
subset(diamonds,splitr==T) -> train
subset(diamonds,splitr==F) ->  test
lm(depth~carat+x+y+z,data = train)-> mod2
predict(mod2,test)->res2
cbind(actual=test$depth,predicted=res2)->final_data2
as.data.frame(final_data2)->final_data2
final_data2$actual-final_data2$predicted->error2
View(error2)
cbind(final_data2,error2)->final_data2
View(final_data2)
sqrt(mean((final_data2$error2)^2))->rms2

#logistic regression

View(mtcars)
glm(formula = vs~mpg,data = mtcars, family = "binomial") -> model1
predict(model1,data.frame(mpg=28),type ="response")
predict(model1,data.frame(mpg=c(20:30)),type ="response")

glm(formula = vs~disp,data = mtcars, family="binomial") -> model2
predict(model2,data.frame(disp=1000),type="response")
predict(model2,data.frame(disp=c(100,150,200)),type ="response")

#can include both the models simultaneously by including the variables in the glm func separating by commas

#confusion matrix

input<-read.csv('D:/customer_churn.csv')
anyNA(input)
cleandata<-input[complete.cases(input),]
data<-cleandata
print(data)


library(caTools)
sample.split(data$Churn, SplitRatio = 0.65) ->splitres
subset(data,splitres==T) -> train
subset(data,splitres==F) -> test

nrow(train)

glm(Churn~MonthlyCharges,data = train ,family = "binomial") ->modl
predict(modl,test,type = "response")->resl
View(resl)
cbind(test,resl)->d
View(d)

table(test$Churn,resl)
View(table(test$Churn,resl))
table(test$Churn,resl>0.41)


predict(modl,test)->res4

predict(View(test$Churn))

library(ROCR)
plot(res4)
prediction(resl,test$Churn) -> predl
performance(predl,"acc") -> acc
plot(acc)

performance(predl,"tpr","fpr") ->rocrc
#acc=accuracy
#tpr=true +ve rate
performance(predl,"tnr","fpr") ->rocrc1
plot(rocrc1)

#final aoc=area under the curve = implicit performance of all possible thresholds

performance(predl,"auc") -> auc

#decision tree
library(ISLR)
data(package="ISLR")
data("Carseats")
carseats<-Carseats
High = ifelse(Carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)

library(tree)

library(tree)