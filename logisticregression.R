library(caTools)
library(readxl)
library(ggplot2)
library(dplyr)
library(ROCR)
y<-read_excel('D:/datasets/EmployeeData.xlsx')
cleandata<-y[complete.cases(y),]
data<-cleandata
print(data)


split <- sample.split(data$Attrition, SplitRatio = 0.8)
train <- subset(train, split == TRUE)
test <- subset(train, split == FALSE)

#logistic regression model
#model <- glm (data$Attrition ~ data$PercentSalaryHike, data = train, family = binomial)
#summary(model)
#predict <- predict(model, type = 'response')
#View(predict)

model <- glm(data$Attrition ~data$PercentSalaryHike,family=binomial(link='logit'),data=train)
summary(model)

anova(model, test="Chisq")

library(pscl)
pR2(model)

fitted.results <- predict(model,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))

p <- predict(model, type="response")
pr <- prediction(p, test$Attrition)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
#table(train$Attrition, predict > 0.5)
#ROCRpred <- prediction(predict, train$Attrition)
#ROCRperf <- performance(ROCRpred, 'tpr','fpr')
#plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#ggplot(train, aes(x=data$PercentSalaryHike, y=data$Attrition)) + geom_point() + 
#  stat_smooth(method="glm", family="binomial", se=FALSE)




model2 <- glm(data$Attrition ~data$PerformanceRating,family=binomial(link='logit'),data=train)
summary(model)

anova(model2, test="Chisq")
fitted.results <- predict(model2,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))



model3 <- glm(data$Attrition ~data$RelationshipSatisfaction,family=binomial(link='logit'),data=train)
summary(model)

anova(model3, test="Chisq")
fitted.results <- predict(model3,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))



model4 <- glm(data$Attrition ~data$StockOptionLevel,family=binomial(link='logit'),data=train)
summary(model)

anova(model4, test="Chisq")
fitted.results <- predict(model4,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))


model5 <- glm(data$Attrition ~data$TotalWorkingYears,family=binomial(link='logit'),data=train)
summary(model)

anova(model5, test="Chisq")
fitted.results <- predict(model5,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))


model6 <- glm(data$Attrition ~data$TrainingTimesLastYear,family=binomial(link='logit'),data=train)
summary(model)

anova(model6, test="Chisq")
fitted.results <- predict(model6,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))




model7 <- glm(data$Attrition ~data$WorkLifeBalance,family=binomial(link='logit'),data=train)
summary(model)

anova(model7, test="Chisq")
fitted.results <- predict(model7,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))




model8 <- glm(data$Attrition ~data$YearsAtCompany,family=binomial(link='logit'),data=train)
summary(model)

anova(model8, test="Chisq")
fitted.results <- predict(model8,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Attrition)
print(paste('Accuracy',1-misClasificError))