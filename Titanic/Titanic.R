train<-read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Titanic\\train.csv",stringsAsFactors = FALSE)
test<-read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Titanic\\test.csv",stringsAsFactors = FALSE)

table(train$Survived)
prop.table(table(train$Survived))

table(train$Sex)
table(train$Sex,train$Survived)
prop.table(table(train$Sex,train$Survived))
round(prop.table(table(train$Sex,train$Survived))*100,2)

prop.table(table(train$Sex,train$Survived),1) #results in row-wise 100% proportions
prop.table(table(train$Sex,train$Survived),2) #results in column-wise 100% proportions

colnames(test)
test$Survived=0 #created a new column and assigning a value '0'
test$Survived[test$Sex=='female']=1

summary(train$Age)
train$Child<-0
train$Child[train$Age<18]<-1
table(train$Child)

aggregate(Survived~Child+Sex,data=train,FUN = sum)
aggregate(Survived~Child+Sex,data=train,FUN = function(x) {sum(x)/length(x)})

train$Fare2<-'30+'
train$Fare2[train$Fare<10]<-'<10'
train$Fare2[train$Fare>=10 & train$Fare<20]<-'10-20'
train$Fare2[train$Fare>=20 & train$Fare<30]<-'20-30'
table(train$Fare2)

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

##################### Decision Tree ###############################
library(rpart)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(fit)
?rpart.control

#chisq.test(table(train$Pclass,train$Sex))

Prediction <- predict(fit,test,type = "class")
submit <- data.frame(PassengerID = test$PassengerId, Survived = Prediction)
