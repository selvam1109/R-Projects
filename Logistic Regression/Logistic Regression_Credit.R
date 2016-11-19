
####################### BASELINE MODEL ##################################

credit=read.csv(file.choose())

str(credit) #structure of the dataset
summary(credit)# summary of the dataset

set.seed(234)
index <- sample(1:nrow(credit), size=0.8*nrow(credit))
train <- credit[index,]
test <- credit[-index,]

library(glm2)

logit=glm(rating~.,data=train,family=binomial())
logit
summary(logit)
coef(logit)
exp(coef(logit))#to make the  model more precise by showing the odds value
exp(confint(logit))#gives a range value - confidence interval

pred=predict(logit,test,type = "response")#type="response" will give the probability values.
ss=floor(pred+0.5)# to  figure threshold limit

table(test$rating,ss)#confusion matrix/table
table(Actual=test$rating,Predicted=ss)

pred
ss
pred=predict(logit,newdata=testdata,type="response")# to predict using unseen or testing data.

library(ROCR)

tprfpr=prediction(ss,test$rating)#
perf=performance(tprfpr,"tpr","fpr")
plot(perf)#to plot TPR against FPR - Area Under Curve

auc=performance(tprfpr,"auc")
auc #Value of area under curve

library("fmsb")
NagelkerkeR2(logit)
library("pscl")
pR2(logit)

# perform and evaluate predictions
lr.predictions <- predict(logit, test, type="response")
library(SDMTools)
library(OptimalCutpoints)
library(InformationValue)
optCutOff <- optimalCutoff(test$rating, lr.predictions)[1]
optCutOff

library("plotROC")
plotROC(test$rating, lr.predictions)


# Specificity and Sensitivity
sensitivity(test$rating, pred, threshold = optCutOff)
specificity(test$rating, pred, threshold = optCutOff)

resid(logit)
summary(logit)
fitted.values(logit)

#install package "aod"
#library(aod)

####################### ITERATION 1 ##################################

set.seed(234)
logit1=glm(rating~.-guarantor-residence.duration-occupation
           -dependents,data=train,family=binomial())
logit1
summary(logit1)

pred1=predict(logit1,test,type = "response")#type="response" will give the probability values.
ss1=floor(pred1+0.5)# to  figure threshold limit

table(test$rating,ss1)#confusion matrix/table
table(Actual=test$rating,Predicted=ss1)

library(ROCR)

tprfpr1=prediction(ss1,test$rating)
perf1=performance(tprfpr1,"tpr","fpr")
plot(perf1)#to plot TPR against FPR - Area Under Curve

auc1=performance(tprfpr1,"auc")
auc1 #Value of area under curve


# perform and evaluate predictions

library(SDMTools)
library(OptimalCutpoints)
library(InformationValue)
optCutOff1 <- optimalCutoff(test$rating, pred1)[1]
optCutOff1

library("plotROC")
plotROC(test$rating, pred1)

# Specificity and Sensitivity
sensitivity(test$rating, pred1, threshold = optCutOff1)
specificity(test$rating, pred1, threshold = optCutOff1)


####################### ITERATION 2 ##################################

set.seed(234)
logit2=glm(rating~.-guarantor-residence.duration-occupation
           -dependents-age,data=train,family=binomial())
logit2
summary(logit2)

pred2=predict(logit2,test,type = "response")#type="response" will give the probability values.
ss2=floor(pred2+0.5)# to  figure threshold limit

table(test$rating,ss2)#confusion matrix/table
table(Actual=test$rating,Predicted=ss2)

library(ROCR)

tprfpr2=prediction(ss2,test$rating)
perf2=performance(tprfpr2,"tpr","fpr")
plot(perf2)#to plot TPR against FPR - Area Under Curve

auc2=performance(tprfpr2,"auc")
auc2 #Value of area under curve


# perform and evaluate predictions

library(SDMTools)
library(OptimalCutpoints)
library(InformationValue)
optCutOff2 <- optimalCutoff(test$rating, pred2)[1]
optCutOff2

library("plotROC")
plotROC(test$rating, pred2)

# Specificity and Sensitivity
sensitivity(test$rating, pred2, threshold = optCutOff2)
specificity(test$rating, pred2, threshold = optCutOff2)


####################### ITERATION 3 ##################################

set.seed(234)
logit3=glm(rating~.-guarantor-residence.duration-bank.credits-
           other.credits-dependents,data=train,family=binomial())
summary(logit3)

pred3=predict(logit3,test,type = "response")#type="response" will give the probability values.
ss3=floor(pred2+0.5)# to  figure threshold limit

table(test$rating,ss3)#confusion matrix/table
table(Actual=test$rating,Predicted=ss3)

library(ROCR)

tprfpr3=prediction(ss3,test$rating)
perf3=performance(tprfpr3,"tpr","fpr")
plot(perf3)#to plot TPR against FPR - Area Under Curve

auc3=performance(tprfpr3,"auc")
auc3 #Value of area under curve


# perform and evaluate predictions

library(SDMTools)
library(OptimalCutpoints)
library(InformationValue)
optCutOff3 <- optimalCutoff(test$rating, pred3)[1]
optCutOff3

library("plotROC")
plotROC(test$rating, pred3)

# Specificity and Sensitivity
sensitivity(test$rating, pred3, threshold = optCutOff3)
specificity(test$rating, pred3, threshold = optCutOff3)

