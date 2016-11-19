testdata=read.csv(file.choose())#importing dataset and creating a dataframe named "testdata"
str(testdata) #structure of the dataset
summary(testdata)# summary of the dataset

library(glm2)
logit=glm(Loyalty~Brand+Product+Shopping,data=testdata,family=binomial())
head(testdata)
logit
summary(logit)
coef(logit)
exp(coef(logit))#to make the  model more precise by showing the odds value
exp(confint(logit))#gives a range value - confidence interval

pred=predict(logit,type="response")#type="response" will give the probability values.
ss1=floor(pred+0.5)# to  figure threshold limit
table(testdata$Loyalty,ss1)#confusion matrix/table
table(Actual=testdata$Loyalty,Predicted=ss1)
pred
ss1
pred=predict(logit,newdata=testdata,type="response")# to predict using unseen or testing data.

library(ROCR)

pred1=prediction(ss1,testdata$Loyalty)#
perf=performance(pred1,"tpr","fpr")
plot(perf)#to plot TPR against FPR - Area Under Curve

auc=performance(pred1,"auc")
auc #Value of area under curve

resid(logit)
summary(logit)
fitted.values(logit)


