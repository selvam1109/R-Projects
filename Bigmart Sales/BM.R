train=read.csv(file.choose())
test=read.csv(file.choose())

library(gtools)
total=smartbind(train,test)

write.csv(total,file = "D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\total.csv",row.names = FALSE)
summary(total)

library(plyr)
count(total,c("Item_Type"))
count(total$Item_Type)
unique(total$Item_Type)

library(DMwR)
centralImputation(total)

library(Hmisc)
total$Item_Weight=with(total,impute(Item_Weight,mean))
View(total)
total$Outlet_Size=with(total,impute(Outlet_Size,mode))
summary(total)
library(mice)
total1=total
total1$Outlet_Size=sub("","NA",total1$Outlet_Size)
View(total1)
summary(total1$Outlet_Size)
summary(total$Outlet_Size)

###########################################################################

train=read.csv(file.choose())
test=read.csv(file.choose())

library(gtools)
total=smartbind(train,test)

write.csv(total,file = "D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\total.csv",row.names = FALSE)

total=read.csv(file.choose())
summary(total)

library(Hmisc)
total$Item_Weight=with(total,impute(Item_Weight,mean))
total$Outlet_Size=with(total,impute(Outlet_Size,mode))
summary(total)

######################### ONE HOT CODING OF CATEGORICAL VARIABLES ############################

total=read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\total.csv")
library(dummies)
total <- dummy.data.frame(total, names=c("Item_type_combined","Item_Fat_Content",
                                         "Outlet_Identifier","Outlet_Size","Outlet_Location_Type",
                                         "Outlet_Type"), sep="_")
View(total)
write.csv(total,file = "C:\\Users\\ADMIN\\Pictures\\total.csv",row.names = FALSE)


################################ BASELINE MODEL - LM ##################################

train <- read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\Modeling\\train.csv")
test <- read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\Modeling\\test.csv")

model_lm <- glm(Item_Outlet_Sales~.-Item_Identifier,data = train)
summary(model_lm)

num_train <- read.csv(file.choose())
library(corrplot)
correl <- cor(num_train)
corrplot(correl, method="circle")

library(car)
library(usdm)
vif(train)

levels(train$Item_type_combined_Drinks)
train$Item_type_combined_Drinks <- factor(train$Item_type_combined_Drinks)
sapply(train, class)

cols<-c(colnames(train))
cols
train[cols] <- lapply(train[cols], factor)
str(train)

cols1<-c("Item_Weight","Item_Visibility_avg","Item_MRP","Item_Outlet_Sales")
cols
train[cols1] <- lapply(train[cols1], as.numeric)
str(train)

train1 <- train
train1$Item_Identifier = NULL
names(train1)

vif(train[cols1])

cat_train=read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\Modeling\\cat_train.csv")
str(cat_train)
cols2=c(names(cat_train))
cat_train[cols2]=lapply(cat_train[cols2],as.factor)

chiSquare(cat_train)

library(MASS)
chisq.test(cat_train$Item_type_combined_Drinks,cat_train$Outlet_Location_Type_Tier.1)

library(rattle)
rattle()

train2 <- cbind(cat_train,num_train)
str(train2)
train3 <- as.data.frame(train2)
str(train3)
str(num_train)

train4 <- (read.csv("D:\\My Drive\\BAP R\\AV Competitions\\Bigmart Sales\\Modeling\\train.csv"))
str(train4)
