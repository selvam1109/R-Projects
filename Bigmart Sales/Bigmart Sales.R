################### Data Exploration and Cleansing ###########################

train=read.csv(file.choose())
summary(train)
str(train)

train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
summary(train$Item_Fat_Content)

train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == "Low Fat","LF",train$Item_Fat_Content)
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == "Low Fat","LF",
                                 (ifelse(train$Item_Fat_Content=="low fat","LF",
                                         train$Item_Fat_Content)))
train$Item_Fat_Content <- ifelse(train$Item_Fat_Content == "reg","Regular",train$Item_Fat_Content)

train$Item_Fat_Content <- factor(train$Item_Fat_Content)

train_LF <- train[which(train$Item_Fat_Content == "LF"),]
summary(train_LF$Item_Weight)

train_Reg <- train[which(train$Item_Fat_Content == "Regular"),]
summary(train_Reg$Item_Weight)

########################## Data Imputation ###############################
library(DMwR)
DMwR::centralImputation(train)
summary(train)

mean(train$Item_Weight)

train$Item_Weight <- ifelse(is.na(train$Item_Weight),12.6,train$Item_Weight)
summary(train)

############### Data Export and Import of Transformed data ################

write.csv(train, file = "D:\\My Drive\\BAP R\\Bigmart Sales\\Train.csv",row.names=FALSE)

# Log transformation has been applied to MRP and Sales

train <- read.csv(file= "D:/My Drive/BAP R/Bigmart Sales/To Send/Train_Cleansed_Transformed.csv")
summary(train)
str(train)

##################### Baseline Model - Linear Regression #################
model_bl <- lm(log_sales ~ .-Item_Identifier-
                 Outlet_Identifier-Item_Type-Item_Fat_Content,data = train)
summary(model_bl)
str(train)
train$Establishment_Age <- as.numeric(train$Establishment_Age)

par(mfrow=c(2,3))
hist(train$log_sales)
summary(train)

############### Splitting Data into Train and Test datasets ###############

indexes <- sample(1:nrow(train), size=0.8*nrow(train))
train.data <- train[indexes,]
test.data <- train[-indexes,]
write.csv(train.data,file = "D:\\My Drive\\BAP R\\Bigmart Sales\\train.data.csv",row.names = FALSE)
write.csv(test.data,file = "D:/My Drive/BAP R/Bigmart Sales/test.data.csv",row.names = FALSE)

############ Removed Item ID & Outlet ID and importing again ###############

train.data <- read.csv("D:\\My Drive\\BAP R\\Bigmart Sales\\To Send\\train.data.csv")
test.data <- read.csv("D:\\My Drive\\BAP R\\Bigmart Sales\\To Send\\test.data.csv")

summary(test.data)

############## Linear Model 1 #######################
model1 <- lm(log_sales ~ .,data = train.data)
summary(model1)

library(car)
vif(model1)

train.data<-train.data[,-4] #removed Item Type as it has VIF value > 4.
test.data<-test.data[,-4]

############## Linear Model 2 #######################
model2 <- lm(log_sales ~ .,data = train.data)
summary(model2)

############ Prediction using Linear Model ################
test.data$predict <- predict(model2,test.data)

write.csv(test.data,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/Predicted Data_LM.csv",row.names = FALSE)


#################### Random Forest Model ##################

data_RF <- read.csv(file= "D:/My Drive/BAP R/Bigmart Sales/To Send/Data_RF.csv")

set.seed(1234)
indexes <- sample(1:nrow(data_RF), size=0.8*nrow(data_RF))

train_RF <- data_RF[indexes,]
write.csv(train_RF,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/train_RF_with IDs.csv",
          row.names = FALSE)

train_RF <- train_RF[,-1]
write.csv(train_RF,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/train_RF.csv",
          row.names = FALSE)

test_RF <- data_RF[-indexes,]
write.csv(test_RF,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/test_RF_with IDs.csv",
          row.names = FALSE)

test_RF <- test_RF[,-12]
test_RF <- test_RF[,-1]
write.csv(test_RF,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/test_RF.csv",
          row.names = FALSE)

library(randomForest)
library(caret)
library(e1071)
library(ROCR) 

############### Initial Model ##################


formula <- "Sales ~ ."
formula <- as.formula(formula)
model_rf <- randomForest(formula, data = train_RF, importance=T, proximity=T)

sales_pred <- predict(model_rf,test_RF)

write.csv(sales_pred,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/Predict_RF_initial.csv",
          row.names = FALSE)

# Model 2
formula <- "Sales ~ ."
formula <- as.formula(formula)
model_rf2 <- randomForest(formula, data = train_RF,ntree = 100, importance=T, proximity=T)

sales_pred2 <- predict(model_rf2,test_RF)

write.csv(sales_pred2,file = "D:/My Drive/BAP R/Bigmart Sales/To Send/Predict_RF_model2.csv",
          row.names = FALSE)