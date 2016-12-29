library(caret)
setwd("D:\\My Drive\\BAP R\\AV Competitions\\Loan Prediction III")
train <- read.csv("train.csv",stringsAsFactors = TRUE)
str(train)
sum(is.na(train)) #NA values in training dataset

# Imputation using KNN method, scaling and centering of data
PreProcValues <- preProcess(train,method = c("knnImpute","center","scale"))
library(RANN)
train_processed <- predict(PreProcValues,train)
sum(is.na(train_processed))

# Converting Response variable to numeric
train_processed$Loan_Status <- ifelse(train_processed$Loan_Status == 'N',0,1)

id <- train_processed$Loan_ID
train_processed$Loan_ID <- NULL

str(train_processed)

# Dummy variables using one hot encoding
dmy <- dummyVars("~.",data = train_processed,fullRank = T)
train_transformed <- data.frame(predict(dmy, newdata = train_processed))

str(train_transformed)

# Converting the response variable back to categorical variable
train_transformed$Loan_Status <- as.factor(train_transformed$Loan_Status)

# Splitting dataset into train and test
index <- createDataPartition(train_transformed$Loan_Status, p=0.75, list = FALSE)
trainset <- train_transformed[index,]
testset <- train_transformed[-index,]

str(trainset)

# Feature Selection using Caret(Recursive Feature Elimination)
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)
outcomeName <- 'Loan_Status'
predictors <- names(trainset)[!names(trainset) %in% outcomeName]
Loan_Pred_Profile <- rfe(trainset[,predictors],trainset[,outcomeName],rfeControl = control)
Loan_Pred_Profile

# Top 5 predictors
predictors_new<-c("Credit_History","ApplicantIncome","LoanAmount",
              "CoapplicantIncome", "Property_Area.Semiurban")

# Model Building using all predictors
model_gbm<-train(trainset[,predictors],trainset[,outcomeName],method='gbm')
model_rf<-train(trainset[,predictors],trainset[,outcomeName],method='rf')
model_nnet<-train(trainset[,predictors],trainset[,outcomeName],method='nnet')
model_glm<-train(trainset[,predictors],trainset[,outcomeName],method='glm')

# Model Building using top 5 predictors
model_gbm_new<-train(trainset[,predictors_new],trainset[,outcomeName],method='gbm')
model_rf_new<-train(trainset[,predictors_new],trainset[,outcomeName],method='rf')
model_nnet_new<-train(trainset[,predictors_new],trainset[,outcomeName],method='nnet')
model_glm_new<-train(trainset[,predictors_new],trainset[,outcomeName],method='glm')

# Predictions using RF
predictions_rf_all<-predict.train(object=model_rf,testset[,predictors],type="raw")
confusionMatrix(predictions_rf_all,testset[,outcomeName])

predictions_rf_new<-predict.train(object=model_rf_new,testset[,predictors_new],type="raw")
confusionMatrix(predictions_rf_new,testset[,outcomeName])


# Predictions using Neural Network
predictions_nn_all<-predict.train(object=model_nnet,testset[,predictors],type="raw")
confusionMatrix(predictions_nn_all,testset[,outcomeName])

predictions_nn_new<-predict.train(object=model_nnet_new,testset[,predictors_new],type="raw")
confusionMatrix(predictions_nn_new,testset[,outcomeName])

# Reading the test file for submission
test_final <- read.csv("test.csv", stringsAsFactors = TRUE)
str(test_final)
sum(is.na(test_final))

# Imputing missing values in test dataset
PreProcValues_test <- preProcess(test_final,method = c("knnImpute","center","scale"))
test_processed <- predict(PreProcValues,test_final)
sum(is.na(test_processed))

# Removing Loan ID from test dataset
id_test <- test_processed$Loan_ID
test_processed$Loan_ID <- NULL

str(train_processed)

# Dummy variables using one hot encoding
dmy_test <- dummyVars("~.",data = test_processed,fullRank = T)
test_transformed <- data.frame(predict(dmy_test, newdata = test_processed))

str(test_transformed)

# Predictions using Neural Nework model with top 5 predictors
predictions_nn_final<-predict.train(object=model_nnet_new,
                                    test_transformed[,predictors_new],type="raw")
predictions_nn_final <- ifelse(predictions_nn_final==1,"Y","N")
write.csv(predictions_nn_final, file = "Predictions_NN_Top5.csv", row.names = FALSE)

