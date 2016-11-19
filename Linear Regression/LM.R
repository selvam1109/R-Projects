
############################### COMMON STEPS ############################

library(MASS)
library(ISLR)

Boston1 <- Boston

head(Boston,10) # To get first 10 rows of the data set
tail(Boston,10) # To get last 10 rows of the data set
dim(Boston)

sapply(Boston,class)

summary(Boston)

sapply(Boston,sd)
fix(Boston)

write.csv(Boston,"D:/My Drive/BAP R/Datasets/Boston.csv")

library(e1071) # Library for checking the skweness of the data

# Step 2.1.1 : Histograms
par(mfrow = c(3,5)) # Defining the framework for fixing each histograms
for(i in 1:14){
  hist(Boston[,i],main=names(Boston)[i]) # Defining the histogram for each variables
}

# Fitting a abline for linear regression
par(mfrow = c(3,5)) # Defining the framework for fixing each histograms
for(i in 1:14){
  hist(Boston[,i],main=names(Boston)[i]) # Defining the histogram for each variables
  abline(lm(Boston$medv~Boston[,i]),col="blue")
}

# Indicating freq = false to give a density histogram
par(mfrow = c(3,5)) # Defining the framework for fixing each histograms
for(i in 1:14){
  hist(Boston[,i],main=names(Boston)[i],freq = FALSE) # Defining the histogram for each variables
}

# Step 2.1.2 : Density Plots
library(lattice) # This is the library which can be used for creating the density plots
par(mfrow = c(3,5)) # Defining the framework for fixing each density plot
for(i in 1:14){
  plot(density(Boston[,i]),main=names(Boston)[i]) # Defining the density plot for each variables
}

# Step 2.1.3 : Box and Whisker Plots
par(mfrow = c(3,5)) # Defining the framework for fixing each Box and Whisker plot
for(i in 1:14){
  boxplot(Boston[,i],main=names(Boston)[i]) # Defining the Box and whisker plot for each variables
}

library(Amelia)
par(mfrow = c(1,1))
missmap(Boston,col=c("black","grey"),legend=FALSE) # NO missing data

library(corrplot)
correl <- cor(Boston)
corrplot(correl, method="circle")

library(mlbench)
library(klaR)

############################## BASELINE MODEL ############################

validindex <- sample(1:506,(506*0.8)) # Create an index of rows
Bos_train <- Boston[validindex,] # Creating a training set
Bos_test <- Boston[-validindex,] # Creating a test set

bl_model <- lm(medv~.,data=Bos_train) # Running the baseline model
summary(bl_model) # Summary statistics of the model
plot(bl_model) # Diagnostic plots

Bos_test$pred <- predict(bl_model, newdata=Bos_test[,1:13]) # Predicting for the test set

Bos_test$error <- (Bos_test$medv - Bos_test$pred)^2 # Calculating the error term
baseline_err <- sqrt(sum(Bos_test$error)) # Calculating the Mean squre root error 
baseline_err

library(gmp)
library(usdm)
library(car)
vif(Bos_train1)

############################### ITERATION 1 ############################
colnames(Bos_train)
Bos_train1 <- Bos_train[c(-8,-9)] # New training set
Bos_test1 <- Bos_test[c(-8,-9)] # New test set

bl_model1 <- lm(medv~.,data=Bos_train1)
summary(bl_model1)

Bos_test1$pred1 <- predict(bl_model1, newdata=Bos_test1[,1:11]) # Predicting for the new test set
Bos_test1$error1 <- (Bos_test1$medv - Bos_test1$pred1)^2 # Calculating the error term
baseline_err1 <- sqrt(sum(Bos_test1$error1)) # Calculating the Mean squre root error 
baseline_err1

library(lattice) # This is the library which can be used for creating the density plots

par(mfrow = c(2,3)) # Defining the framework for fixing each density plot
for(i in 1:12){
  plot(density(Bos_train1[,i]),main=names(Bos_train1)[i]) # Defining the density plot for each variables
}




Bos_transform <- Bos_newtr
Bos_transform$crim <- log(Bos_transform$crim)
Bos_transform$nox <- log(Bos_transform$nox)  
Bos_transform$dis <- log(Bos_transform$dis)
Bos_transform$black <- log(Bos_transform$black)

par(mfrow = c(2,3)) # Defining the framework for fixing each density plot
for(i in 1:12){
 plot(density(Bos_transform[,i]),main=names(Bos_transform)[i]) # Defining the density plot for each variables
}

Bos_transform_test <- Bos_newte

Bos_transform_test$crim <- log(Bos_transform_test$crim)
Bos_transform_test$nox <- log(Bos_transform_test$nox)  
Bos_transform_test$dis <- log(Bos_transform_test$dis)
Bos_transform_test$black <- log(Bos_transform_test$black)

tr2_model <- lm(medv~.,data=Bos_transform)
summary(tr2_model)

Bos_transform_test$pred3 <- predict(tr2_model, newdata=Bos_transform_test[,1:11]) # Predicting for the new test set
Bos_transform_test$error3 <- (Bos_transform_test$medv - Bos_transform_test$pred3)^2 # Calculating the error term
baseline_err3 <- sqrt(sum(Bos_transform_test$error3)) # Calculating the Mean squre root error 
baseline_err3
errordf <- c(baseline_err,baseline_err2,baseline_err3) # Making an error vector

lines(errordf) # Plotting the error vector

Bos_transform <- Bos_train

Bos_transform$crim <- sqrt(Bos_transform$crim)
Bos_transform$nox <- sqrt(Bos_transform$nox)  
Bos_transform$dis <- sqrt(Bos_transform$dis)
Bos_transform$black <- sqrt(Bos_transform$black)

par(mfrow = c(2,6)) # Defining the framework for fixing each density plot
for(i in 1:12){
  plot(density(Bos_transform[,i]),main=names(Bos_transform)[i]) # Defining the density plot for each variables
}

Bos_transform_test <- Bos_test

Bos_transform_test$crim <- sqrt(Bos_transform_test$crim)
Bos_transform_test$nox <- sqrt(Bos_transform_test$nox)  
Bos_transform_test$dis <- sqrt(Bos_transform_test$dis)
Bos_transform_test$black <- sqrt(Bos_transform_test$black)

tr2_model <- lm(medv~.,data=Bos_transform)
summary(tr2_model)

Bos_transform_test$pred3 <- predict(tr2_model, newdata=Bos_transform_test[,1:13]) # Predicting for the new test set
Bos_transform_test$error3 <- (Bos_transform_test$medv - Bos_transform_test$pred3)^2 # Calculating the error term
baseline_err3 <- sqrt(sum(Bos_transform_test$error3)) # Calculating the Mean squre root error 
baseline_err3
errordf <- c(baseline_err,baseline_err2,baseline_err3) # Making an error vector

lines(errordf) ## Plotting the error vector


