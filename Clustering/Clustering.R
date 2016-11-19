library(cluster)
library(fpc)

teens <- read.csv("D:\\My Drive\\BAP R\\Clustering\\snsdata.csv")
str(teens)
prop.table(table(teens$gender))*100
levels(teens$gender)
round(prop.table(table(teens$gender,useNA = "ifany"))*100,2)

summary(teens$age)

teens$age <- ifelse(teens$age >=13 & teens$age<20,teens$age,NA)
summary(teens$age)
teens$female <- ifelse(teens$gender == "F" & !is.na(teens$gender),1,0)
teens$unknown <- ifelse(is.na(teens$gender),1,0)
table(teens$unknown,useNA = "ifany")
table(teens$female,useNA  ="ifany")

summary(teens$age)
mean(teens$age,na.rm = TRUE)

aggregate(data=teens,age~gradyear,mean,na.rm=TRUE)

ave_age <- ave(teens$age,teens$gradyear,FUN = function(x) mean(x,na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age),ave_age,teens$age)
summary(teens$age)

#selecting the feature
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests,scale))

teens_clusters <- kmeans(interests_z,centers = 5)
teens_clusters$size
teens_clusters$centers

#remaining code has to be sent through email