dim(Retail_Data)
str(Retail_Data)

txn_data<-Retail_Data
summary(txn_data)

library(arules)
sample=txn_data[1:12000,2:4]
for(i in 1:ncol(sample))
{
  sample[,i]=as.factor(sample[,i])
}

sample=as(sample,"transactions")
sample
itemFrequencyPlot(sample,topN=9,"absolute")
itemFrequencyPlot(sample,topN=9,"relative")

basket_rules=apriori(sample,parameter = list(sup=0.5,conf=0.01,target="rules",minlen=1,maxlen=3))
summary(basket_rules)
inspect(basket_rules)
inspect(head(sort(basket_rules,by="lift"),20))

#verbose=T will display the console and with F will not display in console
basket_rules=apriori(data=sample,parameter = list(sup=0.005,conf=0.01,minlen=1,maxlen=3),
                     appearance = list(default="lhs",rhs="Prod3=H"),control=list(verbose=T))
basket_rules=apriori(data=sample,parameter = list(sup=0.005,conf=0.01,minlen=1,maxlen=3),
                     appearance = list(default="lhs",rhs="Prod3=H"),control=list(verbose=F))

inspect(basket_rules)


