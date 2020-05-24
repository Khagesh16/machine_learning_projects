#install.packages("arules")
#install.packages("plyr", dependencies = TRUE)
library(arules)
library(plyr)
library(dplyr)

data= read.csv("Cleaned_OnlineRetail.csv")
View(data)

##Getting Substring from date time
data$Date <- substr(data$InvoiceDate, 1, 9)
View(data$Date)
#head(data)

##Generating data of the customer, from the same ID on the same date
data_list<-ddply(data,c("CustomerID","Date"),function(dfl)paste(dfl$Description, collapse = ","))

nrow(data_list)
View(data_list)
##Putting CustomerID and Date Null
data_list$CustomerID<-NULL
data_list$Date<-NULL

#lift support confidence values

#Removing the unnecessary column
data_list<-data_list[-1,]

##Writing it to the market_basket comma seperated file
write.csv(data_list,"market_basket.csv", quote = FALSE, row.names = FALSE)

tr = read.transactions("market_basket.csv",format="basket",sep=",")


## Implementing Apriori Algorithm
itemFrequencyPlot(tr, topN=10)
rules = apriori(tr,parameter = list(supp=0.001,conf=0.8))

# we are not getting better results with lift so used support and confidence
## Generating Rules 
inspect(rules[1:20])

#Inspecting Rules by Support value
inspect(sort(rules,by='support')[1:100])

#Inspecting Rules by confidence value
inspect(sort(rules,by='confidence')[1:100])


# Generating Unique Itemsets 
itemsets=unique(generatingItemsets(sort(rules, by="confidence"))[1:100])


## Analysing the frequent itemsets, arranged by their support value, DESC = TRUE (by default)
inspect(sort(itemsets[1:40], by="support"))

## Getting the Maximal Frequent Itemsets in our rules
inspect(sort(itemsets[is.maximal(itemsets)][1:40], by="support"))
