

#------------------libraries and working directory-----------------

setwd("C:/Users/VPL/Desktop/Ubiqum - Data Analysis/Course 2/Task 4")
library(readr)
library(arules)
library(arulesViz)

#----------------------import data-------------------------------
basket_data <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep=",", rm.duplicates=TRUE) 

#------------------------data exploration--------------------------------
inspect (basket_data)# You can view the transactions. Is there a way to see a certain # of transactions?
length (basket_data) # Number of transactions.
size (basket_data) # Number of items per transaction
LIST(basket_data) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(basket_data)# To see the item labels
summary(basket_data)
basket_data1 <- basket_data[which(size(basket_data) != 0)]

summary(basket_data1)
itemFrequencyPlot(basket_data1, topN = 10 )
image(basket_data1)

#---------------------------------removing imac (not implemented)-------------------------------
basket_data_sin_imac <- basket_data1
basket_data_sin_imac[basket_data_sin_imac == "iMac"] <- ""


#--------------------------------Rules---------------------------------------

Rule_basket<- apriori (basket_data1, parameter = list(supp = 0.007, conf = 0.25))

inspect(Rule_basket)
summary(Rule_basket)
top10rules <- subset(sort(Rule_basket, by = "lift")[1:438])
top10rules <- subset(sort)
                                                    
                                                    
rules_df <- as.data.frame(inspect(top10rules))
inspect(top10rules)
inspect(subset(Rule_basket, (lhs %in% c("iMac"))))
AceraspireRules <- subset(Rule_basket, items %in% "iMac")
inspect(AceraspireRules)
is.redundant(Rule_basket)

itemFrequency(basket_data1)
hist(size(basket_data1))

#-------------------------------------big set-------------------------------------
product_Big <- basket_data1[which(size(basket_data1) > 15)]
summary(product_Big)
Rule_basket_big<- apriori (product_Big, parameter = list(supp = 0.1, conf = 0.3, minlen = 5))
inspect(Rule_basket_big)

#-----------------------plots----------------------------------
plot(Rule_basket_big)
plot(basket_data1, method = "scatterplot", measure = "size")
plot(Rule_basket, method = "grouped", control = list(k = 5))
plot(top10rules, method="graph", control=list(type="items"))
plot(Rule_basket, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(Rule_basket,measure=c("support","lift"),shading="confidence",interactive=T)
plot(basket_data)
                 
