setwd("C:/Users/ansroy/Desktop/Personal/IIIT B/Assignment & Case Study/Assignment - Association Rule Mining")
library("arules")
library("arulesViz")
library("ggplot2")
library(plyr)
arm <- read.csv("Global Superstore.csv", header = TRUE)
dim(arm)  #51290 rows, 24 Cols, Row.ID is the rowid column from excel & not available in Data Dictionary. Need to remove it.
arm <- arm[,-1]
dim(arm)  #51290 rows, 24 Cols, Order.ID is the key column.
str(arm) #Most of the cols are character/ Numeric data type. Our consideration will be sub-category
summary(arm)
nrow(unique(arm)) #When grouped together, all the records are unique. Number of records - 51290
#Lets find unique order id count
length(unique(arm$Order.ID)) 
length(unique(arm$Sub.Category)) #17 unique sub category
#25035 unique Order.ID. Meaning, multiple products of same/different subcategories were ordered on the same
#day with same order ID.

#EDA
ggplot(arm,aes(Sub.Category,fill=Sub.Category)) + geom_bar() + labs(title="Sub Category count")
ggplot(arm,aes(Market,fill=Market)) + geom_bar() + labs(title="Market wise Order count") 
ggplot(arm,aes(Order.Priority,fill=Order.Priority)) + geom_bar() + labs(title="Orders based on priority") 

#There are two ways to convert the above data frame to transactional format. 
#1. Using as function. For this, we need to convert all the required columns into Factor type and then use this. Basically,
#this requires that we manually implement the formating of the data.
#2. Using read.transactions. For this, we need to first store the mini data frame(having only the require columns in factor format)
#to a csv file and then read it using read.transactions function.
#The second approach is much easier to implement and removes any irregularities possible in the data and hence we proceeding with the same.

arm_mini <- arm[,c("Order.ID","Sub.Category")]
write.csv(arm_mini,"Global.Superstore_mini.csv",row.names = F)
arm_transactions <- read.transactions(file = "Global.Superstore_mini.csv",format = "single",sep = ",",cols = c("Order.ID","Sub.Category"),rm.duplicates = T)

#Use itemFrequencyPlot function to plot the graph with frequency of occurance.
itemFrequencyPlot(arm_transactions,topN=20,type="absolute")

#apriori function is used to convert the transactional data (arm_transactions) into rules. There are three important parameters.
#Lift : Lift is one more parameter of interest in the association analysis. Lift is nothing but the ratio of Confidence to 
#Expected Confidence.A lift ratio larger than 1.0 implies that the relationship between the antecedent and the consequent 
#is more significant than would be expected if the two sets were independent. The larger the lift ratio, the more significant 
#the association.
#In short, Lift signifies the correlation between LHS & RHS & hence, greater lift is better selection.

#Support: The support of a rule indicates how frequently the items in the rule occur together. 

#Confidence: The confidence of a rule indicates the probability of both the antecedent and the consequent appearing 
#in the same transaction. Confidence is the conditional probability of the consequent given the antecedent. 

#Both support and confidence along with Lift of greater than 1 must be used to determine if a rule is valid

#Setting the min value of support to 0.0001 and confidence = 0.00001. Reason is to create as much rules as possible. And then
#will analyse the rules created.
rules <- apriori(arm_transactions, parameter = list(support = 0.0001,confidence=0.00001))


#Using the above rules, lets find the rules based on support.
df_basket <- as(rules,"data.frame")
df_support_desc<-arrange(df_basket,desc(support)) #This suggest that the max support is 0.21537 with lift = 1 and confidence = 0.2153


#Arrange df_basket based on confidence & create a df for the same
df_basket <- as(rules,"data.frame")
df_confidence_desc<-arrange(df_basket,desc(confidence)) #Max confidence is 1 with very small support & lift = 5.


#Arrange df_basket based on Lift & create a df for the same
df_basket <- as(rules,"data.frame")
df_lift_desc<-arrange(df_basket,desc(lift)) #Max lift is 17 with confidence of 0.6 and support = 0.000119



#Clearly, we cannt conclude using any one of the statistics. We may have to take a collection of them.
#Among the three, support & confidence should be good and having lift > 1 is minimum requiremnt.
#All the three dataframes are same, just sorted by different items.Lets create data frame using certain filter criteria to see 
#the division of the data. We know, the highest value of each set now i.e. support is 0.21537, lift is 17, confidence is 1.
#Confidence being 1, lets start by reducing confidence and checking how the support and lift gets altered. Also, how many
#rules we are able to create. Also note, we have total 25000 unique orderId. a support of 0.0001 = 0.0001*25k records = 2.5 records which is very less a support.
#So while fixing support, lets start with a decent value say 
df_test1 <- arrange(df_lift_desc[df_lift_desc$confidence >= 0.75 & df_lift_desc$lift > 1,] ,desc(support))  #filtering based on confidence and sorting based on support
nrow(df_test1) #Total number of rules =789
#Clearly, with confidence >= 0.75, the support is 0.00047 which is very less compared to the max support we have. Lets change the values again.

#Confidence >= 0.6

df_test1 <- arrange(df_lift_desc[df_lift_desc$confidence >= 0.6 & df_lift_desc$lift > 1,] ,desc(support))  #filtering based on confidence and sorting based on support
nrow(df_test1) #Total number of rules =1520
#Clearly, with confidence >= 0.6, the max support has though increased 0.0012 but still less.


#Confidence >= 0.5

df_test1 <- arrange(df_lift_desc[df_lift_desc$confidence >= 0.5 & df_lift_desc$lift > 1,] ,desc(support))  #filtering based on confidence and sorting based on support
nrow(df_test1) #Total number of rules =2677
#Clearly, with confidence >= 0.5, the max support has though remained same, but we have increase in number of rules.



#Lets fix support of 0.1 and check the confidence values
#support >= 0.1
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.1 & df_lift_desc$lift > 1,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =0
#Clearly, with support >= 0.1 & lift > 1, there is no rules created.


#Lets  reduce support value & fix support of 0.01 & lift > 1 and check the confidence values
#support >= 0.01
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.01 & df_lift_desc$lift > 1,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =33
#Clearly, with support >= 0.01, the max confidence is only 0.23 which is very less.


#Lets  reduce support value & fix support of 0.001 & lift > 1 and check the confidence values
#support >= 0.01
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.001 & df_lift_desc$lift > 1,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =1679
#Clearly, with support >= 0.001, the max confidence is only 0.6. Though its a good value, but there is only 1 rule with such confidence



#Lets  reduce support value & fix support of 0.0006 & lift > 1 and check the confidence values
#support >= 0.01
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.0006 & df_lift_desc$lift > 1,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =2794
#Clearly, with support >= 0.0006, the max confidence is 0.6. There are 8 Rules with confidence greater than 0.5 and support greater than 0.0006

#Lets fix support of 0.0006 & lift > 1 and check the confidence values >= 0.5
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.0006 & df_lift_desc$lift > 1 & df_lift_desc$confidence >= 0.5,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =8



#Lets   fix support of 0.0004 & confidence values >= 0.5.
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.0004 & df_lift_desc$lift >= 1 & df_lift_desc$confidence >= 0.5,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =37



# # #Among the 37 rules, we have support >= 0.0005 & confidence >= 0.5. We also have lift having values greater than 1. Lets try fixing support now again.
# # #Lets   fix support of 0.0005 & lift >=1 & confidence values >= 0.5.
df_test1 <- arrange(df_lift_desc[df_lift_desc$support >= 0.0005 & df_lift_desc$lift >= 1 & df_lift_desc$confidence >= 0.5,] ,desc(confidence))  #filtering based on support and sorting based on confidence
nrow(df_test1) #Total number of rules =22
# # #Clearly, with support >= 0.0005, the min confidence is 0.5 & lift >=1, we get 22 rules. 

#We can take the above values of support and confidence to further create rules and analyse them below.


 

#Lets re-create the rules based on above finalisations. We will take confidence = 0.5 & try to check the rules. Again increasing confidence 
#& checking for the rule count. And then again increasing the support to 0.0005, we reached at below rule.
rules_final <- apriori(arm_transactions, parameter = list(support = 0.0005,confidence=0.5)) #22 rules without lift in consideration.
df_final <- as(rules_final,"data.frame") #THe lift is greater than 1 which is our min requirement for a good correlation.

coverage(rules_final)
is.redundant(rules_final) #all outputs are TRUE.
plot(rules_final,method="graph",interactive=TRUE,shading=NA)

#Checkpoint 3:
#Confidence = 0.5, support = 0.0005 & lift =1

#There are 22 rules, and they clearly explains if a customer buys a combination of items in LHS, then he is going to buy the item on RHS.
#The probability being more than 75% indicates that these rules are viable and possible & the store should proceed with implementing them.
#Among 25k unique orderId's the support for these rules are more than 0.0005 meaning among 25k orders,more than 12-13 orders are on the same
#rules. The lift for these rules being more than 1 implies that these items are very often bought together & are highly correlated.



#Closely, looking at the rules also suggest that in most of the rules, the item on the RHS is Binders, meaning combination of
#Appliances,Bookcases,Paper,Storage,Phones,Labels,Art,Chairs will lead to buying Binders. So, they can be promoted accordingly.



####################################################################################
##############################END OF ASSIGNMENT#####################################
#Additional code can be used in future for any analysis.
# top_conf<-head(rules, n=10, by= "confidence")
# top_lift<-head(rules, n=10, by= "lift")
# top_support<-head(rules, n=10, by= "support")
# inspect(top_conf)
# inspect(top_lift)
# inspect(top_support)


