
BankCreditCard<- read.csv("G:/Suman/batch34/Project 2_Logistic_R/BankCreditCard.csv", stringsAsFactors=TRUE)
View(BankCreditCard)
str(BankCreditCard) 



model7<- glm(Default_Payment~Credit_Amount+Gender
             +April_Bill_Amount+Feb_Bill_Amount         
             +Marital+Repayment_Status_Jan+Repayment_Status_Feb
             +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May
             +Repayment_Status_June, data = BankCreditCard, family = binomial)  
summary(model7)
#BankCreditCard$creditHope<-BankCreditCard$Credit_Amount+(BankCreditCard$Jan_Bill_Amount+BankCreditCard$Feb_Bill_Amount+BankCreditCard$March_Bill_Amount+BankCreditCard$April_Bill_Amount+BankCreditCard$May_Bill_Amount+BankCreditCard$June_Bill_Amount)
#view(BankCreditCard$creditHope)

model8<- glm(Default_Payment~Gender
             
             +Marital+Repayment_Status_Jan+Repayment_Status_Feb
             +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May
             +Repayment_Status_June, data = BankCreditCard, family = binomial)  
summary(model8)
predict2<-predict(model8,type = "response")
head(predict2,3)

###

BankCreditCard
summary(BankCreditCard)
80*(0.70)

numberrows<-nrow(BankCreditCard)
train1<-head(BankCreditCard,numberrows*.7)
train1
numberrows*.7
View(train1)
test1<- tail(BankCreditCard, numberrows*.3)
test1
BankCreditCard$predict<-predict2
BankCreditCard$predictround<-round(predict2,digits = 0)
table(BankCreditCard$Default_Payment,predict2>0.5)
table(BankCreditCard$Default_Payment,predict2>0.1)
table(BankCreditCard$Default_Payment,predict2>0.2)
table(BankCreditCard$Default_Payment,predict2>0.3)



#splitting the data into training and test data
set.seed(2)
#install.packages("caTools")
library(caTools)
split<-sample.split(BankCreditCard,SplitRatio = 0.7)
split
train<-subset(BankCreditCard,split="TRUE")
test<-subset(BankCreditCard,split="FALSE")
train
test
pridict_price<-predict(model8,train)



##selecting cut off
res<-predict(model8,train,type = "response")
#install.packages("ROCR")
library(ROCR)
ROCPred=prediction(res,train$Default_Payment)
ROCPred
ROCRPref<-performance(ROCPred,"tpr","fpr")
plot(ROCRPref,main="cutoff selection",
     colorize=TRUE,print.cutoffs.at=seq(0.075,by=0.075), print.auc = T,lwd = 4)


###Accurecy 

confmatrix=table(Actual_Value=train$Default_Payment, predicted_Value=res>0.38)
confmatrix
#Accuracy of model
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix)
table(train$Default_Payment)
6636/(23364+6636)

install.packages("smotefamily")
library(smotefamily)
install.packages("ROSE")
library(ROSE)

model8<- glm(Default_Payment~Gender
             
             +Marital+Repayment_Status_Jan+Repayment_Status_Feb
             +Repayment_Status_March+Repayment_Status_April+Repayment_Status_May
             +Repayment_Status_June, data = train, family = binomial)  
summary(model8)

