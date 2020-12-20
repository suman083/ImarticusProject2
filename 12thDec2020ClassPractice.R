BankCreditCard<- read.csv("G:/Suman/batch34/Project 2_Logistic_R/BankCreditCard.csv", stringsAsFactors=TRUE)
View(BankCreditCard)
str(BankCreditCard) 
#bar plots
summary(BankCreditCard)
barplot(BankCreditCard$Credit_Amount)
barplot(table(BankCreditCard$Gender))
        
a = table(BankCreditCard$Gender)
a
view(a)
barplot(a, col=c("blue","red"),
      main = "BankCreditCardGender",
      names.arg = c("M","F")
        
        ) 

barplot(a, col=brewer.pal(2,"Dark2"),
        main = "BankCreditCardGender",
        names.arg = c("M","F")
        
) 
  
####

b = table(BankCreditCard$Academic_Qualification)
b
dim(b)
barplot(b, col=c("blue","red","Green","yellow","pink","black"),
        main = " Academic_Qualificationr",
        names.arg = c("ug","g","pg","pf","ot","un"))

####
c = table(BankCreditCard$Marital)
c
dim(c)
barplot(c, col=c("blue","red","Green","yellow","pink","black","orange"),
        main = " Marital",
        names.arg = c("NA","M","S","dkn"))
# you need to take call if want to remove NA or not

str(BankCreditCard)
d = table(BankCreditCard$Repayment_Status_Jan)
d
dim(d)
barplot(d, col=c("blue","red","Green","yellow","pink","black","orange"),
        main = " Repayment_Status_Jan",
        names.arg = c("no delay","1 m d","2 m d","3 m d", "4 m d ", "5 m d","6 m d"))

library(RColorBrewer)
brewer.pal.info
view(brewer.pal)

str(BankCreditCard)
d = table(BankCreditCard$Repayment_Status_Jan)
d
dim(d)
barplot(d, col = brewer.pal(6,'BrBG'),
        main = " Repayment_Status_Jan",
        names.arg = c("no delay","1 m d","2 m d","3 m d", "4 m d ", "5 m d","6 m d"))

str(BankCreditCard)
d = table(BankCreditCard$Repayment_Status_Jan)
d
dim(d)
barplot(d, col = brewer.pal(6,'Pastel1'),
        main = " Repayment_Status_Jan",
        names.arg = c("no delay","1 m d","2 m d","3 m d", "4 m d ", "5 m d","6 m d"))
str(BankCreditCard)
par(mfrow=c(2,1))
hist(BankCreditCard$Credit_Amount, col ="blue" )        
boxplot(BankCreditCard$Credit_Amount,horizontal = T, col = "blue")
#many outliers 

##chisq test
chisq.test(BankCreditCard$Default_Payment,BankCreditCard$Academic_Qualification,correct=F)

#P Value less than 0.05 , goood predictor
chisq.test(BankCreditCard$Default_Payment,BankCreditCard$Gender,correct=F)
#P Value less than 0.05 , goood predictor

chisq.test(BankCreditCard$Default_Payment,BankCreditCard$Marital)
#P Value less than 0.05 , goood predictor
chisq.test(BankCreditCard$Default_Payment,BankCreditCard$Previous_Payment_Jan)
chisq.test(BankCreditCard$Repayment_Status_Jan,BankCreditCard$Default_Payment,correct = F)

#Jan_Bill_Amount


model21<-glm(default10yr~income+age+loan+LTI,data = creditset,family = binomial())
model21
predict2<-predict(model21,type = "response")
head(predict2,3)
creditset$predict<-predict2
creditset$predictround<-round(predict2,digits = 0)
table(creditset$default10yr,predict2>0.5)
table(creditset$default10yr,predict2>0.033)

pairs.panels(BankCreditCard[c("Credit_Amount","Age_Years","Jan_Bill_Amount","Feb_Bill_Amount","March_Bill_Amount","April_Bill_Amount","May_Bill_Amount","June_Bill_Amount")])
pairs.panels(BankCreditCard[c("Credit_Amount", "Default_Payment","Previous_Payment_Jan","Previous_Payment_Feb","Previous_Payment_March","Previous_Payment_April","Previous_Payment_May","Previous_Payment_June")])

cor(BankCreditCard[c( "Default_Payment","Previous_Payment_Jan","Previous_Payment_Feb","Previous_Payment_March","Previous_Payment_April","Previous_Payment_May","Previous_Payment_June")])
BankCreditCard$Default_Payment


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
BankCreditCard$predict<-predict2
BankCreditCard$predictround<-round(predict2,digits = 0)
table(BankCreditCard$Default_Payment,predict2>0.5)
table(BankCreditCard$Default_Payment,predict2>0.13)

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
install.packages("ROCR")
library(ROCR)
ROCPred=prediction(res,train$Default_Payment)
ROCPred
ROCRPref<-performance(ROCPred,"tpr","fpr")
plot(ROCRPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))
