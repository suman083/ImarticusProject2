BankCreditCard<- read.csv("G:/Suman/batch34/Project 2_Logistic_R/BankCreditCard (1).csv", stringsAsFactors=TRUE)
View(`BankCreditCard.(1)`)
str(BankCreditCard) 
#bar plots
summary(BankCreditCard)
barplot(BankCreditCard$Credit_Amount)
barplot(table(BankCreditCard$Gender))
        
a = table(BankCreditCard$Gender,)
a
view(a)
barplot(a, col=c("blue","red"),
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

str(BankCreditCard)
d = table(BankCreditCard$Repayment_Status_Jan)
d
dim(d)
barplot(d, col = brewer.pal(6,'Set3'),
        main = " Repayment_Status_Jan",
        names.arg = c("no delay","1 m d","2 m d","3 m d", "4 m d ", "5 m d","6 m d"))

str(BankCreditCard)
d = table(BankCreditCard$Repayment_Status_Jan)
d
dim(d)
barplot(d, col = brewer.pal(6,'Set3'),
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
