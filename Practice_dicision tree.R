##
student <- read.csv("G:/Suman/batch34/student.csv", stringsAsFactors=TRUE)

library(caTools)
library(tree)
set.seed(1)
split<-sample.split(student$Grade,SplitRatio = 0.70)
studentTrain<-subset(student,split==TRUE)
studentTest<-subset(student,split==FALSE)
table(student$Grade)
table(studentTrain$Grade)
table(studentTest$Grade)
prop.table(table(studentTest$Grade))
table(studentTest$Grade)
modelclassTree<-tree(Grade ~ Motivation+Age+Gender, data = student)
plot(modelclassTree)
summary(modelclassTree)

#Regression tree / decision tree
#View(student)
names(student)
nrow(student)
ncol(student)
str(student)

select_row<-sample(1:nrow(student),round(0.2*nrow(student)),replace = F)
select_row
stuTest<-student[select_row,]
stuTrain<-student[-(select_row),]
modelReg_tree<-tree(Mark~Motivation+Gender+Age, data = stuTrain)
plot(modelReg_tree)

plot(Mark~Motivation, data = student)
abline(lm(Mark~Motivation, data = student),col='blue')
modelLR<-lm(Mark~Motivation + Gender + Age, data =student )
summary(modelLR)

plot(Mark~Age, data = student)
abline(lm(Mark~Age, data = student),col='blue')


text(modelReg_tree,pretty = 0,cex=0.75)
pred_1<-predict(modelReg_tree,newdata = stuTest)
head(pred_1,nrow=5)
library(HH)
vif(modelLR)
error=residuals(modelLR)
hist(error,col='red')
library(car)

durbinWatsonTest(modelLR)

ME<-sum(stuTest$Mark-pred_1)/nrow(stuTest)
ME
RSS<-sum(stuTest$Mark-pred_1)^2
RSS
RMSE<-sqrt(RSS/nrow(stuTest))
RMSE
MAPE<-sum(abs(stuTest$Mark-pred_1)/stuTest$Mark)*100
MAPE
 
##liner regression compare

model_lr<-lm(Mark~Motivation+Gender+Age, data = stuTrain)
model_lr
summary(model_lr)

pred_2<-predict(model_lr,newdata = stuTest)
pred_2
#head(pred_1,nrow=5)
ME_LR<-sum(stuTest$Mark-pred_2)/nrow(stuTest)
ME_LR
RSS_LR<-sum(stuTest$Mark-pred_2)^2
RSS_LR
RMSE_LR<-sqrt(RSS_LR/nrow(stuTest))
RMSE_LR
MAPE_LR<-sum(abs(stuTest$Mark-pred_2)/stuTest$Mark)*300
MAPE_LR


###decision tree is having less RMSE value


###plot(Mark~Motivation, data = student)
From Me to Everyone:  10:17 AM
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")
From Padmaraj Khobare to Everyone:  10:18 AM
abline(lm(y~x, data =students), col = blue)


