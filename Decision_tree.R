student <- read.csv("G:/Suman/batch34/student.csv")
names(student)
nrow(student)
ncol(student)
str(student)

select_row<-sample(1:nrow(student),round(0.2*nrow(student)),
                   replace = F)
select_row
stuTest<-student[select_row,]
stuTrain<-student[-(select_row),]
stuTrain
#install.packages("tree")
library(tree)
modelRegTree<- tree(Mark~ Motivation + Gender + Age, data=stuTrain)
plot(modelRegTree)
text(modelRegTree,pretty = 4,cex=0.75)
pred<-predict(modelRegTree,newdata = stuTest)
head(pred,nrow=5)
head(pred,n=5)
head(pred,5)
ME<-sum(stuTest$Mark-pred)/nrow(stuTest)
ME
rss<-sum(stuTest$Mark-pred)^2
rmse<-sqrt(rss/nrow(stuTest))
rmse
mape<-sum(abs(stuTest$Mark-pred)/stuTest$Mark)*100
mape/
  View(studentTest)


##Classification
#install.packages("caTools")
library(caTools)
install.packages("tree")
library(tree)
set.seed(1)

split<-sample.split(student,SplitRatio = 0.7)
split
studentTrain<-subset(student,split=="TRUE")
studentTest<-subset(student,split=="FALSE")
#test<-subset(PPT,split="FALSE")
studentTrain
studentTest
table(student$Grade)
table(studentTest$Grade)
table(studentTrain$Grade)
prop.table(table(studentTest$Grade))
modelclassTree<-tree(Grade ~ Motivation+Age+Gender, data=studentTrain)
plot(modelclassTree)
modelclassTree
text(modelclassTree,pretty = 2,cex=0.75)

pred<-predict(modelclassTree,newdata = studentTest,type = "class")
conf<-table(studentTest$Grade,pred)
conf
qaa<-(conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]+conf[6,6])/sum(conf)
qaa


##decision tree vs Regression logistic


