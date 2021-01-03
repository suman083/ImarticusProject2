student1 <- read.csv("G:/Suman/batch34/student1.csv", stringsAsFactors=TRUE)
install.packages("nnet")
library(nnet)

library(caTools)
library(tree)
set.seed(1)
split<-sample.split(student1$Grade,SplitRatio = 0.70)
studentTrain1<-subset(student1,split==TRUE)
studentTest1<-subset(student1,split==FALSE)
table(student1$Grade)
table(studentTrain1$Grade)
table(studentTest1$Grade)
prop.table(table(studentTest1$Grade))
table(studentTest1$Grade)
modelclassTree<-tree(Grade ~ Motivation+Age+Gender, data = student1)
plot(modelclassTree)
text(modelclassTree,pretty = 0,cex=0.75)
pred_cl<-predict(modelclassTree,newdata = studentTest1,type="class")
pred_cl
conf<-table(studentTest1$Grade,pred_cl)
conf<-table(studentTest1$Grade,pred_cl)
conf
OAA<-(conf[1,1]+conf[2,2]+conf[3,3]+conf[4,4]+conf[5,5]+conf[6,6])/sum(conf)
OAA

####Logistic reression

model21<-multinom(Grade ~ Motivation+Age+Gender, data = studentTest1)
summary(model21)

###Accurecy 
###prediction
predict(model21,student1)

predict(model21,student1[c(1,10,15),],type = 'prob')
#Accuracy of model

cm<-table(predict(model21),student1$Grade)
print(cm)

1-sum(diag(cm))/sum(cm)  ##wrong prediction

#correct prediction
sum(diag(cm))/sum(cm)


##two tail z test 

predict(model21,studentTest1)

predict(model21,studentTest1[c(1,10,15),],type = 'prob')
#Accuracy of model

cm<-table(predict(model21),studentTest1$Grade)
print(cm)

1-sum(diag(cm))/sum(cm)  ##wrong prediction

#correct prediction
sum(diag(cm))/sum(cm)

