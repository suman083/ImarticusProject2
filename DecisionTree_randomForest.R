#install.packages('ISLR')
library(ISLR)
attach(Carseats)
dim(Carseats)
write.csv('G:/Suman/batch34/Carseats.csv')


High<-ifelse(Sales<=8,'No','Yes')
dim(Carseats)
str(High)
#High<-as.factor(High)
Carseats<-data.frame(Carseats,High)
dim(Carseats)
names(Carseats)
library(tree)
Carseats$High <- as.factor(Carseats$High)
tree.carseats<-tree(High~. -Sales, Carseats)
summary(tree.carseats, pretty=0)

plot(tree.carseats)
text(tree.carseats)
tree.carseats
set.seed(2)
train<-sample(1:nrow((Carseats)),200)
Carseats.test<-Carseats[-train, ]
High.test<-High[-train]

tree.carseats1<-tree(High~. -Sales, Carseats,subset = train)
tree.pred<-predict(tree.carseats1,Carseats.test,type='class')
cm=table(tree.pred,High.test)
sum(diag(cm)/sum(cm))
summary(tree.carseats1)

#prune treeee
set.seed(3)
cv.carseats<-cv.tree(tree.carseats1,FUN = prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev, type = 'b', col='red', lwd=2)
plot(cv.carseats$k,cv.carseats$dev,type = 'b', col='blue',lwd=2)

prune.carseats<-prune.misclass(tree.carseats1,best = 8)
plot(prune.carseats)
summary(prune.carseats)
text(prune.carseats,pretty = 0)
tree.pred1<-predict(prune.carseats,Carseats.test,type = 'class')
summary(tree.pred1)
cm1=table(tree.pred1,High.test)
sum(diag(cm1)/sum(cm1))
(89+62)/2


##bagging 
#install.packages("randomForest")
library(randomForest)
set.seed(1)
bag.carseats<-randomForest(High~. -Sales,Carseats,subset = train,mtry=10,importance=TRUE)
dim(Carseats)
str(Carseats)
importance(bag.carseats)
varImpPlot(bag.carseats,col='red',pch=10,cex=1.25)
bag.carseats
test.pred.beg<-predict(bag.carseats,newdata = Carseats[-train, ], ttype = 'class')
table(test.pred.beg,High.test)
sum(diag(table(test.pred.beg,High.test))/sum(table(test.pred.beg,High.test)))
(104+61)/2


###random forest 

set.seed(1)
rf.carseats<-randomForest(High~. -Sales, Carseats,subset = train,mtry=4,importance=TRUE)
dim(Carseats)
importance(rf.carseats)
varImpPlot(rf.carseats,col='blue',pch=10,cex=1.25)
rf.carseats
test.pred.rf<-predict(rf.carseats,newdata = Carseats[-train, ], type='class')
table(test.pred.rf, High.test)
sum(diag(table(test.pred.rf, High.test))/sum(table(test.pred.rf, High.test)))

