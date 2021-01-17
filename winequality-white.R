# wine <- read.csv("G:/Suman/batch34/projectofdecisiontreeandlda/winequality-white.csv", sep=";", stringsAsFactors=TRUE)
# wine$quality<-as.factor(wine$quality)
# dim(wine)
# str(wine)
# library(tidyverse)
# library(tidyverse)
# library(caret)
# #install.packages("MASS")
# library(MASS)
# library(gmodels)
# 
# set.seed(123)
# training.samples<- wine$quality %>% createDataPartition(p=0.8,list = FALSE)
# train.data<- wine[training.samples, ]
# test.data<- wine[-training.samples, ]
# preproc.param<- train.data %>% preProcess(method=c("center","scale"))
# preproc.param
# 
# train.transformed<-preproc.param %>% predict(train.data)
# test.transformed<- preproc.param %>% predict(test.data)
# 
# library(MASS)
# model14<- lda(quality~., data = train.transformed)
# model14
# 
# ##grp mean
# 
# predictions14<-model14 %>% predict(test.transformed)
# predictions14
# 
# #probablity LD SCORE
# 
# # Confusion matrix
# y_predict14=predictions14$class
# y_predict14
# 
# #Confusion matrix 
# y_obs14= test.data$quality
# y_obs14
# 
# library(gmodels)
# CrossTable(y_obs14,y_predict14)
# 
# 
# ###we need to check the variables in cross table
# ##Model accuracy 
# mean(predictions14$class==test.transformed$quality)
# ## slection of level
# y=seq(-2.925,3,0.075)
# length(y)
# head(y,10)
# lda.data<-cbind(train.transformed,predict(model14)$x)
# ggplot(lda.data,aes(LD1,LD2,LD3,y))+ 
#   goem_point(aes(color= quality))
# 
# y= seq(-2.925, 3, 0.075)
# length(y)
# head(y, 10)
# lda.data<- cbind(train.transformed, predict(model14)$x)
# ggplot(lda.data, aes(LD1,LD2, LD3, LD4,LD5, LD6 ,y))+geom_point(aes(color = quality))


####

##data("iris")
wine1<-read.csv("G:/Suman/batch34/projectofdecisiontreeandlda/winequality-white.csv", sep=";", stringsAsFactors=TRUE)
wine1$quality<-as.factor(wine1$quality)
str(wine1)
library(psych)
#pairs.panels(wine1[1:7])
set.seed(555)
ind<-sample(2,nrow(wine1), replace = TRUE,prob = c(0.6,0.4))
training<-wine1[ind==1,]
testingg<- wine1[ind==2, ]

##linear discriment analysis

library(MASS)
linear<-lda(quality~., data=training)
attributes(linear)
linear$prior
linear$counts
##histograms
p<-predict(linear,training)
p

ldahist(data = p$x[ ,1], g=training$quality)
ldahist(data = p$x[ ,2], g=training$quality)
ldahist(data = p$x[ ,3], g=training$quality)
ldahist(data = p$x[ ,4], g=training$quality)
ldahist(data = p$x[ ,5], g=training$quality)
ldahist(data = p$x[ ,6], g=training$quality)

###
p<-predict(linear,testingg)
p

ldahist(data = p$x[1:20,1], g=testingg$quality)
ldahist(data = p$x[ ,2], g=training$quality)
ldahist(data = p$x[ ,3], g=training$quality)
ldahist(data = p$x[ ,4], g=training$quality)
ldahist(data = p$x[ ,5], g=training$quality)
ldahist(data = p$x[ ,6], g=training$quality)

##bi-plot
library(devtools)
##install_github("fawda123/ggord",force = TRUE)
library(ggord)
ggord(linear, training$quality)

##Partition plot
#install.packages("klaR")
library(klaR)
partimat(quality~., data=training, method="lda")
partimat(quality~., data=training, method="qda")


###Confusion matrix and accurecy 
p1<-predict(linear,training)$class
p1
tab<-table(Predicted=p1, Actuav=training$quality)
tab
sum(diag(tab)/sum(tab))

p2<-predict(linear, testingg)$class
p2
tab1<-table(Predicted=p2, Actual=testingg$quality)
tab1
sum(diag(tab1)/sum(tab1))

