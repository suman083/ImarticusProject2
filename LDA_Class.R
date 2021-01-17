library(tidyverse)
library(caret)
install.packages("MASS")
library(MASS)
theme_set(theme_classic())
iris100_2 <- read.csv("G:/Suman/batch34/TimeSeries/iris100_2.csv", stringsAsFactors=TRUE)
str(iris100_2)
dim(iris100_2)
dim(iris)
str(iris)
set.seed(123)
training.samples<-iris100_2$Species %>% createDataPartition(p=0.8,list=FALSE)
train.data<- iris100_2[training.samples,]
test.data<-iris100_2[-training.samples,]

preproc.param<-train.data %>% preProcess(method=c("center","scale"))
preproc.param
train.transformed<- preproc.param %>% predict((train.data))
test.transformed <- preproc.param %>% predict(test.data)
model<- lda(Species~., data=train.transformed)
model

predictions <- model %>% predict(test.transformed)
predictions

#Group mean()

y_predict=predictions$class
y_predict
y_obs=test.data$Species
y_obs
library(gmodels)
CrossTable(y_obs,y_predict)
mean(predictions$class==train.transformed$Species)
y=seq(-2.925,3,0.975)
length(y)
head(y,10)
lda.data <- cbind(train.transformed, predict(model)$x)
ggplot(lda.data, aes(LD1, y))+
  geom_point(aes(color=Species))

ggplot(lda.data, aes(LD1,y))+
  geom_point(aes(color = Species))

lda.data <- cbind(train.transformed, predict(model)$x)
   
######iris
data(iris)



library(tidyverse)
library(caret)
library(MASS)
library(gmodels)
theme_set(theme_classic())
iris100_2 <- read.csv("E:/Data Science/Linear Discriminant Analysis/iris100_2.csv", stringsAsFactors=TRUE)
#View(iris100_2)
iris<-iris100_2
dim(iris)
str(iris)
#Split the data into 80 and 20%
set.seed(123)
training.samples<-iris$Species %>%
  createDataPartition(p=0.8,list = FALSE)
train.data<-iris[training.samples,]
test.data<-iris[-training.samples,]
#Estimate pre-processing parameter
#Scaling of data
preproc.param<-train.data %>%
  preProcess(method=c('center','scale'))
preproc.param
train.transformed<-preproc.param %>% predict(train.data)
test.transformed<-preproc.param %>% predict(test.data)
model<-lda(Species~.,data = train.transformed)
model
#Make Predictions
predictions<-model %>% predict(test.transformed)
predictions
#Confusion Matrix
y_predict<-predictions$class
y_predict
y_obs<-test.data$Species
y_obs
CrossTable(y_obs,y_predict)
#Model Accuracy
mean(predictions$class==test.transformed$Species)
       y<-seq(-2.925,3,0.075)
       length(y)
       head(y,10)
       lda.data<-cbind(train.transformed,predict(model)$x)
       ggplot(lda.data,aes(LD1,y)) +
         geom_point(aes(color=Species))
       
       