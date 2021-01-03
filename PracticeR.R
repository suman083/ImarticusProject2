x<- 'happy learning'
x
x/7
##rep
sum(2,3,5)
rep("suman happy always", 4)
sqrt(64)
rep(99,5)
##vectors
q<-c('a','b','c')
q
g<-5:10
g
wc<-c('Suman','suman1','suman2')
wc
h<-c(2:9)
h
i<-seq(1:8)
i

##increament 
j<-seq(1,10,0.3)
j
k<-seq(10,1,-.5)
k
learn<-c('you','me','r')
learn
learn[1]
learn[3]
##replacing subset value
learn[3]<-'java'
learn
#add element 
learn[4]<-'SPSS'
learn
a<-c(1,2,3)
a
a+1
b<-c(4,5,6)
b
a+b
a*b
a-b
a/b
a==c(1,99,3)
a==b
m<-c(1,3,NA,7,9)
m
sum(m)
sum(m,na.rm = T)

##create matrix
matrix(0,3,4)


##storing 1-8 in vector
sijo<-1:8
sijo
s<-matrix(sijo,c(2,4))
s
###matrix access 
#2nd row all column 
s[2,]
#4th column
s[,4]
s[1,1]
s[2,3]

#create vector /variable
names<-c('joel','chris','jule','mary','sprina')
names
percent<-c(85,88,95,92,89)
percent
lunch<-c('biryani','chicken kabab','biryani','checken kabah','veg pulav')
lunch
yummy<-as.factor(lunch)
yummy
str(yummy)
joy<-data.frame(names,percent,yummy)
joy
joy$names
joy$percent
joy$yummy
joy[[3]]
joy[[1]]
joy[1,]
joy[,1]
joy[["yummy"]]

infantry <- read.csv("G:/Suman/batch34/basicsofr/infantry.csv", stringsAsFactors=TRUE)
targets <- read.csv("G:/Suman/batch34/basicsofr/targets.csv", stringsAsFactors=TRUE)
steve<-merge(x=infantry,y=targets)
steve

###
cs2m <- read.csv("G:/Suman/batch34/cs2m.csv", stringsAsFactors=TRUE)

dim(cs2m)
str(cs2m)
summary(cs2m)
cs2m$Prgnt<-as.factor(cs2m$DrugR)
str(cs2m)
cs2m$AnxtyLH<-as.factor(cs2m$AnxtyLH)
str(cs2m)
summary(cs2m)
hist(cs2m$Age,main='Histogra of age' ,col = 'blue',xlab = 'Age',ylab = 'Frequency')

##psych
library(psych)
describe(cs2m$Age)
boxplot(cs2m$Age,horizontal = T)
B=c(210,22,20.5,22,23,24.5,24.7,25,31,27,26.5,25.3)
B
hist(B)
boxplot(B)
par(mfrow=c(3,1))
library(RColorBrewer)
brewer.pal.info
barplot(table(cs2m$AnxtyLH),col = brewer.pal(2,'BrBG'))
pairs.panels(cs2m[,c(1,2,3)])
