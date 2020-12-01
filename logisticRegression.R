cs2m <- read.csv("G:/Suman/batch34/cs2m.csv", stringsAsFactors=TRUE)

model<-glm(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH,data = cs2m,family = binomial())
model
predict1<-predict(model,type = "response")
head(predict1,3)
cs2m$predict<-predict1
cs2m$predictround<-round(predict1,digits = 0)
table(cs2m$DrugR,predict1>0.5)


creditset <- read.csv("G:/Suman/batch34/logisticregression/creditset.csv", stringsAsFactors=TRUE)
model21<-glm(default10yr~income+age+loan+LTI,data = creditset,family = binomial())
model21
predict2<-predict(model21,type = "response")
head(predict2,3)
creditset$predict<-predict2
creditset$predictround<-round(predict2,digits = 0)
table(creditset$default10yr,predict2>0.5)
table(creditset$default10yr,predict2>0.033)

