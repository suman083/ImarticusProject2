setwd("G:/Suman/batch34/TM13122020/csw")

getwd()
library(tm)
data<-readLines("file.txt")
data
text<-VectorSource(data)
text
text1<- VCorpus(text)
text1
csw<-read.csv("csw.csv",header = F)
csw
class(csw)
csw1<-as.character(csw$V1)
csw1
class(csw1)
mySw<-c(csw1,stopwords())
mySw
text2<-tm_map(text1,content_transformer(tolower))
text2[[1]]$content
text3<- tm_map(text2,removeWords,mySw)
text3[[1]]$content
text4<-tm_map(text3,stripWhitespace)
text4[[1]]$content
