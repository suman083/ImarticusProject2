setwd("G:/Suman/batch34/TM13122020")
readLines("Foodc.txt")
a<-c("Hello", "word", "my","first","Handsaome","Through", "x")
b<-paste(a,collapse = " ")
b
chunk_2<-readLines("Foodc.txt")
chunk_pasted_2<-paste(chunk_2,collapse =" ")
head(chunk_pasted_2)
clean_data1<-tolower(chunk_pasted_2)
head(clean_data1)
#punchuations
?gsub
clean_data2 <- gsub(pattern = "\\W", replace=" ", clean_data1)
#head(clean_data2)

##Cleaning digit pattern ="\\d"
clean_data3<-gsub(pattern="\\d",replace=" ",clean_data2)
#head(clean_data3)

#cleaning stop words
#install.packages('tm')
?tm
library('tm')
stopwords()
stopwords("russian")
?stopwords()
#remove stop Words
clean_data4<-removeWords(clean_data3,stopwords(en))
head(clean_data4)
clean_wd<-removeWords(clean_data4,c("agrifood","intoxication"))
head(clean_wd)
clean_data5 <- gsub(pattern = "b[A-z]\\b{1}",replace=" ", clean_data4)
clean_data5

##frequency of the word###
#we now have  a chunk of lines and we are looking for counting the words 
#if you remember we had joined variouslines and made a chunk 
#so we split individual words and add a space beetween them as splitter 

#clean_data7<- strsplit(clean_data6," ")
#head(clean_data7)

clean_data6 <- stripWhitespace(clean_data5)
clean_data7<-strsplit(clean_data6," ")
head(clean_data7)

##frequency of words
word_freq1<-table(clean_data7)
head(word_freq1,10)
word_freq2<- cbind(names(word_freq1),as.integer(word_freq1))
head(word_freq2,10)
write.csv(word_freq2,"Word Frequency5.csv")

#World clud ####
##install.packages("RColorBrewer")

library(RColorBrewer)
library(wordcloud)
class(clean_data7)
word_cloud1<-unlist(clean_data7)
word_cloud1
wordcloud(word_cloud1)
wordcloud(word_cloud1,min.freq = 3,scale =c(4,.3))
wordcloud(word_cloud1, min.freq = 10,random.order = F,scale =c(5,.7),colors = brewer.pal(15,"Dark2"))
install.packages("wordcloud2")
library(wordcloud2)
wc2<-data.frame(word_cloud1)
colnames(wc2)<-c('word','fred')
wc2
#wordcloud2(wc2, shape = "pentagon")
wordcloud2(demoFreq,color = brewer.pal(15,"BrBG"), shape = "diamond")

brewer.pal.info
##abc<-as.factor(demoFreq)
wordcloud2(abc)
head(demoFreq)

letterCloud(demoFreq,color = brewer.pal(8,"BrBG"), word = "R")

positive<-scan("positive.txt",what = "character",comment.char = ";")
negative<-scan("negative.txt",what = "character",comment.char = ";")
senti_analysis<-unlist(clean_data7)
match(senti_analysis,positive)
match(senti_analysis,negative)
p_score<-sum(!is.na(match(senti_analysis,positive)))
p_score
n_score<-sum(!is.na(match(senti_analysis,negative)))
n_score
Sentiment_score=p_score-n_score
Sentiment_score
