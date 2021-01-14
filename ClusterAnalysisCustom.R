setwd("G:/Suman/batch34/TM13122020/cls")

getwd()
library(tm)
docs<- Coupus(DirSource("G:/Suman/batch34/TM13122020/cls"))
docs
docs<-Corpus(DirSource("G:/Suman/batch34/TM13122020/cls"))
docs
length(docs)
##Sentiment analysis nd word cloud
budget20<-readLines("bh1.txt")
##paste funstion is used to paste function and make it chunk
budget20_clean<-paste(budget20,collapse = " ")
budget20_clean
budget20_clean1<-tolower(budget20_clean)
budget20_clean1

#cleaning the punctuations , pattern = "\\w"
budget20_clean2<-gsub(pattern = "\\w",replacement = " ",budget20_clean1)

head(budget20_clean2)
##remove digits, pattern="\\d"
budget20_clean3<-gsub(pattern = "\\d", replace= " ",budget20_clean2)
budget20_clean3
library('tm')
budget20_clean4<-removeWords(budget20_clean3,stopwords())

csw<-read.csv("csw.csv",header = F)
csw
class(csw)
CSW1<-as.character(csw$V1)
CSW1
mySW<-c(CSW1,stopwords())
mySW

budget20_clean4<-removeWords(budget20_clean3,mySW)



head(budget20_clean4)
budget20_clean5<-gsub(pattern = "\\b[A-z]\\b{1}", replace=" ",budget20_clean4)
head(budget20_clean5)
budget20_clean6<-stripWhitespace(budget20_clean5)
head(budget20_clean6)
budget20_clean7<-strsplit(budget20_clean6," ")
head(budget20_clean7)
#word_freq1<-table(budget20_clean7)

#budget20_clean7<-tm_map(budget20_clean7,removeWords,mySW)

word_freq1<-table(budget20_clean7)

head(word_freq1)

word_freq2<-cbind(names(word_freq1),as.integer(word_freq1))
head(word_freq2)
write.csv(word_freq2,"Word frequency6.csv")
library(RColorBrewer)
library(wordcloud)
word_cloud<-unlist(budget20_clean7)
wordcloud(word_cloud,colors = brewer.pal(10,"BrBG"),min.freq = 5, random.order = F, scale = c(3,.7))
#wordcloud(word_cloud,random.color = T,min.freq = 5, random.order = F)


#brewer.pal.info

char<-as.character(docs[[4]])
char
writeLines(char)
##View(G:/Suman/batch34/TM13122020/cls/bh1(1).txt)
writeLines(as.character(docs[[2]]))
#writeLines(as.character(docs[[3]]))

#document term matrix shows sparsity 40%, 3289/2210 max term length is 22
dtm<- DocumentTermMatrix(docs)
dtm
#inspect dtm with 3 rows
inspect(dtm[1:3, 100:105])
m<- as.matrix(dtm)
write.csv(m, file = "3docs2.csv")

distmatrix<- dist(m, method = "euclidean")
distmatrix

groups<- hclust(distmatrix, method = "ward.D")
plot(groups, cex=1.2, hang = 1) # Cex will change the font size

rect.hclust(groups, k=2)


##x <- paste(month.name, "is", c("Snowy", "Flowy", "Blowy", "Showery", "Flowery", "Bowery", "Hoppy", "Croppy", "Droppy", "Breezy", "Sneezy", "Freezy"))
#writeLines(x)
