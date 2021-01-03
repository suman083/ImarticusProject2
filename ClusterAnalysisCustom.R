setwd("G:/Suman/batch34/TM13122020/cls")

getwd()
library(tm)
docs<- Coupus(DirSource("G:/Suman/batch34/TM13122020/cls"))
docs
docs<-Corpus(DirSource("G:/Suman/batch34/TM13122020/cls"))
docs
length(docs)

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
