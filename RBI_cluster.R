RBI <- read.csv("G:/Suman/batch34/cluster analysis exercise/RBI1.csv", stringsAsFactors=TRUE)
str(RBI)

dim(RBI)
row.names(RBI)=RBI$States_Union
RBI=RBI[, -1]
str(RBI)
RBI 
ddf<-RBI
#ddf<-ddf[-c(8, 9, 11, 27), ]


ddf<-scale(RBI)
ddf<-na.omit(ddf)
str(ddf)
head(ddf,3)
#ddf_f<-c(RBI[,1],ddf[,])
#ddf_f$States_Union<-RBI$States_Union
#ddf_f
#str(ddf_f)
#install.packages('cluster')
library(cluster)
library(factoextra)

#######

##sample of 10 rows
ss<-sample(1:30,10)
ddf1<-RBI[ss,]
head(ddf1, 3)
ddf1_scaled<-scale(ddf1)
head(ddf1_scaled)
#Euclidean Distance 
dist.euc1_10<-dist(ddf1_scaled,method = "euclidean")
head(dist.euc1_10,3)
round(as.matrix(dist.euc1_10)[1:3, 1:3], 1)
fviz_dist(dist.euc1_10)
dist()
write.csv(df_m,'G:/Suman/batch34/project2/df_m')



fviz_nbclust(ddf,kmeans,method='wss')+geom_vline(xintercept =4, linetype=5,col='red' )
set.seed(123)
km.res<-kmeans(ddf,4,nstart = 25)
km.res
df_m<-cbind(RBI,cluster=km.res$cluster)
km.res$totss




fviz_cluster(km.res,data = ddf, palette= c('#2E9FDF', '#00AFBB','#E7B800', '#FC4E07','blue'),
             ellipse.type = 'euclid', stars.plot=T,
             repel = T,
             ggtheme = theme())

