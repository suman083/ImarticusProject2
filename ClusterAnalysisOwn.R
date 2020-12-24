data("USArrests")
str(USArrests)
df<-USArrests
dim(df)
df<-na.omit(df)
df<-scale(df)
str(df)
head(df,3)
install.packages('cluster')

install.packages("factoextra")
library(cluster)
library(factoextra)
set.seed(123)
##sample of 15 rows
ss<-sample(1:50,15)
df1<-USArrests[ss,]
head(df1, 3)
df1_scaled<-scale(df1)
head(df1_scaled)
#Euclidean Distance 
dist.euc1_15<-dist(df1_scaled,method = "euclidean")
head(dist.euc1_15,3)
round(as.matrix(dist.euc1_15)[1:3, 1:3], 1)
fviz_dist(dist.euc1_15)
dist()
fviz_nbclust(df,kmeans, method = 'wss')+geom_vline(xintercept = 4, linetype=5,col='red')
set.seed(123)
km.res<-kmeans(df,4,nstart = 25)
km.res
km.res$totss
km.res$betweenss
aggregate(USArrests,by=list(cluster=km.res$cluster),mean)
km.res
df_m<-cbind(USArrests,cluster=km.res$cluster)
head(df_m)
write.csv(df_m, 'G:/Suman/batch34/TM13122020/USArrests_m.csv')
fviz_cluster(km.res,data = df, palette= c('#2E9FDF', '#00AFBB','#E7B800', '#FC4E07'),
             ellipse.type = 'euclid', stars.plot=T,
             repel = T,
             ggtheme = theme())
