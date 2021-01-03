data("USArrests")
str(USArrests)
df<-USArrests
dim(df)
df<-na.omit(df)
df<-scale(df)
str(df)
head(df,3)
#install.packages('cluster')
library(cluster)
library(factoextra)
set.seed(123)
ss<-sample(1:50,15)
ss
df1<-USArrests[ss,]
df1.sacled<-scale(df1)
head(df1.sacled,3)
df1
dist.euc1_15<-dist(df1.sacled, method = 'euclidean')
head(dist.euc1_15,3)
round(as.matrix(dist.euc1_15)[1:3,1:3],1)
fviz_dist(dist.euc1_15)
fviz_nbclust(df,kmeans,method='wss')+geom_vline(xintercept =4, linetype=5,col='red' )
set.seed(123)
km.res<-kmeans(df,4,nstart = 25)
km.res
df_m<-cbind(USArrests,cluster=km.res$cluster)
km.res$totss

head(df_m)
View(df_m)
write.csv(df_m,'G:/Suman/batch34/project2/df_m')
fviz_cluster(km.res,data = df, palette= c('#2E9FDF', '#00AFBB','#E7B800', '#FC4E07'),
             ellipse.type = 'euclid',
             star.plot=T,
             repel = T,
             ggtheme = theme()
             )


fviz_cluster(km.res,data = df, palette= c('#2E9FDF', '#00AFBB','#E7B800', '#FC4E07'),
             ellipse.type = 'euclid', stars.plot=T,
             repel = T,
             ggtheme = theme())
