cereals_data <- read.csv("G:/Suman/batch34/cluster analysis exercise/cereals_data.csv", row.names=1, stringsAsFactors=TRUE)
df<-cereals_data[,c(4,5,6,7)]
df
library(cluster)
library(factoextra)
dim(df)
df<-na.omit(df)
df<-scale(df)
df

#samplew of 15 rows
ss<-sample(1:77,15)
df1<-df[ss,]
df1
df1.scaled<-scale(df1)
df1.scaled

#Euclidean Distance 
dist.euc1_16<-dist(df1.scaled,method = 'euclidean')
dist.euc1_16
round(as.matrix(dist.euc1_16)[1:3, 1:3], 1)
dist.euc1_16

fviz_dist(dist.euc1_16)
fviz_nbclust(df,kmeans, method = 'wss')+geom_vline(xintercept = 5, linetype=5,col='red')
set.seed(123)
km.res<-kmeans(df,5,nstart = 25)
km.res
km.res$totss
km.res$betweenss


###

fviz_cluster(km.res,data = df, palette= c('#2E9FDF', '#00AFBB','#E7B800', '#FC4E07','#999999'),
             ellipse.type = 'euclid', stars.plot=T,
             repel = T,
             ggtheme = theme(),main = 'Cluster Analysis')
?palette
palette.pals()
palette.colors()

##Try more
res.dist<-dist(df,method = 'euclidean')
head(res.dist)
#round(as.matrix(res.dist)[1:3,1:3],1)
##Hierarchical Clustering: (Agglomeration) Linkage Methods
fviz_dist(res.dist)
res.hc<-hclust(d=res.dist, method = 'ward.D2')
fviz_dend(res.hc,cex = 0.5)
fviz_dend(res.hc,k=4,cex=0.5,k_colors =c("#000000","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7","#999999"),
          color_labels_by_k = T,
          rect = T)
