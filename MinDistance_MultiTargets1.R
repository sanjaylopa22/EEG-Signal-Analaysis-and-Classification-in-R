##Cursor movement in MultiTargets BCI using MinimumDistance clustering##
library(cluster)
library(readxl)
library (vegan)
library(flexclust)
library(clusteval)
library(factoextra)
library(fpc)
library(clValid)
library(stats)
data(Iris_CursorTargets_Dist,package="cluster")
km<-kmeans(Iris_CursorTargets_Dist,centers =3,nstart = 5 )
km
#distance based clustering is "kmeans".Only here no change of centroids 
plot(Iris_CursorTargets_Dist,col=km$cluster)
summary(km$cluster)

##### Global Initialization ######
vector1 <- c(5, 4, 3, 2, 1)
amp <- array(c(vector1),dim = c(1,5,1))
print(amp)
cur <- .2
vector2 <-c(3.1,3.3,4.2,5) # assign targets value for cluster1
targets<- array(c(vector2),dim=c(1,4,1))
targets
##### End Initialization ########
findMultiTarget_MinDistance <- function()
{
 targets
 diff=targets-cur 
  #calculate difference between cursor from all targets
 diff
 mindiff=min(abs(diff)) # find minimum difference between cursor and target
 mindiff
 nearesttar <- which.min(mindiff) #find nearest target from cursor
 nearesttar
 targets[nearesttar]  #location and value of the target
 if(mindiff==0)
 {
   print("Cursor reaches to the target!! Success")
 }
 else
 {
   diffnew<-amp-mindiff  #which amplitude is best suited for difference
   diffnew
   minshift <-min(abs(diffnew))
   minindex <-which.min(abs(diffnew)) #return index of minimum shift
   minindex
   curnew = amp[minindex]+cur # cursor moved to the target
   curnew  # new position of cursor
   cur <-curnew
   cur
   closeTar <- targets[nearesttar]
   closeTar
   if(cur>=targets[nearesttar])
   {
     print("Success")
     if(length(targets) == 4) #1st iteration is corect,need to check from 2nd
     {
       OriginalDs <- read_excel("D:/Debashis/Research/BCI/Multitarget_Clustering/DataSets/Iris_CursorTargets_Dist.xlsx")
       df1<-data.frame((OriginalDs))
       df1
       firstTar <- closeTar
       firstTar
       dfnew <-df1[!(df1$Targets.Sepal.Width.== firstTar),]
       dfnew #remaining targets in dataframe
       plot(dfnew,col=db$cluster+1L)
       targets <- targets[-nearesttar] # remove the targets as cursor reaches it
       targets #remaining targets
       
     } 
     length(targets)
     else if(length(targets)==3)
     {
       firstTar
       secTar <- closeTar
       secTar
       dfnew <-df1[!(df1$Targets.Sepal.Width.== firstTar),]
       dfnew
       dfnew1 <-dfnew[!(dfnew$Targets.Sepal.Width.== secTar),]  
       dfnew1
       plot(dfnew1,col=db$cluster+1L)
       targets <- targets[-nearesttar] # remove the targets as cursor reaches it
       targets #remaining targets
       
     }
     else if(length(targets)==2)
     {
       firstTar
       secTar
       thrdTar <- closeTar
       thrdTar
       dfnew <-df1[!(df1$Targets.Sepal.Width.== firstTar),]
       dfnew
       dfnew1 <-dfnew[!(dfnew$Targets.Sepal.Width.== secTar),]  
       dfnew1
       dfnew2 <-dfnew1[!(dfnew1$Targets.Sepal.Width.== thrdTar),]  
       dfnew2
       plot(dfnew2,col=db$cluster+1L)
       targets <- targets[-nearesttar] # remove the targets as cursor reaches it
       targets #remaining targets
       
     }
     else if(length(targets)==1)
     {
       firstTar
       secTar
       thrdTar
       fourthTar <- closeTar
       fourthTar
       dfnew <-df1[!(df1$Targets.Sepal.Width.== firstTar),]
       dfnew
       dfnew1 <-dfnew[!(dfnew$Targets.Sepal.Width.== secTar),]  
       dfnew1
       dfnew2 <-dfnew1[!(dfnew1$Targets.Sepal.Width.== thrdTar),]  
       dfnew2 
       dfnew3 <-dfnew2[!(dfnew2$Targets.Sepal.Width.== fourthTar),]  
       dfnew3
       plot(dfnew3,col=db$cluster+1L)
       targets <- targets[-nearesttar] # remove the targets as cursor reaches it
       targets #remaining targets
       
     }
   }
 }
}

##Cluster Statistics ##
dd <- dist(Iris_CursorTargets_Dist, method ="euclidean")
# Statistics for k-means clustering
km_stats <- cluster.stats(dd,  km$cluster)
# (k-means) within clusters sum of squares
km_stats$within.cluster.ss


km_stats$diameter
km_stats  #All statistics of clustering using k-means


###***(Internal cluster evolution)***##
###1.Cohesion(SSE)--check from DB-Scan file
###2.Silhoutte criteria##
data(Iris_CursorTargets_Dist,package="cluster")
dis = dist(Iris_CursorTargets_Dist$`Targets(Sepal.Width)`)^2
res = kmeans(Iris_CursorTargets_Dist,3)
sil = silhouette (res$cluster, dis)

#plot(sil)
fviz_silhouette(sil)
names(sil)
summary(sil)

##3.Dunn Index#
km
Dist <- dist(Iris_CursorTargets_Dist,method = "euclidian")
dunn(Dist,km$cluster)
##**End of Internal validation*****##

##******External validation*******##
##1.RandIndex###
kdt <- table(Iris_CursorTargets_Dist$`Targets(Sepal.Width)`,km$cluster)
sam1<- sample(kdt[1:6,])
sam1
sam2 <- sample(kdt[7:12,])
sam2
dt<- table(sam1,sam2)
randIndex(dt)
summary(dt)
#**Optimal RandIndex =.09 after some iteration(-.07,-.04,-.06,08,.09,0.02645503)
##2 Jaccard Index(based on similarity)
cluster_similarity(sam1,sam2,similarity = "jaccard",method = "independence")
###****End of criteria*****##

