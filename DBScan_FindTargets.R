##Create cluster using DB-Scan##
library(dbscan)
library("fpc")
library (vegan)
db<-dbscan(Iris_Small_Cursor_Targets,eps=.3,minPts = 2) # create cluster
db
plot(Iris_Small_Cursor_Targets,col=db$cluster+1L)
summary(db)
hullplot(Iris_Small_Cursor_Targets,db)
#cs = cluster.stats(dist(iris[1:4]), db$cluster)


#####Global Initialization ######
vector1 <- c(5, 4, 3, 2, 1)
amp <- array(c(vector1),dim = c(1,5,1))
print(amp)
cur <- .2
vector2 <-c(3.1,3.25,3.3,3.4) # assign targets value for cluster1
targets<- array(c(vector2),dim=c(1,4,1))
targets
######End Initialization########

#call findTarget function until all target gets vanished
#while(length(targets)>0)
#{
 # findMultiTarget_DBScan()
  # delete the specific targets from the cluster's dataset
  
#}
#targets
findMultiTarget_DBScan <- function()
{
  #****need while loop until targets is NoTNULL
  # while(length(targets)>0) # until all the targets are vanished
  #{
  #x <- length(targets)
  #x
  #cur <- .2
  cur
  targets
  diff=targets-cur 
  #calculate difference between cursor from all targets
  diff
  mindiff=min(abs(diff)) # find minimum difference between cursor and target
  mindiff
  nearesttar <- which.min(mindiff)
  nearesttar
  targets[nearesttar] # nearest target position
  
  length(targets) 
  if(mindiff==0)
  {
    print("Cursor reaches to the target!! Success")
  }
  else
  {
    diffnew<-amp-mindiff
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
      targets <- targets[-nearesttar] # remove the targets as cursor reaches it
      targets #remaining targets
      
      if(length(targets) == 4) #1st iteration is corect,need to check from 2nd
      {
       OriginalDs <- read_excel("D:/Debashis/Research/BCI/Multitarget_Clustering/DataSets/Iris_Small_Cursor_Targets.xlsx")
       df1<-data.frame((OriginalDs))
       df1
       firstTar <- closeTar
       firstTar
       dfnew <-df1[!(df1$Targets.Sepal.Width.== firstTar),]
       dfnew #remaining targets in dataframe
       plot(dfnew,col=db$cluster+1L)
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
       }
      #df1[!(which(df1$Targets.Sepal.Width.== closeTar)),]
      #delete the specific target from dataframe
              
    }
    
  } 
  
} 

## Analysis sectionI(Internal clustering criteria)
SSE_Analysis <-function()
{
  
  vector2 <-c(3.1,3.25,3.3,3.4) # assign targets value for cluster1
  targets1<- array(c(vector2),dim=c(1,4,1))
  targets1
  c1<-length(targets1)
  c1
  
  vector4 <-c(7.50,7.62,7.70,7.67,7.80,7.51,7.63,7.72) # assign targets value for cluster2
  targets2<- array(c(vector4),dim=c(1,8,1))
  targets2
  c2<-length(targets2)
  c2
  
  vector5 <-c(9.00,9.20) # assign targets value for cluster3
  targets3<- array(c(vector5),dim=c(1,2,1))
  targets3
  c3<-length(targets3)
  c3
  
  vector3 <-c(7.50,3.30,7.62,3.10,9.00,7.70,9.20,3.25,3.40,7.67,7.80,7.51,7.63,7.72)
  AllTargets<-array(c(vector3),dim=c(1,14,1))
  AllTargets
  
  m<- mean(Iris_Small_Cursor_Targets$`Targets(Sepal.Width)`)
  m
 # m <- mean(AllTargets)  #mean of all clusters
#  m
  m1 <-mean(targets) # mean of cluster1
  m1
  m2 <-mean(targets2) # mean of cluster2
  m2
  m3 <-mean(targets3) # mean of cluster3
  m3
  #**Intracluster (WSS) measurements**--here we consider only cluster1#
  
  t1<-(targets[1]-m)^2
  t1
  t2<-(targets[2]-m)^2
  t2
  t3<-(targets[3]-m)^2
  t3
  t4<-(targets[4]-m)^2
  t4
  s<- t1+t2+t3+t4
  s
  #squaredErr <- (((targets[1]-m)^2) + ((targets[2]-m)^2) + ((targets[3]-m)^2) + ((targets[4]-m)))
  #squaredErr
  WSS<- s  #cohesion(internal cluster quality:using SSE method)
  WSS
  print(paste0("Cohesion of Cluster1 using DBScan algorithm is :", WSS))
  
  #***Intercluster(BSS) measurement(for 3 clusters)-->Seperation**#
  BSS<-((c1*(m-m1)^2) + (c2*(m-m2)^2) + (c3*(m-m3)^2))
  BSS1 <- c1*((m-m1)^2)
  BSS1
  BSS2 <- c2*((m-m2)^2)
  BSS2
  BSS3 <- c3*((m-m3)^2)
  BSS3
  BSS <- BSS1+BSS2+BSS3
  BSS
  print(paste0("Seperation between the clusters using DBScan algorithm is :", BSS))
  
}


