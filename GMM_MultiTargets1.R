library(mclust)
library(readxl)
gmm <- Mclust(Iris_CursorTargets_Dist,G=3)
summary(gmm)
plot(gmm,what="classification")
#mod1<- densityMclust(Iris_CursorTargets_Dist)
#summary(mod1)
#plot(mod1,what="BIC")
##### Global Initialization ######
vector1 <- c(5, 4, 3, 2, 1)
amp <- array(c(vector1),dim = c(1,5,1))
print(amp)
cur <- .2
vector2 <-c(3.1,3.3,4.2,5) # assign targets value for cluster1
targets<- array(c(vector2),dim=c(1,4,1))
targets
##### End Initialization ########
findMultiTarget_GMM <- function()
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
        gmm1 <- Mclust(dfnew,G=3)
        plot(gmm1,what="classification")
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
        gmm2 <- Mclust(dfnew1,G=3)
        plot(gmm2,what="classification")
        #plot(dfnew1,col=db$cluster+1L)
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
        gmm3 <- Mclust(dfnew2,G=3)
        plot(gmm3,what="classification")
        summary(gmm3)
        #plot(dfnew2,col=db$cluster+1L)
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
        gmm4 <- Mclust(dfnew3,G=3)
        plot(gmm4,what="classification")
        
        #plot(dfnew3,col=db$cluster+1L)
        targets <- targets[-nearesttar] # remove the targets as cursor reaches it
        targets #remaining targets
        
      }
    }
  }
}