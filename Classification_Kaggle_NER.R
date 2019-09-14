#library(cluster)
library(readxl)

#library (vegan)
library(fpc)
library(MASS)
library(ggplot2)
library(scales)
library(gridExtra)
library(caret)
library(plotly)
library(ROCR)
library(plotROC)
#library(Biocomb)
library(kernlab)
library(entropy)
library(factoextra)
library(psych)
start.time <- Sys.time()
#OriginalDs <- read_excel("Kaggle_GAL_subj1_series1_data.xlsx")
OriginalDs <- read.csv("Kaggle_GAL_subj1_series1_data.csv")
summary(OriginalDs)
kp<- Kappa(OriginalDs)
Kp
View(OriginalDs)

#df<-data.frame(OriginalDs,Sp=rep(c("s","c","v","H"), rep(100,4)))
newds <- OriginalDs[ ,-c(1)] #remove Time field for classification
#newds1<- newds[ ,-c(10)] # remove Class field(FeedBackevent)
newds
r <- lda(formula = FeedBackEvent ~ ., 
         data = newds,method="t"
         )
r_predict <- predict(r)
r_predict$class
predict(r,newds)$class
test_r <-lda(formula = FeedBackEvent ~ ., 
             data = newds,CV=TRUE  ) #cross validation the LDA
test_r
###LDA (after dimension rduction:reduced set of channels:6) with 1010 data###
largeDs <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/Kaggle/Kaggle_NER_2015/Large_Subject2_training.xlsx")
largeDs
largeds1 <- largeDs[ ,-c(1)] #remove Time field for classification
#newds1<- newds[ ,-c(10)] # remove Class field(FeedBackevent)
largeds1
##LDA with all the channels (before dimension reduction)
BigDs <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/Kaggle/Kaggle_NER_2015/Large_Subject2_training_13channels.xls")
BigDs
BigDs1 <- BigDs[ ,-c(1)] #remove Time field for classification

##Small dataset for subject2
#smallDs <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/Kaggle/Kaggle_NER_2015/Subject1_training.xlsx")
smallDs <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/VeryImp_DEAP/Reduced_preprocessed_EEGdata.xlsx")
smallDs1 <- smallDs[ ,-c(1)]
smallDs1
# Dimension reduction with Correlation based Graph approach(source has 21 columns)

#plot(largeds1$Fp1,largeds1$Fp2,main="Scatterplot") #scatter plot correlation) between electrodes value of FP1,Fp2
cor(smallDs1$Fp1,smallDs1$Fp2,method="pearson")
cor(smallDs1$Fp1,smallDs1$F3,method="pearson")
cor(smallDs1$Fp1,smallDs1$F4,method="pearson")
cor(smallDs1$Fp1,smallDs1$T7,method="pearson")
cor(smallDs1$Fp1,smallDs1$T8,method="pearson")
cor(smallDs1$Fp1,smallDs1$C1,method="pearson")
cor(smallDs1$Fp1,smallDs1$Fz,method="pearson")
cor(smallDs1$Fp1,smallDs1$P3,method="pearson")
cor(smallDs1$Fp1,smallDs1$Pz,method="pearson")
cor(smallDs1$Fp1,smallDs1$PO7,method="pearson")
cor(smallDs1$Fp1,smallDs1$O1,method="pearson")
cor(smallDs1$Fp1,smallDs1$P4,method="pearson")
#--------------------FP2-------
cor(smallDs1$Fp2,smallDs1$F3,method="pearson")
cor(smallDs1$Fp2,smallDs1$F4,method="pearson")
cor(smallDs1$Fp2,smallDs1$T7,method="pearson")
cor(smallDs1$Fp1,smallDs1$T8,method="pearson")
cor(smallDs1$Fp2,smallDs1$C1,method="pearson")
cor(smallDs1$Fp2,smallDs1$Fz,method="pearson")
cor(smallDs1$Fp2,smallDs1$P3,method="pearson")
cor(smallDs1$Fp2,smallDs1$Pz,method="pearson")
cor(smallDs1$Fp2,smallDs1$PO7,method="pearson")
cor(smallDs1$Fp2,smallDs1$O1,method="pearson")
cor(smallDs1$Fp2,smallDs1$P4,method="pearson")
#----F3
cor(smallDs1$F3,smallDs1$F4,method="pearson")
cor(smallDs1$F3,smallDs1$T7,method="pearson")
cor(smallDs1$F3,smallDs1$T8,method="pearson")
cor(smallDs1$F3,smallDs1$C1,method="pearson")
cor(smallDs1$F3,smallDs1$Fz,method="pearson")
cor(smallDs1$F3,smallDs1$P3,method="pearson")
cor(smallDs1$F3,smallDs1$Pz,method="pearson")
cor(smallDs1$F3,smallDs1$PO7,method="pearson")
cor(smallDs1$F3,smallDs1$O1,method="pearson")
cor(smallDs1$F3,smallDs1$P4,method="pearson")
#--F4
cor(smallDs1$F4,smallDs1$T7,method="pearson")
cor(smallDs1$F4,smallDs1$T8,method="pearson")
cor(smallDs1$F4,smallDs1$C1,method="pearson")
cor(smallDs1$F4,smallDs1$Fz,method="pearson")
cor(smallDs1$F4,smallDs1$P3,method="pearson")
cor(smallDs1$F4,smallDs1$Pz,method="pearson")
cor(smallDs1$F4,smallDs1$PO7,method="pearson")
cor(smallDs1$F4,smallDs1$O1,method="pearson")
cor(smallDs1$F4,smallDs1$P4,method="pearson")
##---T7
cor(smallDs1$T7,smallDs1$T8,method="pearson")
cor(smallDs1$T7,smallDs1$C1,method="pearson")
cor(smallDs1$T7,smallDs1$Fz,method="pearson")
cor(smallDs1$T7,smallDs1$P3,method="pearson")
cor(smallDs1$T7,smallDs1$Pz,method="pearson")
cor(smallDs1$T7,smallDs1$PO7,method="pearson")
cor(smallDs1$T7,smallDs1$O1,method="pearson")
cor(smallDs1$T7,smallDs1$P4,method="pearson")
##T8
cor(smallDs1$T8,smallDs1$C1,method="pearson")
cor(smallDs1$T8,smallDs1$Fz,method="pearson")
cor(smallDs1$T8,smallDs1$P3,method="pearson")
cor(smallDs1$T8,smallDs1$Pz,method="pearson")
cor(smallDs1$T8,smallDs1$PO7,method="pearson")
cor(smallDs1$T8,smallDs1$O1,method="pearson")
cor(smallDs1$T8,smallDs1$P4,method="pearson")
##C1
cor(smallDs1$C1,smallDs1$Fz,method="pearson")
cor(smallDs1$C1,smallDs1$P3,method="pearson")
cor(smallDs1$C1,smallDs1$Pz,method="pearson")
cor(smallDs1$C1,smallDs1$PO7,method="pearson")
cor(smallDs1$C1,smallDs1$O1,method="pearson")
cor(smallDs1$C1,smallDs1$P4,method="pearson")
##Fz
cor(smallDs1$Fz,smallDs1$P3,method="pearson")
cor(smallDs1$Fz,smallDs1$Pz,method="pearson")
cor(smallDs1$Fz,smallDs1$PO7,method="pearson")
cor(smallDs1$Fz,smallDs1$O1,method="pearson")
cor(smallDs1$Fz,smallDs1$P4,method="pearson")
##P3
cor(smallDs1$P3,smallDs1$Pz,method="pearson")
cor(smallDs1$P3,smallDs1$PO7,method="pearson")
cor(smallDs1$P3,smallDs1$O1,method="pearson")
cor(smallDs1$P3,smallDs1$P4,method="pearson")
##Pz
cor(smallDs1$Pz,smallDs1$PO7,method="pearson")
cor(smallDs1$Pz,smallDs1$O1,method="pearson")
cor(smallDs1$Pz,smallDs1$P4,method="pearson")
##PO7
cor(smallDs1$PO7,smallDs1$O1,method="pearson")
cor(smallDs1$PO7,smallDs1$P4,method="pearson")
##O1
cor(smallDs1$O1,smallDs1$P4,method="pearson")
##calculating correlation for Graphical Dimension reduction method
entropy.Dirichlet(smallDs1$Fp1,a=1) #4.57
entropy.Dirichlet(smallDs1$Fp2,a=1) #4.59
entropy.Dirichlet(smallDs1$F3,a=1) #4.54
entropy.Dirichlet(smallDs1$F4,a=1) #6.02
entropy.Dirichlet(smallDs1$T7,a=1) #4.58
entropy.Dirichlet(smallDs1$T8,a=1) #4.58
entropy.Dirichlet(smallDs1$C1,a=1) #4.57
entropy.Dirichlet(smallDs1$Fz,a=1) #4.51
entropy.Dirichlet(smallDs1$P3,a=1) #4.58
entropy.Dirichlet(smallDs1$Pz,a=1) #4.56
entropy.Dirichlet(smallDs1$PO7,a=1) #4.59
entropy.Dirichlet(smallDs1$O1,a=1) #4.59
entropy.Dirichlet(smallDs1$P4,a=1) #4.53
#Variance for proposed method:for reduced channels
Reduced_Channels <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/VeryImp_DEAP/Reduced_preprocessed_EEGdata.xlsx", sheet = "Sheet2")
var(Reduced_Channels$FP1)
var(Reduced_Channels$F3)
var(Reduced_Channels$T7)
var(Reduced_Channels$PO3)
RPCA <- princomp(Reduced_Channels,cor="False") #for all input channels
RPCA
summary(RPCA) #97.25 (Accuracy with 1st component)

# PCA:for reduced channels
Reduced_Channels <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Datasets/VeryImp_DEAP/Reduced_preprocessed_EEGdata.xlsx", sheet = "Sheet2")
RPCA <- princomp(Reduced_Channels,cor="False") #for all input channels
RPCA
summary(RPCA) #97.25 (Accuracy with 1st component)
dev_pca<- RPCA$sdev
Rpca_vr <- RPCA$sdev^2
pro_varex<-Rpca_vr/sum(Rpca_vr)
Rpca_vr[1:10]
pro_varex[1:10]
# Start off with unrotated PCA

pc1 = psych::principal(Reduced_Channels, nfactors = 
                         length(Reduced_Channels), rotate="none")
pc1
pc1$residual[1]
pc1$

##PCA:for all input channels
PCA <- princomp(smallDs1,cor="False") #for all input channels
PCA
fviz_eig(PCA)
eig.data
#PCA$scores
summary(PCA) #97.25 (Accuracy with 1st component)
dev_pca<- PCA$sdev
pca_vr <- PCA$sdev^2
Ppro_varex<-pca_vr/sum(pca_vr)
Rpca_vr[1:10]
PCA$loadings
pca_vr[1:10]
##CFS(Correlation based Feature selector)
#Score=Cov/sqare root(Cx,Cy)
c1<-cov(largeds1$Fp1,largeds1$Fp2)
#select.cfs
##KernelPCA
kpc <- kpca(~., data=Reduced_Channels,
            kernel = "rbfdot",  # Gaussian Radial Basis kernel function
            kpar   = list(sigma=0.2),
            features=4)
kpc
kpc@pcv
kpc@eig # comp1 has highest eigen value so it has the principal components and largest accuarcy
kpc@kcall
head( pcv(kpc) )  # print the principal component vectors
emb <- predict(kpc,smallDs1)
summary(emb)
# Analysis of classification
# boxplot of channels value
boxplot(largeds1,ylab="Channels Value",xlab="Channels",col=c("red","green","yellow","gray","pink","sienna","royalblue2"),notch = TRUE)

#Execution time of LDA classification
ldadt <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Analysis/LDA_differentSamples.xlsx", sheet = "Barchart_Classification")
lda_emotion <-table(ldadt$Subjects,ldadt$`Execution Time`)
counts <- table(ldadt$Subjects,ldadt$`Execution Time`)
barplot(lda_emotion,main="Execution time of classification for different subjects",beside= TRUE,angle = 15+5*1:5,density = 20, xlab="Subjects",ylab = "Execution Time",col=NULL,legend = rownames(counts))
#Execution time of State transition machine
transdt <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Analysis/LDA_differentSamples.xlsx", sheet = "Barchart_TransitionMachine")
trans_emotion <-table(transdt$Subjects,transdt$`Execution Time`)
counts <- table(transdt$Subjects,transdt$`Execution Time`)
barplot(trans_emotion,main="Execution time of State transition machine for different subjects",beside= TRUE,angle = 15+5*1:5,density = 50, xlab="Subjects",ylab = "Execution Time",col=NULL,legend = rownames(counts))
#Comparison analysis with previous model
compdt <- read_excel("F:/Debashis/Research/BCI/EmotionDetection/Analysis/LDA_differentSamples.xlsx", sheet = "Comparison_Study")
compdt
com_prevdt <-table(compdt$`Property of study`,compdt$Study)
counts <- table(compdt$`Property of study`,compdt$Study)
barplot(com_prevdt,main="Comparison study of classification methods",beside= TRUE,angle = 15+5*1:5,density = 50, xlab="Property of study",ylab = "Study",col=NULL,legend = rownames(counts))


r1 <- lda(formula = FeedBackEvent ~ ., 
         data = largeds1
)
r1
r2 <- lda(formula = FeedBackEvent ~ ., 
          data = BigDs1
)
r2


pred1<-predict(r1,largeds1)
pred1

#Accuracy & confusionMatrix(largeds1,pred1)--with large samples (after dim reduction)
test_r1 <-lda(formula = FeedBackEvent ~ ., 
             data = largeds1,CV=TRUE  ) #cross validation the LDA
test_r1
predtb <-table(predict(r1,largeds1)$class, largeds1$FeedBackEvent)
predtb
ct <- table(largeds1$FeedBackEvent, test_r1$class)
#diag(prop.table(ct, 1))
# total percent correct
confusionMatrix(ct)
-----------------------------------
#Accuracy & confusionMatrix(BigDS1,pred1)--with large samples (after dim reduction)
BigDs1
test_r3 <-lda(formula = FeedBackEvent ~ ., 
                data = BigDs1,CV=TRUE ) #cross validation the LDA
#test_r3
predtb_BIG <-table(predict(r2,BigDs1)$class, BigDs1$FeedBackEvent)
predtb_BIG
ct1 <- table(BigDs1$FeedBackEvent, test_r3$class)
#diag(prop.table(ct, 1))
# total percent correct
confusionMatrix(ct1) #Accuracy:69.18


#confusion matrix(smallds1,pred1)--with small samples
test_r2 <-lda(formula = FeedBackEvent ~ ., 
              data = smallDs1,CV=TRUE  ) #cross validation the LDA
test_r2
ct1 <- table(smallDs1$FeedBackEvent, test_r2$class)
#diag(prop.table(ct, 1))
# total percent correct
confusionMatrix(ct1)

###Accuracy measure ##
# percent correct for each category of FeedbackEvent(Class)

#sum(diag(prop.table(ct))) #61% accuracy
##plot by Anova test
fit <- aov(FeedBackEvent ~ ., data=largeds1)
plot(fit)
summary(fit)
fit$coefficients

# ROC curve
#modelFit<- train(formula = FeedBackEvent ~ .,method="lda", preProcess=c('center','scale'), data=largeds1)
shiny_plotROC()
#confusionMatrix(test$Direction, predict(modelFit, test))
##Small dataset classification
data(largeds1)
largeds1
r2 <- lda(formula = FeedBackEvent ~ ., 
          data = smallDs1
)
r2


Rpred <-performance(pred1,"tpr","fpr")
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

