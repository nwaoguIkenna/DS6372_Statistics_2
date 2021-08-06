install.packages('Lahman')
library(Lahman)
library(dplyr) 
library(tidyverse)
library(tidyr)
data(Batting)

Batting <- Batting %>% filter(!is.na(IBB)) %>% filter(!is.na(HBP))%>% filter(!is.na(SH))%>% filter(!is.na(SF))

apply(Batting, 2, function(x) any(is.na(x)))

Batting <- Batting %>% filter(!is.na(SO)) %>% filter(!is.na(GIDP))


reduced<-Batting[,6:22]
#pairs(reduced)

reduced <- reduced %>% select(-SH)

apply(reduced,2,summary)
var.raw<-apply(reduced,2,var)
var.raw
#Total variance
sum(var.raw)

cov(reduced)
#Another way to get total variance
sum(diag(cov(reduced)))

pc.result<-prcomp(reduced,scale.=TRUE)
pc.scores<-pc.result$x
#pairs(pc.scores)
#cor(pc.scores)

var.pca<-apply(pc.scores,2,var)
var.pca
#Total Variance of PC's
sum(var.pca)
#Total Variance of Original Variables.
sum(var.raw)

#List of eigenvectors
pc.result$rotation

par(mfrow=c(1,2))
eigenvals<-(pc.result$sdev)^2
plot(1:17,eigenvals/sum(eigenvals),type="l",main="Scree Plot",ylab="Prop. Var. Explained")
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
plot(1:17,cumulative.prop,type="l",main="Cumulative proportion",ylim=c(0,1))
par(mfrow=c(1,1))


bc<-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data",header=F,sep=",")
names(bc)<- c('id_number', 'diagnosis', 'radius_mean', 
              'texture_mean', 'perimeter_mean', 'area_mean', 
              'smoothness_mean', 'compactness_mean', 
              'concavity_mean','concave_points_mean', 
              'symmetry_mean', 'fractal_dimension_mean',
              'radius_se', 'texture_se', 'perimeter_se', 
              'area_se', 'smoothness_se', 'compactness_se', 
              'concavity_se', 'concave_points_se', 
              'symmetry_se', 'fractal_dimension_se', 
              'radius_worst', 'texture_worst', 
              'perimeter_worst', 'area_worst', 
              'smoothness_worst', 'compactness_worst', 
              'concavity_worst', 'concave_points_worst', 
              'symmetry_worst', 'fractal_dimension_worst')

#Getting a look at the distribution
table(bc$diagnosis)
bc

#Scatter plots color coded by response for just the first few variables
pairs(bc[,3:6],col=bc$diagnosis)

pc.bc<-prcomp(bc[,-c(1,2)],scale.=TRUE)
pc.bc.scores<-pc.bc$x

pc.bc.scores<-data.frame(pc.bc.scores)
pc.bc.scores$Diagnosis<-bc$diagnosis

library(ggplot2)
ggplot(data = pc.bc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")

ggplot(data = pc.bc.scores, aes(x = PC1, y = PC22)) +
  geom_point(aes(col=Diagnosis), size=1)+
  ggtitle("PCA of Breast Cancer Tumor Biopsies")

bc
pc.bc.scores

newbc <- bc[,-c(1)]

#  do lda
library(MASS)
library(caret)
r <- lda(formula = diagnosis ~ ., 
         data = newbc)
r

predict = predict(r, newbc)
cfm = confusionMatrix(predict$class,as.factor(newbc$diagnosis))

head(predict$x, 6)
head(predict$class)

fake<-bc
fake$diagnosis<-sample(fake$diagnosis,569,replace=F)
newfake <- bc[,-c(1)]

r <- lda(formula = diagnosis ~ ., 
         data = newfake)
r

predict = predict(r, newfake)
cfm = confusionMatrix(predict$class,as.factor(newfake$diagnosis))
cfm

head(predict$x, 6)
head(predict$class)
