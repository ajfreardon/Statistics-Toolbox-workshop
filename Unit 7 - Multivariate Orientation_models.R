#Unit 7 - Multivariate Orientation

install.packages("rlang","dplyr","plyr","candisc","MASS")

library(rlang)
library(dplyr)
library(plyr)
library(candisc)
library(MASS)

#load up data for this module
data("USArrests")
head(USArrests)
tail(USArrests)
str(USArrests)

data("iris")
head(iris)
tail(iris)
str(iris)

#Principal Component Analysis (PCA)
PCAoutput1<-princomp(USArrests, cor = T)

#correlation = true or false
#cor = false (covariance matrix) when variables are on the same scale
#cor = true (correlation matrix) when variables are not on the same scale (standardization)

#Output from the PCA
PCAoutput1$loadings     #correlations with the original variables
PCAoutput1$scores       #new coordinate points after rotation
summary(PCAoutput1)     #variance explained by the components
eigen(cor(USArrests))   #eigenvalues
eigen(cor(USArrests))$values/4    #variance explained by eigenvalues

#PCA biplot to visualize the rotated points
biplot(PCAoutput1, choices = c(1,2))

#Discmriminant Analysis

x=lm(cbind(Petal.Length,Sepal.Length,Petal.Width,Sepal.Width)~Species,data = iris)
out2=candisc(x,term="Species")

#Output from the discriminant analysis
#First - look at the results, including the variance explained by each linear discriminant
#Second - Extract the discriminant function loadings 
#Finally - Plot

summary(out2)
out2$structure #discriminant loadings
plot(out2,which=c(1,2), scale=8, var.col="#777777", var.lwd=1,
     col=c("red","green","blue"))

#After installing and loading the library for the MASS pacakge

#Discriminant analysis
out2.v2=lda(Species~.,iris)

#Output from the discriminant analysis
out2 #analysis results
lda.scores<-predict(out2.v2,iris)$x   #new coordinates of points after rotation

#Build a table with the new observations
Sepal.Length<-c(4.7,4.8,6.2,5.1,5.6,6.3)
Sepal.Width<-c(3.2,3.2,2.9,2.5,2.8,3.5)
Petal.Length<-c(1.7,1.6,4.2,3.1,4.1,6)
Petal.Width<-c(0.2,0.2,1.3,1.1,1.3,2.5)
newObs<-as.data.frame(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width))

#Predict species of the new observations using discriminant analysis
out2p<-predict(out2.v2,newObs)
scores_unknown<-out2p$x
plot(lda.scores,col=rainbow(3)[iris$Species], asp=1)
points(scores_unknown, pch=19)

#Testing PLSDA Analysis - packages from the internet
install.packages("DiscriMiner")
library(DiscriMiner)

# PLS discriminant analysis specifying number of components = 2
my_pls1 = plsDA(iris[,1:4], iris$Species, autosel=FALSE, comps=2)
my_pls1$confusion
my_pls1$error_rate
#plot circle of correlations
plot(my_pls1)

# PLS discriminant analysis with automatic selection of components
my_pls2 = plsDA(iris[,1:4], iris$Species, autosel=TRUE)
my_pls2$confusion
my_pls2$error_rate
plot(my_pls2)

#linear discriminant analysis with learn-test validation
learning = c(1:40, 51:90, 101:140)
testing = c(41:50, 91:100, 141:150)
my_pls3 = plsDA(iris[,1:4], iris$Species, validation="learntest",
learn=learning, test=testing)
my_pls3$confusion
my_pls3$error_rate
plot(my_pls3)
