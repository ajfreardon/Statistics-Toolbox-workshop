#Unit 6 - Basic Regression

non.linear<-read.csv("non_linear.csv", header = T, na.strings = "")
head(non.linear)

#Dealing with Outliers
#can affect the analysis and interpretation of your data 
#especially in correlation and regression - depend heavily on central tendency
#typically defined as data points that lie outside the IQR (25th to 75th cent)

#install packages
install.packages("ggplot2", "tidyr", "dplyr")
library(ggplot2)
library(tidyr)
library(dplyr)

graphics.off
ggplot(data=lentils, aes(x=SITE,y=HEIGHT)) + geom_boxplot()

#may also test for outliers statistically
install.packages("outliers")
library(outliers)

chisq.out.test(lentils.m$HEIGHT, variance=var(lentils.m$HEIGHT), opposite = TRUE)
chisq.out.test(lentils.m$HEIGHT, variance=var(lentils.m$HEIGHT), opposite = FALSE)

#Pearson Correlation - and assumptions
#homogeneity of variance (homoscedasticity)

plot(lm(YIELD~HEIGHT,data=lentils)) #residuals normally distributed and variance seems equal

#Pearson (parametric correlation)
cor(lentils$HEIGHT,lentils$YIELD) #returns the correlation corefficient
cor.test(lentils$HEIGHT,lentils$YIELD) #p-value (significance)

#Non-parametric correlation - if assumptions of normality and variance are violated
#Spearman rank correlation, refers to correlation of rank/order (does not indicate the magnitude)

cor(lentils$DENSITY,lentils$HEIGHT, method = "spearman") 
cor.test(lentils$DENSITY,lentils$HEIGHT, method = "spearman")

#Simple Linear Regression
#Tests if the slope of th relationship is significantly different from zero
#must meet assumptions of normality and equal variance

output.lm<-lm(YIELD~HEIGHT, data=lentils) #defining the syntax for our linear model
summary(output.lm) #view the outputs

#Building a mutliple linear regression model
summary(output.lm)
#multiple linear regression 2 outcome variables
output.lm2<-lm(YIELD~HEIGHT+DENSITY, data=lentils)
#Multiple linear regression with 2 variables
summary(output.lm2)
#Multiple linear regression with 3 variables
output.lm3<-lm(YIELD~HEIGHT+DENSITY+FERTILIZER, data=lentils)
summary(output.lm3)

#Akaike's Information Criterion (AIC)
#considers the fit of the model and the number of parameters - more parameters = penalty
#model fit = the likelihood of the parameters being correct for the population based on the observed samples

#Simply--> AIC = number of parameters - liklihood of overall model
#THE LOWER THE AIC, THE BETTER THE MODEL
#Find the balance between model complexity and model fit using a ste-wise comparison

step(lm(YIELD~HEIGHT+DENSITY+FERTILIZER, data=lentils), direction = "backward")
step(lm(YIELD~HEIGHT+DENSITY+FERTILIZER, data=lentils), direction = "forward")
step(lm(YIELD~HEIGHT+DENSITY+FERTILIZER, data=lentils), direction = "both")

?step # more detailed information on choosing a model with AIC step-wise criterion

#Non-linear Regression
install.packages("minpack.lm")
library(minpack.lm)

#non-linear regression - log curve
nlsLM(Y1~a*PV^b, data=non.linear) # without starting values (error)
nlsLM(Y1~a*PV^b, data=non.linear, start = list(a=1,b=2)) #including start values

#check if the produced curve fits your data visually
log.curve<-function(x)0.8896*x^1.9539 #create the function

#plot the data and the curve
ggplot(non.linear,aes(x=PV,y=Y1)) + geom_point() +
  stat_function(fun = log.curve,color = "blue", size =1)

#Use AIC to evaluate the fit of a non-linear model (R2 values not appropriate)
AIC(nlsLM(Y1~a*PV^b, data = non.linear, start = list(a=1,b=2))) #Log model
AIC(nlsLM(Y1~a+b*PV^c, data = non.linear, start = list(a=0,b=1,c=2))) #exponential model
AIC(nlsLM(Y1~a+b*PV, data = non.linear, start = list(a=0,b=1))) #linear model
#Log model gave the lowest AIC and best fits the data

#Logistic Regression
#when the response variable is binomial (categorical), cannot use traditional regression
#use a logistic model - curve between two alternate "y" responses along an x variable

#"glm" function is preferred over "lm"
#build a logistic regression model
lr.model<-glm(SURVIVE~NITROGEN+PHOSPHORUS, data = lentils.s, family = "binomial")
summary(lr.model)

#we can use a step-wise comparison to identify the model fit (AIC)
step(glm(SURVIVE~NITROGEN+PHOSPHORUS, data = lentils.s, family = "binomial"))
#best logistic model is one that also considers nitrogen (lowest AIC score)

#can also add interactions to your logistic model
lr.model2<-glm(SURVIVE~NITROGEN*PHOSPHORUS, data = lentils.s, family = "binomial")
summary(lr.model2)

#we can also use logistic regression to determine the odds ratio
#using a logistic model considering only nitrogen
lr.model.n<-glm(SURVIVE~NITROGEN, data = lentils.s, family = "binomial")
summary(lr.model.n)

#odds ratio
exp(coef(lr.model.n)) # one unit of increase Nitrogen increases odds of survival by 0.22


