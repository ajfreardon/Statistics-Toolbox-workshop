#Unit 3 - Parametric Statistics

#Importing Data for parametric statistics
lentils<-read.csv("lentils.csv", header = T, na.strings = T)

head(lentils)
str(lentils)

lentilN<-read.csv("lentils_nitrogen.csv", header = T, na.strings = "")

head(lentilN)
View(lentilN)

#install needed packages for parametric testing
install.packages("ggplot2", "dplyr", "tidyr")
library("ggplot2")
library("dplyr")
library("tidyr")

#Single treatement analysis - Building a BOXPLOT
graphics.off
ggplot(filter(lentils, FARM=="Farm1"), aes(x=VARIETY, y=YIELD)) +geom_boxplot()+
  scale_x_discrete(name="Lentil Variety")

#Multiple treatment analysis - Grouping Boxplots
ggplot(lentils, aes(x=paste(FARM, VARIETY), y=YIELD)) +geom_boxplot()+
  scale_x_discrete(name="Farm and Lentil Variety")

#Check normality and variance by plotting the data residuals

#Single treatment anaysis
plot(lm(YIELD~VARIETY, data=filter(lentils, FARM=="Farm1")))

#Multiple treatment anaysis
plot(lm(YIELD~FARM*VARIETY, data=lentils))

#Testing normality statistically - Shapiro-Wilks
#Multiple treatments means multiple tests of combinations for normality -
#e.g. Farm varieties A B and C

F1VarA<-filter(lentils, FARM=="Farm1"& VARIETY=="A")
shapiro.test(F1VarA$YIELD)
F1VarB<-filter(lentils, FARM=="Farm1"& VARIETY=="B")
shapiro.test(F1VarB$YIELD)
F1VarC<-filter(lentils, FARM=="Farm1"& VARIETY=="C")
shapiro.test(F1VarB$YIELD)

F2VarA<-filter(lentils, FARM=="Farm2"& VARIETY=="A")
shapiro.test(F2VarA$YIELD)
F2VarB<-filter(lentils, FARM=="Farm2"& VARIETY=="B")
shapiro.test(F2VarB$YIELD)
F2VarC<-filter(lentils, FARM=="Farm2"& VARIETY=="C")
shapiro.test(F2VarB$YIELD)

#Testing homogeneity and equal variance statistically - Bartlett

Farm1<-filter(lentils, FARM=="Farm1")
Farm2<-filter(lentils, FARM=="Farm2")
bartlett.test(Farm1$YIELD~Farm1$VARIETY)
bartlett.test(Farm2$YIELD~Farm2$VARIETY)

#Option - change the distribution of the data (3 main ways)

sqrt_PROTIEN<-sqrt(lentils$PROTEIN)   #Square root the data
log_PROTEIN<-log(lentils$PROTEIN)     #log transform the data
inv_PROTEIN<-1/lentils$PROTEIN        #inverse the data

#One sample T-TEST --> is the mean of VARIETY for Farm1 greater than 650 (pop mean)
t.test(F1VarA$YIELD, mu=650, alternative = "greater")

#Two sample T-TEST --> are the means of two populations significantly different?
#E.g. are varieties A and B on Farm 1 significantly different?
t.test(F1VarA$YIELD, F1VarB$YIELD, alternative = "two.sided")

#Paired t-test --> comparison of individuals or samples before and after treatement
t.test(lentilN$BEFORE, lentilN$AFTER)                   #normal T-Test
t.test(lentilN$BEFORE, lentilN$AFTER, paired = T)     #paired T-Test

#One-way ANOVA - investigating the effect of ne treatment
#example uses the variety of lentil (ignoring the FARM)

output.lm<-anova(lm(YIELD~VARIETY, data=lentils))   #ANOVA option 1
output.lm

output.aov<-aov(YIELD~VARIETY, data = lentils)      #ANOVA option 2 (gives the same result)
summary(output.aov)
# high p-value - no significant effect of lentil variety on yield 

#Multi-way ANOVA
#Examining the main effect and interacting effects (e.g., farm 1 and 2, and varieties ABC)

output.2m<-anova(lm(YIELD~VARIETY*FARM,data=lentils))
output.2m       #view the ANOVA output

output2.aov<-aov(YIELD~VARIETY*FARM, data=lentils)
summary(output2.aov)

#Since there is a significant interaction of variety and farm we need to look further
#PAIRWISE COMPARISONS

#However, instead of running mutliple T-Tests, you can run one test in R "TukeyHSD"
#Tukey runs all the comparisons for every treatment level and automatically adjusts the p-value

TukeyHSD(aov(YIELD~FARM*VARIETY,data = lentils))

#Can also observe the interaction visually with an interaction plot

#Varieties across Farms
interaction.plot(lentils$FARM,lentils$VARIETY,lentils$YIELD)

#Varieties at farms
interaction.plot(lentils$VARIETY,lentils$FARM,lentils$YIELD)

#Lines cross? --> significant interaction!
#interpretation --> higher yield farm 1 compared to farm 2, with big difference for Var A

#Analysis of Covariance

data.F1<-filter(lentils, FARM=="Farm1")
data.F1$NITROGEN = c(2.2, 3.4, 1.9, 4.3, 3.8, 1.1, 4.9, 2.5, 5.7, 1.0, 3.2, 2.0)
data.F1$BLOCK=c("B2","B3","B1","B4","B3","B1","B4","B2","B4","B1","B3","B2")

#Determining the effect of a covariate
summary(aov(YIELD~VARIETY, data=data.F1))
summary(aov(YIELD~VARIETY+NITROGEN,data=data.F1))

#Adding a Block structure
#if blocking accounts for a proportion of the variance then your test becomes more powerful

summary(aov(YIELD~VARIETY, data = data.F1))
summary(aov(YIELD~VARIETY+BLOCK, data = data.F1))

#smaller p-value, inclusion of BLOCK accounted for part of the error
#lowering the residual SS and MS, increading the F-ratio and decreasgin the p-value