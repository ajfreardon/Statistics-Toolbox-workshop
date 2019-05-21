#Unit 4 - Non-Parametric statisitics

#Importing Data for non-parametric statistics
lentils<-read.csv("lentils.csv", header = T, na.strings = T)

head(lentils)
str(lentils)

#install needed packages for Non-parametric testing
install.packages("ggplot2", "dplyr", "tidyr")

#Load packages
library("ggplot2")
library("dplyr")
library("tidyr")

#Build a boxplot of YIELD by VARIETY (checking the distribution of the data - normal?)
ggplot(lentils, aes(x=VARIETY, y=HEIGHT))+ geom_boxplot()+
  scale_x_discrete(name="Lentil Variety")

#Build boxplot of YIELD by SITE
ggplot(lentils, aes(x=SITE, y=HEIGHT))+ geom_boxplot()+
  scale_x_discrete(name="Planting Site")

#Shapiro Test - investigating the normality
shapiro.test(lentils$HEIGHT[lentils$VARIETY=="A"])
shapiro.test(lentils$HEIGHT[lentils$VARIETY=="B"])
shapiro.test(lentils$HEIGHT[lentils$VARIETY=="C"])

#Comparison of the t-test with its non-parametric equivalent --> Wilcoxon
lentils.x<-filter(lentils,SITE=="xeric")

#Compare results from one sample t-test and Wilcoxon
t.test(lentils.x$HEIGHT, mu=0.46, alternative = "two.sided")
wilcox.test(lentils.x$HEIGHT, mu=0.46, alternative = "two.sided")

#Running Mann-Whitney(Wilxocon)
#subset the data so you can compare the two groups --> lentils.x and lentils.m
lentils.m<-filter(lentils,SITE=="mesic")

t.test(lentils.x$HEIGHT,lentils.m$HEIGHT)
wilcox.test(lentils.x$HEIGHT,lentils.m$HEIGHT) #cannot compute exact p-value with ties
#two values share the same value, and therefore the same rank "tied"

#Kolmogorov-Smirnov test
#For a two sample, two-tailed t.test
ks.test(lentils.x$HEIGHT, lentils.m$HEIGHT)

#For two sample, one-tailed test
ks.test(lentils.x$HEIGHT, lentils.m$HEIGHT, alternative = "greater")

#Kruskal-Wallis - non-parametric equivalent of the one-way ANOVA
kruskal.test(HEIGHT~VARIETY, data=lentils)

#Can compare these results to the parametric ANOVA
summary(aov(HEIGHT~VARIETY, data=lentils))

#Since there are multiple treatments, we need to follow-up with pair-wise Wilcoxon 

VarA<-filter(lentils, VARIETY=="A")
VarB<-filter(lentils, VARIETY=="B")
VarC<-filter(lentils, VARIETY=="C")

wilcox.test(VarA$HEIGHT, VarB$HEIGHT, alternative = "two.sided")
wilcox.test(VarA$HEIGHT, VarC$HEIGHT, alternative = "two.sided")
wilcox.test(VarB$HEIGHT, VarC$HEIGHT, alternative = "two.sided")

#We are making multiple comparisons --> we need to adjust the p-value
p.adjust(c(1, 0.09265, 0.0001554), method ="bonferroni", n=3)

#Permutation Testing
#Permutation Analysis of Variance

#Start with a regular ANOVA
anova(lm(YIELD~FARM*VARIETY,data=lentils))

#run the permutational ANOVA
install.packages("lmPerm")
library(lmPerm)
summary(aovp(YIELD~FARM*VARIETY,data=lentils, seqs=T))

#follow-up with pairwise comparisons
output3.p<-aovp(YIELD~FARM*VARIETY,data=lentils, seqs = T)
TukeyHSD(output3.p)
