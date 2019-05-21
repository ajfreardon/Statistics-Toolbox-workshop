#Statistics Toolbox

#import the dataset
lentils.csv

lentils<-read.csv("lentils.csv", header=T, na.strings = ".")
head(lentils)
tail(lentils)
View(lentils)
str(lentisl)
structure(lentils)

#functions for descriptive statistics
#performing simple statiscal commands
mean(lentils$YIELD)
IQR(lentils$YIELD)
max(lentils$YIELD)

#skipping missing values --> na.rm=TRUE (remember that this is case sensitive i.e., CAPS)
mean(lentils$PROTEIN)
mean(lentils$PROTEIN, na.rm = TRUE)

#Can also calculate quantiles
quantile(lentils$YIELD, c(0.025, 0.05, 0.075, 0.95, 0.975))

#You can also calulate multiple descriptive statistics
#min, low hinge, median, high-hinge, max)
fivenum(lentils$HEIGHT)

summary(lentils)

#Can conduct multi-level summaries (i.e., grouping data)
lentils<-group_by(lentils,VARIETY)

summarize(lentils, 
          YIELD.avg=mean(YIELD, na.rm=T),
          HEIGHT.avg=mean(HEIGHT, na.rm=T),
          PROTEIN.avg=mean(PROTEIN, na.rm=T))

#finding length or count but not including missing values
length(lentils$PROTEIN[!is.na(lentils$PROTEIN)])

#including groups of the mean and standard deviation
lentils.sgb<-summarize(group_by(lentils, VARIETY),
                       YIELD.avg = mean(YIELD, na.rm = T),
                       YIELD.se = sd(YIELD)/sqrt(length(YIELD)),
                       PROTEIN.avg = mean(PROTEIN, na.rm=T),
                       PROTEIN.sd = sd(PROTEIN, na.rm = T)/sqrt(length(PROTEIN[!is.na(PROTEIN)])))
#view the output
lentils.sgb                       

#export the dataset
write.csv(lentils.sgb,"lentil_summary.csv")

#Assign the groups multiple variables in the multi-way summary table
summarize(group_by(lentils,FARM,VARIETY),
          YIELD.avg=mean(YIELD,na.rm = T),
          YIELD.q1 = quantile(YIELD, 0.25),
          YIELD.q3 = quantile(YIELD, 0.75),
          YIELD.min = min(YIELD, na.rm = T),
          YIELD.max = max(YIELD, na.rm = T))

#Provides a simple equivalent to a multifactor boxplot
boxplot(YIELD~VARIETY*FARM,data = lentils)

#Confidence Intervals

#functions qnorm() and pnorm() - convert unis of SD or SE for a normal distribution

pnorm(1)
pnorm(-1)
qnorm(0.86)
qnorm(0.84)

#Calculating the 95 % Confidence Interval
qnorm(0.95) #95%, right side of the normal distribution
qnorm(0.05) #5%, left side of the normal distribution

#Dataset with normal distribution - mean 10, SD 4, we see that 6th cent. equals an SD of 1
pnorm(6,mean = 10, sd = 4) #equal to SD of -1
pnorm(6, 10, 4) #short form

#Calculating confidence intervals for small sample sizes
#State the degrees fo freedom (n-1), example df = 9 for a sample size of 10
qt(0.05, df=9)
qt(0.95, df=9)

#for percentiles use pnorm() or pt()
pt(1.5, df=9)

#Example - 95 % CI of variety A on farm 1 (n=4)

VarAF1<-filter(lentils, VARIETY=="A"& FARM=="Farm1") #identify a variabe for Variety A Farm 1
mean(VarAF1$YIELD, na.rm = T)
sd(VarAF1$YIELD)/sqrt(4)
qt(0.95, 3) #+/- SDs of the 95 % CI with 3 degrees of freedom

#Confidence Intervals
mean(VarAF1$YIELD, na.rm = T) + sd(VarAF1$YIELD)/sqrt(4) * qt(0.95, 3) #upper
mean(VarAF1$YIELD, na.rm = T) + sd(VarAF1$YIELD)/sqrt(4) * qt(0.05, 3) #lower

#T-test function also returns the confidence intervals
t.test(VarAF1$YIELD, conf.level = 0.95)
t.test(VarAF1$YIELD, conf.level = 0.90)

#For non-normal distributions
install.packages("boot") #to obtain bootstrap values (see notes)
library(boot)

Bmean<-function(data, indices) {
  d<-data[indices]
  return(mean(d))
}

results<-boot(data=lentils$YIELD, statistic = Bmean, R=1000)
plot(results)

boot.ci(results, type=c("norm", "basic", "perc", "bca"))
