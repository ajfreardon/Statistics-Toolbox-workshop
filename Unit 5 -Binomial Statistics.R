#Unit 5 - Binomial Statistics

#load up data on lentil survival
lentils.s<-read.csv("lentils_survival.csv", header = T, na.strings = "")

head(lentils)
tail(lentils.s)
str(lentils.s)

#load up pertinent packages - dplyr and tidyr
install.packages("dplyr", "tidyr")
library(dplyr)
library(tidyr)

#Assign groups to the data
lentils.s<-group_by(lentils.s, VARIETY)

#Create a table of the three species 
