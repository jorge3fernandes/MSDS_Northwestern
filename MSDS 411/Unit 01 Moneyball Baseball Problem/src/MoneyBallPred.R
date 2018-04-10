library(caret)
library(corrplot)
library(dplyr)
library(missForest) # will be used to impute missing values

moneyball = read.csv(file = file.choose(), header = T)

#let check for NAs in the data
summary(moneyball)
#Counting the number of NAs per column and check the percentage of NAs per column
NAPerc <- sapply(moneyball, function(x) (sum(is.na(x))/length(x))*100) %>%
  data.frame()

# let's check focus only on columns with missing values.

subset(NAPerc,. > 0)

# Looks like we will be deleting the TEAM_BATTING_HPB, But first I need to understand why it's missing. After doing some reading it looks like that columns could be translated to a 0 or 1. I need to do more investigating

plot(NAPerc)

data.frame(NAanalysis)







corrplot(moneyball, method="circle")


mball_cor <- cor(moneyball)
findCorrelation(mball_cor, .75)
findLinearCombos(mball_cor)
