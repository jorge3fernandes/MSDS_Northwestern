######## Predict 411, Template for Unit 1
######## Video 1:  https://youtu.be/5DOzWVyfKoQ
######## Video 2:  https://youtu.be/68fb5vWDrFA

######## Download appropriate packages and install them from (https://cran.r-project.org/web/packages/available_packages_by_name.html)

library(rJava)
library(readr)
library(pbkrtest)
library(car)
library(leaps)
library(MASS)
library(xlsxjars)
library(xlsx)
library(psych)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####


moneyball= read.csv(file = file.choose(),header=T)
describe(moneyball)

					  
############## Part 1: Data Exploration ##########################################################################
str(moneyball)
summary(moneyball)

# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow = c(1,2))
hist(moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))


######## Scatterplot Matrix ##########

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Batting Stats and Wins
pairs(moneyball[2:8], lower.panel=panel.smooth, upper.panel = panel.cor)

#Baserunning  Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_BASERUN_CS + moneyball$TEAM_BASERUN_SB, lower.panel = panel.smooth)

#Pitcher Stats and Wins
pairs(~ moneyball$TARGET_WINS + moneyball$TEAM_PITCHING_BB + moneyball$TEAM_PITCHING_H + 
        moneyball$TEAM_PITCHING_HR + moneyball$TEAM_PITCHING_SO, lower.panel = panel.smooth)

pairs(moneyball[2,9,10,11,12,13])

######################### Part 2: Data Preparation #####################

#Fix Missing Values Using Mean of All Seasons
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_BATTING_HBP[is.na(moneyball$TEAM_BATTING_HBP)] = mean(moneyball$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball$TEAM_BASERUN_SB[is.na(moneyball$TEAM_BASERUN_SB)] = mean(moneyball$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball$TEAM_BASERUN_CS[is.na(moneyball$TEAM_BASERUN_CS)] = mean(moneyball$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball$TEAM_FIELDING_DP[is.na(moneyball$TEAM_FIELDING_DP)] = mean(moneyball$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball$TEAM_PITCHING_SO[is.na(moneyball$TEAM_PITCHING_SO)] = mean(moneyball$TEAM_PITCHING_SO, na.rm = TRUE)

#Straighten Relationships
moneyball$TEAM_BATTING_1B <- moneyball$TEAM_BATTING_H - moneyball$TEAM_BATTING_HR - moneyball$TEAM_BATTING_3B -
                             moneyball$TEAM_BATTING_2B
moneyball$log_TEAM_BATTING_1B <- log(moneyball$TEAM_BATTING_1B)
moneyball$log_TEAM_BATTING_3B <- log(moneyball$TEAM_BATTING_3B)
moneyball$log_TEAM_BASERUN_SB <- log(moneyball$TEAM_BASERUN_SB)
moneyball$log_TEAM_BASERUN_CS <- log(moneyball$TEAM_BASERUN_CS)
moneyball$TEAM_BATTING_SO[is.na(moneyball$TEAM_BATTING_SO)] = mean(moneyball$TEAM_BATTING_SO, na.rm = TRUE)
moneyball$TEAM_FIELDING_E[(moneyball$TEAM_FIELDING_E > 500)] = 500
moneyball$sqrt_TEAM_PITCHING_HR <- sqrt(moneyball$TEAM_PITCHING_HR)
moneyball$SB_PCT <- moneyball$TEAM_BASERUN_SB/(1.0*moneyball$TEAM_BASERUN_SB+moneyball$TEAM_BASERUN_CS)

#Check that na's are gone. 
summary(moneyball)

#Remove bad data from data set

moneyball2 <- subset(moneyball, TARGET_WINS >= 21 & TARGET_WINS <= 120)
moneyball2 <- subset(moneyball2, TEAM_PITCHING_H < 2000)

#################### Part 3: Model Creation ############################################

#Function for Mean Square Error Calculation
mse <- function(sm) 
  mean(sm$residuals^2)

# Stepwise Approach
stepwisemodel <- lm(formula = TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
               TEAM_BATTING_H + 
               TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
               TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + log_TEAM_BATTING_1B + 
               log_TEAM_BATTING_3B + log_TEAM_BASERUN_SB + SB_PCT + log_TEAM_BASERUN_CS + 
               sqrt_TEAM_PITCHING_HR, data = moneyball2)
stepwise <- stepAIC(stepwisemodel, direction = "both")
summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2

# All subsets regression
subsets <- regsubsets(TARGET_WINS ~ TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
                 TEAM_BATTING_H + 
                  TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_BASERUN_CS + TEAM_PITCHING_HR + 
                TEAM_PITCHING_BB + TEAM_PITCHING_SO + TEAM_FIELDING_E + TEAM_FIELDING_DP + 
                  SB_PCT + log_TEAM_BASERUN_CS + 
                  sqrt_TEAM_PITCHING_HR,  data = moneyball2, nbest = 2)
plot(subsets, scale="adjr2")

subset <- lm(TARGET_WINS ~ 
               TEAM_BATTING_1B + TEAM_BATTING_3B + TEAM_BATTING_HR + TEAM_BATTING_SO + TEAM_BASERUN_SB +
               TEAM_BASERUN_CS + TEAM_FIELDING_E + TEAM_FIELDING_DP + SB_PCT, data = moneyball2)
summary(subset)

# Model 3
model3 <- lm(TARGET_WINS ~ 
     TEAM_BATTING_1B + TEAM_BATTING_2B + TEAM_BATTING_3B + TEAM_BATTING_HR + 
       TEAM_BASERUN_SB + TEAM_BASERUN_CS + 
       TEAM_FIELDING_E + TEAM_FIELDING_DP +
       TEAM_PITCHING_SO + TEAM_PITCHING_BB, data = moneyball2)
summary(model3)
vif(model3)

######## Performance #######
AIC(stepwisemodel)
AIC(subset)
AIC(model3)
mse(stepwisemodel)
mse(subset)
mse(model3)

#####
#Designated proper working environment on my computer. You will want to make sure it is in proper place for your computer.
#####

#################### Test Data ##########################
setwd("D:/RFile/")
moneyball_test=read.csv("moneyball_test.csv",header=T)

# Fixing na's
moneyball_test$TEAM_BATTING_1B <- moneyball_test$TEAM_BATTING_H - moneyball_test$TEAM_BATTING_HR -
  moneyball_test$TEAM_BATTING_3B -moneyball_test$TEAM_BATTING_2B
moneyball_test$TEAM_BATTING_SO[is.na(moneyball_test$TEAM_BATTING_SO)] = mean(moneyball_test$TEAM_BATTING_SO, na.rm = TRUE)
moneyball_test$TEAM_BATTING_HBP[is.na(moneyball_test$TEAM_BATTING_HBP)] = mean(moneyball_test$TEAM_BATTING_HBP, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_SB[is.na(moneyball_test$TEAM_BASERUN_SB)] = mean(moneyball_test$TEAM_BASERUN_SB, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[is.na(moneyball_test$TEAM_BASERUN_CS)] = mean(moneyball_test$TEAM_BASERUN_CS, na.rm = TRUE)
moneyball_test$TEAM_FIELDING_DP[is.na(moneyball_test$TEAM_FIELDING_DP)] = mean(moneyball_test$TEAM_FIELDING_DP, na.rm = TRUE)
moneyball_test$TEAM_PITCHING_SO[is.na(moneyball_test$TEAM_PITCHING_SO)] = mean(moneyball_test$TEAM_PITCHING_SO, na.rm = TRUE)
moneyball_test$TEAM_BASERUN_CS[moneyball_test$TEAM_BASERUN_CS < 1] = 1
moneyball_test$SB_PCT <- moneyball_test$TEAM_BASERUN_SB/(1.0*moneyball_test$TEAM_BASERUN_SB+moneyball_test$TEAM_BASERUN_CS)
moneyball_test$SB_PCT[is.na(moneyball_test$SB_PCT)] = mean(moneyball_test$SB_PCT)
moneyball_test$log_TEAM_BASERUN_CS <- log(moneyball_test$TEAM_BASERUN_CS)


# Stand Alone Scoring
moneyball_test$P_TARGET_WINS <- 68.453769 + 0.036145 * moneyball_test$TEAM_BATTING_1B -
  0.011591* moneyball_test$TEAM_BATTING_2B + 
  0.203495* moneyball_test$TEAM_BATTING_3B +
  0.618848* moneyball_test$TEAM_BATTING_HR +
  0.154710* moneyball_test$TEAM_BATTING_BB -
  0.166438* moneyball_test$TEAM_BATTING_SO + 
  0.100967* moneyball_test$TEAM_BASERUN_SB -
  0.487091* moneyball_test$TEAM_PITCHING_HR - 
  0.115184* moneyball_test$TEAM_PITCHING_BB + 
  0.142082* moneyball_test$TEAM_PITCHING_SO -
  0.110246* moneyball_test$TEAM_FIELDING_E - 
  0.111298* moneyball_test$TEAM_FIELDING_DP -
  7.868975* moneyball_test$SB_PCT -
  4.986598* moneyball_test$log_TEAM_BASERUN_CS
  
#subset of data set for the deliverable "Scored data file"
prediction <- moneyball_test[c("INDEX","P_TARGET_WINS")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File 
write.xlsx(prediction, file = "write.xlsx", sheetName = "Predictions",
           col.names = TRUE)