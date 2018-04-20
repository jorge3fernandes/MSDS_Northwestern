library(caret)
library(corrplot)
library(dplyr)
library(psych)
library(mice) # will be used to impute missing values
library(e1071) # to understand skewness
library(AppliedPredictiveModeling)

moneyball = read.csv('/Users/legs_jorge/Documents/Data Science Projects/MSDS_Northwestern/MSDS 411/Unit 01 Moneyball Baseball Problem/Data/moneyball.csv', header = T)

# 1.0 - Preprocessing ---------------------------------

## 1.1 - Data cleaning
### 1.1.1 Dealing with Missing Values
#
# Columns with missing values
# 
str(moneyball)

NAPerc <- sapply(moneyball, function(x) (sum(is.na(x))/length(x))*100) %>%
  data.frame()
NAPerc$Column <- rownames(NAPerc)
colnames(NAPerc) <- c("NA_Perc", "Col_Name")
NA_col <- subset(NAPerc,NA_Perc > 0) %>% arrange(desc(NA_Perc)) # Trying to understand the percentage of NAs per Column

#What is the distribution of the NA values?

NA_subset <- moneyball[,c(NA_col$Col_Name)]
matrixplot(NA_subset, labels = TRUE, interactive = TRUE) # the red indicates NAs

# another way to visualize missing data

md.pattern(moneyball) %>% data.frame()

is.na(moneyball$TEAM_BATTING_HBP) <- 0 # Assuming that NAs mean zero


#Drawing margin plot
#The margin plot, plots two features at a time. 
#The red plot indicates distribution of one feature when it is missing 
#while the blue box is the distribution of all others when the feature 
#is present. This plot is useful to understand if the missing values are MCAR. 
#For MCAR values, the red and blue boxes will be identical.
missing_col <- NA_col$Col_Name
marginmatrix(moneyball[,missing_col], delimiter = "_imp", alpha = 0.6)
marginplot(moneyball[, c("TEAM_BATTING_HBP","TEAM_BASERUN_CS")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(moneyball[, c("TEAM_FIELDING_DP","TEAM_BASERUN_SB")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)
marginplot(moneyball[, c("TEAM_BATTING_SO","TEAM_PITCHING_SO")], col = mdc(1:2), cex.numbers = 1.2, pch = 19)


#Imputing missing values using mice
mice_imputes = mice(moneyball, m = 5, maxit = 40)

#What methods were used for imputing
mice_imputes$method
# I only have numeric values, mice chose PMM (Predictive Mean Matching)

#Imputed dataset
Imputed_data <- complete(mice_imputes,5)

#make a density plot
densityplot(mice_imputes)
# the red imputed values should be similar to the blue imputed values for them 
# to be MAR here.


#Plotting and comparing values with xyplot()
xyplot(mice_imputes, TEAM_BASERUN_SB ~ TEAM_BATTING_SO | .imp, pch = 20, cex = 1.4)



# understand the variable distribution before imputation
#  # Bulmer (1979) — a classic — suggests this rule of thumb:
#   
# If skewness is less than −1 or greater than +1, the distribution is highly skewed.
# If skewness is between −1 and −½ or between +½ and +1, the distribution is moderately skewed.
# If skewness is between −½ and +½, the distribution is approximately symmetric.

 skew_col <- apply(Imputed_data, 2, skewness, na.rm = TRUE)
 skew_col
 
 # We can clearly see our data is extremely skewd. Let's try a couple histograms
 # Let's use box-cox to transform the data
 
 Prep_moneyball <- preProcess(Imputed_data, method = c("BoxCox","center","scale")) %>% predict(Imputed_data)
apply(Prep_moneyball, 2, skewness, na.rm = TRUE)

# Wins - Use lower bound for lower outliers, upper bound for higher outliers.
par(mfrow = c(1,2))
hist(Prep_moneyball$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(Prep_moneyball$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(Prep_moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(Prep_moneyball$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(Prep_moneyball$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(Prep_moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(Prep_moneyball$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(Prep_moneyball$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(Prep_moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(Prep_moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(Prep_moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(Prep_moneyball$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(Prep_moneyball$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(Prep_moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(Prep_moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(Prep_moneyball$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(Prep_moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(Prep_moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(Prep_moneyball$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(Prep_moneyball$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(Prep_moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(Prep_moneyball$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(Prep_moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(Prep_moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(Prep_moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(Prep_moneyball$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(Prep_moneyball$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))

 
 # We can clearly see that we have some outliers using the histogram.
 # One way to deal with it is to use Hubers function to not weight all the points equally. The outliers should have less weight on the model.
 # 
 # we can also use Spatial Sign to 






# 2.0 - Correlation Analysis ---------------------------------

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(Prep_moneyball, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)
cor.ci(Prep_moneyball, method = "spearman")
corr_matrix <- cor(Prep_moneyball)

findCorrelation(corr_matrix, cutoff = 0.7)
findLinearCombos(Prep_moneyball) # Any fields that are a linear combination of each other?
# 3.0 - Model Training ---------------------------------
model1 <- lm(TARGET_WINS ~ ., data = Prep_moneyball[,-c(1,11)])
summary(model1)
stepwise <- stepAIC(model1, direction = "both")
summary(stepwise)
vif(stepwise)
sqrt(vif(stepwise)) > 2

# 4.0 - Residual Analysis ---------------------------------


