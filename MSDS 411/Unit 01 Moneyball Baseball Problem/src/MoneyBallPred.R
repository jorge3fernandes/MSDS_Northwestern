library(caret)
library(corrplot)
library(dplyr)
library(psych)
library(mice) # will be used to impute missing values
library(e1071) # to understand skewness

moneyball = read.csv('/Users/legs_jorge/Documents/Data Science Projects/MSDS_Northwestern/MSDS 411/Unit 01 Moneyball Baseball Problem/Data/moneyball.csv', header = T)

# 1.0 - Preprocessing ---------------------------------

## 1.1 - Data cleaning
### 1.1.1 Dealing with Missing Values
#
# Columns with missing values
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

# understand the variable distribution before imputation
 skew_col <- apply(moneyball, 2, skewness, na.rm = TRUE)
 skew_col
 
 # Bulmer (1979) — a classic — suggests this rule of thumb:
 #   
 # If skewness is less than −1 or greater than +1, the distribution is highly skewed.
 # If skewness is between −1 and −½ or between +½ and +1, the distribution is moderately skewed.
 # If skewness is between −½ and +½, the distribution is approximately symmetric.
 # We can clearly see our data is extremely skewd. Let's try a couple histograms
 
 # Try adjusting bin size for those with few bars
 hist(moneyball$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
 hist(moneyball$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
 hist(moneyball$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
 hist(moneyball$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs") 
 hist(moneyball$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
 hist(moneyball$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
 hist(moneyball$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP") 
 hist(moneyball$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
 hist(moneyball$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
 hist(moneyball$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
 hist(moneyball$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
 hist(moneyball$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
 hist(moneyball$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
 hist(moneyball$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
 hist(moneyball$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")