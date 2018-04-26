library(e1071) # to understand skewness
library(dplyr)
library(stringr) # Used to rename the columns by removing the word team from the column header
library(VIM) # To understand NAs
library(caret)
library(xlsxjars)
library(xlsx)
library(mice)
library(missForest) # Imputation 
library(MASS) # to use for robust Linear Regression.


moneyball_test = read.csv(
  '/Users/legs_jorge/Documents/Data Science Projects/MSDS_Northwestern/MSDS 411/Unit 01 Moneyball Baseball Problem/Data/moneyball_test.csv',
  header = T
)
colnames(moneyball_test) <-
  str_replace_all(colnames(moneyball_test), "TEAM_", "") %>%
  tolower() # Fixing column names


batting_hbp_bi_test <- if_else(is.na(moneyball_test$batting_hbp), 0, 1)
batting_hbp_test <- moneyball_test$batting_hbp
moneyball_trans_test <- subset(moneyball_test, select = -c(batting_hbp))

# Imputing NAs
moneyball_imp_test <- missForest(moneyball_trans_test)$ximp

#add
moneyball_imp_test$batting_hbp <- if_else(is.na(batting_hbp_test),0,as.numeric(batting_hbp_test))
moneyball_imp_test$batting_hbp_bi <- batting_hbp_bi_test

# Feature engineering
# 
# 

moneyball_imp_test$batting_1B <- moneyball_imp_test$batting_h-(moneyball_imp_test$batting_2b + moneyball_imp_test$batting_3b + moneyball_imp_test$batting_hr)
moneyball_imp_test$free_bases_num <-  if_else(is.na(moneyball_imp_test$batting_hbp),0,as.numeric(moneyball_imp_test$batting_hbp)) + moneyball_imp_test$batting_bb
moneyball_imp_test$total_bases <- moneyball_imp_test$batting_1B + 2 * moneyball_imp_test$batting_2b + 3 * moneyball_imp_test$batting_3b + 4 * moneyball_imp_test$batting_hr + moneyball_imp_test$batting_bb + if_else(is.na(moneyball_imp_test$batting_hbp),0,as.numeric(moneyball_imp_test$batting_hbp)) + moneyball_imp_test$baserun_sb
moneyball_imp_test$total_bases_allowed = moneyball_imp_test$pitching_bb + 4 * moneyball_imp_test$pitching_hr + moneyball_imp_test$pitching_h
moneyball_imp_test$HR_over_OP = moneyball_imp_test$batting_hr - moneyball_imp_test$pitching_hr
moneyball_imp_test$walks_over_OP = moneyball_imp_test$batting_bb - moneyball_imp_test$pitching_bb
moneyball_imp_test$SO_over_OP = moneyball_imp_test$pitching_so - moneyball_imp_test$batting_so


moneyball_imp_test <- subset(moneyball_imp_test, select = -c(batting_hbp))
moneyball_imp_test <- subset(moneyball_imp_test, select = -c(batting_hr, free_bases_num, pitching_h))

moneyball_imp_test$P_TARGET_WINS <- 32.157432 - 0.035903 * moneyball_imp_test$batting_2b + 0.068862 * moneyball_imp_test$batting_3b + 0.044466 * moneyball_imp_test$batting_bb  - 0.016966 * moneyball_imp_test$batting_so + 0.060647 * moneyball_imp_test$baserun_sb  - 0.050230 * moneyball_imp_test$pitching_bb - 0.043364 * moneyball_imp_test$fielding_e - 0.105258 * moneyball_imp_test$fielding_dp- 4.089404 * moneyball_imp_test$batting_hbp_bi + 0.021326 * moneyball_imp_test$total_bases + 0.011782 * moneyball_imp_test$total_bases_allowed + 0.023997 * moneyball_imp_test$walks_over_OP + 0.008021 * moneyball_imp_test$SO_over_OP
#predict(base_model_lp_rm,moneyball_imp_test)                                  
                                  
                                   




#subset of data set for the deliverable "Scored data file"
prediction <- moneyball_imp_test[c("index", "P_TARGET_WINS")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File
write.xlsx(prediction,
           file = "Jorge_Fernandes_MoneyBall_Pred.xlsx",
           sheetName = "Predictions",
           col.names = TRUE,
           row.names = FALSE)
