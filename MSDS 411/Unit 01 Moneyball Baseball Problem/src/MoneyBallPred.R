library(e1071) # to understand skewness
library(dplyr)
library(stringr) # Used to rename the columns by removing the word team from the column header
library(VIM) # To understand NAs
library(caret)
library(mice)
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
mice_imputes_test <- mice(moneyball_trans_test, m = 10, maxit = 40)
#What methods were used for imputing
method_test <- mice_imputes_test$method
# I only have numeric values, mice chose PMM (Predictive Mean Matching)

#Imputed dataset
moneyball_imp_test <- complete(mice_imputes_test, 10)

#add
moneyball_imp_test$batting_hbp <- batting_hbp_test
moneyball_imp_test$batting_hbp_bi_test <- batting_hbp_bi_test
# Feature engineering

moneyball_imp_test$batting_1B <-
  moneyball_imp_test$batting_h - (moneyball_imp_test$batting_2b + moneyball_imp_test$batting_3b + moneyball_imp_test$batting_hr)
moneyball_imp_test$free_bases_num <-
  if_else(is.na(moneyball_imp_test$batting_hbp),
          0,
          as.numeric(moneyball_imp_test$batting_hbp)) + moneyball_imp_test$batting_bb
moneyball_imp_test$total_bases <-
  moneyball_imp_test$batting_1B + 2 * moneyball_imp_test$batting_2b + 3 * moneyball_imp_test$batting_3b + 4 * moneyball_imp_test$batting_hr + moneyball_imp_test$batting_bb + if_else(is.na(moneyball_imp_test$batting_hbp),
                                                                                                                                                             0,
                                                                                                                                                             as.numeric(moneyball_imp_test$batting_hbp)) + moneyball_imp_test$baserun_sb
moneyball_imp_test$total_bases_allowed = moneyball_imp_test$pitching_bb + 4 * moneyball_imp_test$pitching_hr + moneyball_imp_test$pitching_h
moneyball_imp_test$HR_over_OP = moneyball_imp_test$batting_hr - moneyball_imp_test$pitching_hr
moneyball_imp_test$walks_over_OP = moneyball_imp_test$batting_bb - moneyball_imp_test$pitching_bb
moneyball_imp_test$SO_over_OP = moneyball_imp_test$pitching_so - moneyball_imp_test$batting_so 


moneyball_imp_test <- subset(moneyball_imp_test, select = -c(batting_hbp))

moneyball_test$P_TARGET_WINS <- predict(stepwise_base_model_bd, moneyball_imp_test)
  0.000000000069 + 
  0.2724 * moneyball_imp_test$batting_h - 
  0.05554 * moneyball_imp_test$batting_2b + 
  0.1149 * moneyball_imp_test$batting_3b + 
  0.9685 * moneyball_imp_test$batting_bb - 
  0.3603 * moneyball_imp_test$batting_so + 
  0.1612 * moneyball_imp_test$baserun_sb - 
  0.2747 * moneyball_imp_test$pitching_h - 
  0.06647 * moneyball_imp_test$pitching_bb +
  0.1534 * moneyball_imp_test$pitching_so - 
  0.5182 * moneyball_imp_test$fielding_e - 
  0.2421 * moneyball_imp_test$fielding_dp - 
  0.9498 * moneyball_imp_test$free_bases_num +
  0.3222 * moneyball_imp_test$total_bases +
  0.08224 * moneyball_imp_test$total_bases_allowed -
  0.04188 * moneyball_imp_test$HR_over_OP




#subset of data set for the deliverable "Scored data file"
prediction <- moneyball_test[c("index", "P_TARGET_WINS")]

#####
#Note, this next function will output an Excel file in your work environment called write.xlsx.
#####

#Prediction File
write.xlsx(prediction,
           file = "Jorge_Fernandes_MoneyBall_Pred.xlsx",
           sheetName = "Predictions",
           col.names = TRUE)
