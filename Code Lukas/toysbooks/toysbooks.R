# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# library(devtools) 

library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)

library(tidyverse)

getwd()
bt <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
bt <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")

str(bt)

table(bt$controlGroup)

bt[,c(1:3)] <- NULL
bt$campaignValue[bt$campaignValue > 15] <- bt$campaignValue[bt$campaignValue > 15]/100 # correcting cases where discount precentages are given in two-digits instead of four (e.g. 15 instead of 1500)


table(bt$campaignValue)
table(bt$campaignUnit, bt$campaignValue, bt$controlGroup)

prop.table(table(bt$checkoutAmount>0)) #14.4% have a positive checkout amount 
prop.table(table(bt$checkoutDiscount>0)) #6.7% received a Discount (includes both treatment and control group)
prop.table(table(bt$checkoutDiscount>0, bt$controlGroup)) 
# [69.5% treatment=1,discountAmount=0]; [23.8% treatment=0,discountAmount=0]; [5.3% treatment=1,discountAmount>0], [1.4% treatment=0,discountAmount>0]


table(bt$controlGroup)
with(bt, prop.table(table(checkoutDiscount>0, controlGroup), margin=1)) # 25.5% of treatment group members (35533) received discount, 20.4% (7933) of control group members
with(bt, table(checkoutDiscount>0, controlGroup))


treatment_uplift_bt <- aggregate(checkoutAmount ~ controlGroup, data=bt, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=bt, mean)[2,2]
treatment_uplift_bt #the treatment gives an average uplift across the whole population of 0.9200081
aov_bt <- aov(checkoutAmount  ~ controlGroup, data=bt)
ttest_bt <- t.test(checkoutAmount ~ controlGroup, data=bt) # the differences in checkout amount are statistically significant!
ttest_bt


# feature engineering -----------------------------------------------------

bt$basketValue <- bt$checkoutAmount + bt$checkoutDiscount
bt$discountPercentage  <- bt$checkoutDiscount / bt$basketValue

names(bt)
bt <- bt[,c(1:5, 60, 91,92,8,9,6,7,10:59,61:90)]

prop.table(table(bt$basketValue>0)) # 15.4% have a positive basket value (sum of checkout amount and discount) >> this number should include all purchases!

summary(bt$checkoutDiscount)
summary(bt$discountPercentage)

# Discount Delta Function -------------------------------------------------

# THIS FUNCTION CALCULATES THE DIFFERENCE BETWEEN THE ACTUAL DISCOUNT AND THE DISCOUNT AS PRESCRIBED BY THE TREATMENT
# Positive difference means the person reveiced MORE discount than offered in the treatmet, 
# negative difference means the person received LESS discount 

bt$discountDelta <- numeric(nrow(bt))
condition1 <- (bt$campaignUnit=="PERCENT" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
condition2 <- (bt$campaignUnit=="CURRENCY" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
# pre-vectorization and allocation, and condition outsourcing taken from https://datascienceplus.com/strategies-to-speedup-r-code/

for (i in seq_along(1:nrow(bt))){
  if (condition1[i]) {
  bt$discountDelta[i] <- bt$checkoutDiscount[i] - (bt$basketValue[i] * (bt$campaignValue[i]/100))
  }
else if(condition2[i]) {
  bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
  }
}

table(bt$campaignUnit)
table(bt$campaignUnit, bt$campaignValue)


summary(bt$discountDelta)

table(bt$discountDelta>0,bt$controlGroup)
table(bt$discountDelta<0, bt$controlGroup)

table(bt$discountPercentage>0.5,bt$controlGroup, bt$basketValue>50)


#bt_exclude <- bt[(bt$controlGroup==1 & bt$checkoutDiscount > 0),]
bt <- bt[!(bt$controlGroup==1 & bt$checkoutDiscount > 0),]
bt <- bt[!(bt$controlGroup==0 & bt$discountDelta < 0),]
bt <- bt[!(bt$controlGroup==0 & bt$discountDelta < 0),]

hist(bt$discountDelta[bt$discountDelta>0], breaks=700)

