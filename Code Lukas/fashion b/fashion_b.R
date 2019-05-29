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
fashion_b <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/FashionB.csv", sep=",")
fashion_b <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/FashionB.csv", sep=",")

str(fashion_b)

table(fashion_b$controlGroup)

fashion_b[,c(1:3)] <- NULL
#fashion_b$campaignValue[fashion_b$campaignValue > 15] <- fashion_b$campaignValue[fashion_b$campaignValue > 15]/100 # correcting cases where discount precentages are given in two-digits instead of four (e.g. 15 instead of 1500)


table(fashion_b$campaignValue) #campaign value ALWAYS 500 CURRENCY UNITS, except for ~1000 where it is 0
table(fashion_b$campaignValue, fashion_b$controlGroup)

table(fashion_b$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(fashion_b$checkoutAmount>0)) #9.3% have a positive checkout amount 


table(fashion_b$controlGroup)
with(fashion_b, prop.table(table(checkoutDiscount>0, controlGroup), margin=1)) # 25.5% of treatment group members (35533) received discount, 20.4% (7933) of control group members
with(fashion_b, table(checkoutDiscount>0, controlGroup))

treatment_uplift_b <- aggregate(checkoutAmount ~ controlGroup, data=fashion_b, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=fashion_b, mean)[2,2]
treatment_uplift_b #the treatment gives an average uplift across the whole population of 0.4892036
aov_b <- aov(checkoutAmount  ~ controlGroup, data=fashion_b)
ttest_b <- t.test(checkoutAmount ~ controlGroup, data=fashion_b) # the differences in checkout amount are statistically significant!
ttest_b

# feature engineering -----------------------------------------------------

#fashion_b$basketValue <- fashion_b$checkoutAmount + fashion_b$checkoutDiscount
#fashion_b$discountPercentage  <- fashion_b$checkoutDiscount / fashion_b$basketValue

#prop.table(table(fashion_b$basketValue>0)) # 15.4% have a positive basket value (sum of checkout amount and discount) >> this number should include all purchases!


names(fashion_b)
fashion_b <- fashion_b[,c(1:5, 60,8,9,6,7,10:59,61:90)]


summary(fashion_b$checkoutDiscount)
summary(fashion_b$discountPercentage)

# Discount Delta Function -------------------------------------------------

# THIS FUNCTION CALCULATES THE DIFFERENCE BETWEEN THE ACTUAL DISCOUNT AND THE DISCOUNT AS PRESCRIBED BY THE TREATMENT
# Positive difference means the person reveiced MORE discount than offered in the treatmet, 
# negative difference means the person received LESS discount 

fashion_b$discountDelta <- numeric(nrow(fashion_b))
condition1 <- (fashion_b$campaignUnit=="PERCENT" & fashion_b$controlGroup==0 & fashion_b$checkoutDiscount > 0)
condition2 <- (fashion_b$campaignUnit=="CURRENCY" & fashion_b$controlGroup==0 & fashion_b$checkoutDiscount > 0)
# pre-vectorization and allocation, and condition outsourcing taken from https://datascienceplus.com/strategies-to-speedup-r-code/

for (i in seq_along(1:nrow(fashion_b))){
  if (condition1[i]) {
    fashion_b$discountDelta[i] <- fashion_b$checkoutDiscount[i] - (fashion_b$basketValue[i] * (fashion_b$campaignValue[i]/100))
  }
  else if(condition2[i]) {
    fashion_b$discountDelta[i] <- fashion_b$checkoutDiscount[i] - fashion_b$campaignValue[i]
  }
}

table(fashion_b$campaignUnit)
table(fashion_b$campaignUnit, fashion_b$campaignValue)


summary(fashion_b$discountDelta)

table(fashion_b$discountDelta>0,fashion_b$controlGroup)
table(fashion_b$discountDelta<0, fashion_b$controlGroup)

table(fashion_b$discountPercentage>0.5,fashion_b$controlGroup, fashion_b$basketValue>50)


#fashion_b_exclude <- fashion_b[(fashion_b$controlGroup==1 & fashion_b$checkoutDiscount > 0),]
fashion_b <- fashion_b[!(fashion_b$controlGroup==1 & fashion_b$checkoutDiscount > 0),]
fashion_b <- fashion_b[!(fashion_b$controlGroup==0 & fashion_b$discountDelta < 0),]
fashion_b <- fashion_b[!(fashion_b$controlGroup==0 & fashion_b$discountDelta < 0),]

hist(fashion_b$discountDelta[fashion_b$discountDelta>0], breaks=700)

