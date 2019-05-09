library(tidyverse)

getwd()
bt <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")

str(bt)

table(bt$controlGroup)

bt[,c(1:3)] <- NULL

bt$campaignValue <- bt$campaignValue/100
bt$basketValue <- bt$checkoutAmount + bt$checkoutDiscount
bt$discountPercentage  <- bt$checkoutDiscount / bt$basketValue

#bt_exclude <- bt[(bt$controlGroup==1 & bt$checkoutDiscount > 0),]
#bt <- bt[!(bt$controlGroup==1 & bt$checkoutDiscount > 0),]

# Discount Delta Function -------------------------------------------------

bt$discountDelta <- numeric(nrow(bt))

# for (i in seq_along(1:nrow(bt))){
#   if (bt$campaignUnit[i]=="PERCENT" & bt$checkoutDiscount[i] > 0) {
#     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
#   }
#   else if(bt$campaignUnit[i]=="CURRENCY" & bt$checkoutDiscount[i] > 0) {
#     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
#   }
# }



condition1 <- (bt$campaignUnit[i]=="PERCENT" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
condition2 <- (bt$campaignUnit[i]=="CURRENCY" & bt$controlGroup==0 & bt$checkoutDiscount > 0)


for (i in seq_along(1:nrow(bt))){
  if (condition1[i]) {
  bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
  }
else if(condition2[i]) {
  bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
  }
}

table(bt$campaignUnit)

summary(bt$discountDelta)

table(bt$discountDelta>0,bt$controlGroup)
table(bt$discountDelta<0, bt$controlGroup)

table(bt$discountPercentage>0.5,bt$controlGroup, bt$basketValue>50)



