# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# install.packages("tools4uplift")

library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)

# Laoding the Data set -----------------
set.seed(666)
f_a <- read.csv("FashionA.csv", sep=",")

# Decriptive Analysis ------------------
str(f_a)
table(f_a$controlGroup)

table(f_a$campaignMov, f_a$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
prop.table(table(f_a$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%

with(f_a, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=f_a)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)

#with(f_a, prop.table(table(converted,controlGroup, campaignValue), margin=1))

table(f_a$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(f_a$checkoutAmount>0, f_a$controlGroup)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)

# Drop columns with no information
f_a <- f_a[,-which(names(f_a) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TabSwitchPer.product.", "TimeToFirst.cart.","TabSwitchOnLastScreenCount","TotalTabSwitchCount"))] 
summary(f_a)
#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(f_a, anyNA)))
# Check % of NA Columns
colMeans(is.na(f_a))
# Drop the high NA percentage Columns
cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
                 "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
                 "TimeToCartAdd","SecondsSinceTabSwitch") # get rid of these
f_a=f_a[,! names(f_a) %in% cols.dont.want, drop=F]

#Create Dummy Variables from NA Columns
#f_a$HasSessionBefore <- ifelse(f_a$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0


#mean imputation for low NA percentage Columns
#colMeans(is.na(f_a))
#summary(f_a)
#str(f_a)

for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}

# Drop all Variables with Nulls
#for(i in 1:ncol(f_a)){
 # f_a[is.na(f_a[,i]),i] = NULL
#}

# Feature Engineering -------------------------------------------
f_a$treatment = numeric(nrow(f_a))
f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)

library(lubridate)
# create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond

f_a$month=as.factor(month(as_datetime(f_a$epochSecond)))
table(f_a$month)
str(f_a$month)

# Data set rearrangement 
#f_a <- f_a[,c(4:9,94,63,10:62,64:93,1,2,3)] # sorting new for better visibility of important columns

# Dropping further columns, we do not need anymore
f_a <- f_a[,-which(names(f_a) %in% c("controlGroup", "epochSecond"))] 

# Seperating the different treatments --> Later test with 2-Model-Approach if treatment effects are higher for different treatments
f_a_5 <- f_a[f_a$campaignValue==500,] 
f_a_0 <- f_a[f_a$campaignValue==0,]
f_a <- f_a[f_a$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!

install.packages("corrplot")
library(corrplot)

#colnames(f_a)
corrplot::corrplot(cor(f_a[,61:ncol(f_a)]),method= "ellipse")

# Expected Profit----------------------------
# need conversion 
f_a$expectedProfit
