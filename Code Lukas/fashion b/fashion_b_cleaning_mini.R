# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# install.packages("tools4uplift")
# install.packages("DMwR")
# install.packages("mlbench")
# install.packages("randomForest")
install.packages("splitstackshape")


library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library("DMwR") #for SMOTE
library(mlbench)
library(randomForest)
library(splitstackshape)

set.seed(111)

getwd()
f_b <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionB.csv", sep=",")
#f_b <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_b <- read.csv("H:\\Applied Predictive Analytics\\Data\\fashion\\FashionB.csv", sep=",")

# Feature Engineering & variable transformation -------------------------------------------------

f_b$z_var <- 0
f_b$z_var <- ifelse(f_b$label>0, 1, 0)
summary(f_b$z_var)

# Drop columns with no information
f_b <- f_b[,-which(names(f_b) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_b[, varlist][is.na(f_b[,varlist])] = 0

for(i in 1:ncol(f_b)){
  f_b[is.na(f_b[,i]), i] <- median(f_b[,i], na.rm = TRUE)
}

f_b$treatment = numeric(nrow(f_b))
f_b$treatment = ifelse(f_b$controlGroup==0, 1, 0)

#f_b <- f_b[f_b$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!


f_b <- f_b[,which(names(f_b) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]


#  Splitting into train & test set ----------------------------------------

set.seed(12)

strat_split <- stratified(f_b, c("treatment", "converted"), 0.8, bothSets=TRUE)
f_b.learn <- as.data.frame(strat_split[[1]])
f_b.learn.sub <- as.data.frame(strat_split[[2]])

strat_split_small <- stratified(f_b.learn.sub, c("treatment", "converted"), 0.5, bothSets=TRUE)
f_b.train_small <- as.data.frame(strat_split_small[[1]])
f_b.test_small <- as.data.frame(strat_split_small[[2]])

#summary(aov(checkoutAmount  ~ treatment, data=f_b)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=f_b.train_small)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=f_b.test_small)) # checking statistical significance of the differences in checkout amount

t.test(checkoutAmount ~ treatment, data=f_b.train_small) # checking statistical significance of the differences in checkout amount

table(f_b.train_small$checkoutAmount>0, f_b.train_small$treatment)

prop.table(table(f_b$converted))
prop.table(table(f_b.learn$converted))
prop.table(table(f_b.learn.sub$converted))

aggregate(checkoutAmount ~ treatment, data=f_b, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_b.learn, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b.learn, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_b.learn.sub, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b.learn.sub, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_b.train_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b.train_small, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_b.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b.test_small, mean)[1,2] 


# SMOTE SAMPLING ---------------------------------------------
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3648438/pdf/1471-2105-14-106.pdf

n <- names(f_b.train_small)
f_smote_f_b <- as.formula(paste("converted ~",paste(n[!n %in% c("converted","checkoutAmount","treatment","label","z_var")], collapse = " + ")))

f_b.train_small$converted <- as.factor(f_b.train_small$converted)
f_b.train_SMOTE <- SMOTE(f_smote_f_b,f_b.train_small,perc.over = 400,perc.under = 200)### this setup decreases the size of the input by factor 2/3, while resulting in a split of 40/60 (c/nc)

# checking balance
prop.table(table(f_b.train_small$converted))
prop.table(table(f_b.train_SMOTE$converted))

prop.table(table(f_b.train_SMOTE$converted))
aggregate(checkoutAmount ~ treatment, data=f_b.test_small, mean)
aggregate(checkoutAmount ~ treatment, data=f_b.train_SMOTE, mean)
aggregate(checkoutAmount ~ treatment, data=f_b, mean)

aggregate(converted ~ treatment, data=f_b.test_small, mean)
aggregate(converted ~ treatment, data=f_b.train_SMOTE, mean)
aggregate(converted ~ treatment, data=f_b, mean)

aggregate(checkoutAmount ~ treatment, data=f_b.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b.test_small, mean)[1,2] 
with(f_b.train_SMOTE, prop.table(table(treatment,converted), margin=2)) 
with(f_b.train_SMOTE, prop.table(table(treatment,converted), margin=1))

with(f_b.test_small, prop.table(table(treatment,converted), margin=2)) 
with(f_b.test_small, prop.table(table(treatment,converted), margin=1))

t.test(checkoutAmount ~ treatment, data=f_b.test_small)
summary(aov(converted  ~ treatment, data=f_b.test_small))

t.test(checkoutAmount ~ treatment, data=f_b.train_SMOTE)
summary(aov(as.numeric(converted)  ~ treatment, data=f_b.train_SMOTE))