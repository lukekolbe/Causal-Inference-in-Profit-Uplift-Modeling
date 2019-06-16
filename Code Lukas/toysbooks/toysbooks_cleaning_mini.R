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

set.seed(101010)

getwd()
b_t <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
#b_t <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("H:\\Applied Predictive Analytics\\Data\\BooksAndToys.csv", sep=",")

# Feature Engineering & variable transformation -------------------------------------------------

b_t$z_var <- 0
b_t$z_var <- ifelse(b_t$label>0, 1, 0)
summary(b_t$z_var)

b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)

# Drop columns with no information
b_t <- b_t[,-which(names(b_t) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0

for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}


#b_t <- b_t[b_t$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!


b_t <- b_t[,which(names(b_t) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]


#  Splitting into train & test set ----------------------------------------

set.seed(101010)#12

strat_split <- stratified(b_t, c("treatment", "converted"), 0.667, bothSets=TRUE)
b_t.train <- as.data.frame(strat_split[[1]])
b_t.validate <- as.data.frame(strat_split[[2]])

#do SMOTE here, BEFORE the next split

# strat_split_small <- stratified(b_t.learn.sub, c("treatment", "converted"), 0.5, bothSets=TRUE)
# b_t.train_small <- as.data.frame(strat_split_small[[1]])
# b_t.test_small <- as.data.frame(strat_split_small[[2]])

#summary(aov(checkoutAmount  ~ treatment, data=b_t)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=b_t.train_small)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=b_t.test_small)) # checking statistical significance of the differences in checkout amount

t.test(checkoutAmount ~ treatment, data=b_t.train_small) # checking statistical significance of the differences in checkout amount

table(b_t.train_small$checkoutAmount>0, b_t.train_small$treatment)

prop.table(table(b_t$converted))
prop.table(table(b_t.learn$converted))
prop.table(table(b_t.learn.sub$converted))

aggregate(checkoutAmount ~ treatment, data=b_t, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=b_t.learn, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.learn, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=b_t.learn.sub, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.learn.sub, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=b_t.train_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.train_small, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[1,2] 


# SMOTE SAMPLING ---------------------------------------------
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3648438/pdf/1471-2105-14-106.pdf

n <- names(b_t.train)
f_smote_b_t <- as.formula(paste("converted ~",paste(n[!n %in% c("converted","checkoutAmount","treatment","label","z_var")], collapse = " + ")))

b_t.train$converted <- as.factor(b_t.train$converted)
b_t.train_SMOTE <- SMOTE(f_smote_b_t,b_t.train,perc.over = 33 ,perc.under = 1600)### this setup decreases the size of the input by factor 2/3, while resulting in a split of 40/60 (c/nc)
#good
b_t.validate$converted <- as.factor(b_t.validate$converted)
b_t.validate_SMOTE <- SMOTE(f_smote_b_t,b_t.validate,perc.over = 33 ,perc.under = 1600)### this setup decreases the size of the input by factor 2/3, while resulting in a split of 40/60 (c/nc)

write.csv2(b_t.train_SMOTE, "b_t.train_SMOTE.csv")
write.csv2(b_t.validate_SMOTE, "b_t.validate_SMOTE.csv")

# checking balance
prop.table(table(b_t.train$converted))
prop.table(table(b_t.train_SMOTE$converted))

prop.table(table(b_t.train_SMOTE$converted))
aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)
aggregate(checkoutAmount ~ treatment, data=b_t.train_SMOTE, mean)
aggregate(checkoutAmount ~ treatment, data=b_t, mean)

aggregate(converted ~ treatment, data=b_t.test_small, mean)
aggregate(converted ~ treatment, data=b_t.train_SMOTE, mean)
aggregate(converted ~ treatment, data=b_t, mean)

aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[1,2] 
with(b_t.train_SMOTE, prop.table(table(treatment,converted), margin=2)) 
with(b_t.train_SMOTE, prop.table(table(treatment,converted), margin=1))

with(b_t.test_small, prop.table(table(treatment,converted), margin=2)) 
with(b_t.test_small, prop.table(table(treatment,converted), margin=1))

t.test(checkoutAmount ~ treatment, data=b_t.test_small)
summary(aov(converted  ~ treatment, data=b_t.test_small))

t.test(checkoutAmount ~ treatment, data=b_t.train_SMOTE)
summary(aov(as.numeric(converted)  ~ treatment, data=b_t.train_SMOTE))