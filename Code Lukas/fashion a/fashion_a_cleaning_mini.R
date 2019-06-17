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
f_a <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
#f_a <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_a <- read.csv("H:\\Applied Predictive Analytics\\Data\\FashionA.csv", sep=",")
f_a <- read.csv("/Users/asmir/Desktop/apa_data/FashionA.csv", sep=",")

# Feature Engineering & variable transformation -------------------------------------------------

f_a$z_var <- 0
f_a$z_var <- ifelse(f_a$label>0, 1, 0)
summary(f_a$z_var)

f_a$treatment = numeric(nrow(f_a))
f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)

# Drop columns with no information
f_a <- f_a[,-which(names(f_a) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0

for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}

f_a <- f_a[f_a$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!


f_a <- f_a[,which(names(f_a) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]


#  Splitting into train & test set ----------------------------------------

set.seed(101010)

strat_split <- stratified(f_a, c("treatment", "converted"), 0.667, bothSets=TRUE)
f_a.train <- as.data.frame(strat_split[[1]])
f_a.validate <- as.data.frame(strat_split[[2]])

#do SMOTE here, BEFORE the next split

# strat_trainsplit_small <- stratified(f_a.train, c("treatment", "converted"), 0.8, bothSets=TRUE)
# f_a.train_discard <- as.data.frame(strat_trainsplit_small[[1]]) # we cannot use this data, too many rows, to expensive to compute.
# f_a.train_small <- as.data.frame(strat_trainsplit_small[[2]])

#summary(aov(checkoutAmount  ~ treatment, data=f_a)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=f_a.train_small)) # checking statistical significance of the differences in checkout amount
summary(aov(checkoutAmount  ~ treatment, data=f_a.test_small)) # checking statistical significance of the differences in checkout amount

t.test(checkoutAmount ~ treatment, data=f_a.train_small) # checking statistical significance of the differences in checkout amount

table(f_a.train_small$checkoutAmount>0, f_a.train_small$treatment)

prop.table(table(f_a$converted))
prop.table(table(f_a.learn$converted))
prop.table(table(f_a.learn.sub$converted))

aggregate(checkoutAmount ~ treatment, data=f_a, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_a.learn, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.learn, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_a.learn.sub, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.learn.sub, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_a.train_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.train_small, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[1,2] 


# SMOTE SAMPLING ---------------------------------------------
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3648438/pdf/1471-2105-14-106.pdf
# needs work: https://beckernick.github.io/oversampling-modeling/
### split of a test/validation chunk, then do SMOTE on the training data, learn, predict on non-smote'd test data

n <- names(f_a.train)
f_smote_f_a <- as.formula(paste("converted ~",paste(n[!n %in% c("converted","checkoutAmount","treatment","label","z_var")], collapse = " + ")))

f_a.train$converted <- as.factor(f_a.train$converted)
f_a.train_SMOTE <- SMOTE(f_smote_f_a,f_a.train,perc.over = 400,perc.under = 500) # data about 10% bigger than before
#good: perc.over = 350,perc.under = 500

f_a.validate$converted <- as.factor(f_a.validate$converted)
f_a.validate_SMOTE <- SMOTE(f_smote_f_a,f_a.validate,perc.over = 400,perc.under = 500) # data about 10% bigger than before

write.csv2(f_a.train_SMOTE, "f_a.train_SMOTE.csv")
write.csv2(f_a.validate_SMOTE, "f_a.validate_SMOTE.csv")

# checking balance
prop.table(table(f_a.train$converted))
prop.table(table(f_a.train_SMOTE$converted))

prop.table(table(f_a.train_SMOTE$converted))
aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)
aggregate(checkoutAmount ~ treatment, data=f_a.train_SMOTE, mean)
aggregate(checkoutAmount ~ treatment, data=f_a, mean)

aggregate(converted ~ treatment, data=f_a.test_small, mean)
aggregate(converted ~ treatment, data=f_a.train_SMOTE, mean)
aggregate(converted ~ treatment, data=f_a, mean)

aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[1,2] 
with(f_a.train_SMOTE, prop.table(table(treatment,converted), margin=2)) 
with(f_a.train_SMOTE, prop.table(table(treatment,converted), margin=1))

with(f_a.test_small, prop.table(table(treatment,converted), margin=2)) 
with(f_a.test_small, prop.table(table(treatment,converted), margin=1))

t.test(checkoutAmount ~ treatment, data=f_a.test_small)
summary(aov(converted  ~ treatment, data=f_a.test_small))

t.test(checkoutAmount ~ treatment, data=f_a.train_SMOTE)
summary(aov(as.numeric(converted)  ~ treatment, data=f_a.train_SMOTE))
