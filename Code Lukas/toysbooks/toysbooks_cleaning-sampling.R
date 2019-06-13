# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# install.packages("tools4uplift")
install.packages("DMwR")
install.packages("mlbench")
install.packages("randomForest")


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


set.seed(111)

getwd()
b_t <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("H:\\Applied Predictive Analytics\\Data\\books and toys\\BooksAndToys.csv", sep=",")

# Decriptive Analysis ------------------



table(b_t$campaignValue)
table(b_t$campaignUnit, b_t$campaignValue, b_t$controlGroup)

prop.table(table(b_t$checkoutAmount>0)) #14.4% have a positive checkout amount 
prop.table(table(b_t$checkoutDiscount>0)) #6.7% received a Discount (includes both treatment and control group)
prop.table(table(b_t$checkoutDiscount>0, b_t$controlGroup)) 
# [69.5% treatment=1,discountAmount=0]; [23.8% treatment=0,discountAmount=0]; [5.3% treatment=1,discountAmount>0], [1.4% treatment=0,discountAmount>0]


table(b_t$controlGroup)
with(b_t, prop.table(table(checkoutDiscount>0, controlGroup), margin=1)) # 25.5% of treatment group members (35533) received discount, 20.4% (7933) of control group members
with(b_t, table(checkoutDiscount>0, controlGroup))

aggregate(checkoutAmount ~ controlGroup, data=b_t, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=b_t, mean)[2,2]
#the treatment gives an average uplift across the whole population of 0.4892036
summary(aov(checkoutAmount  ~ controlGroup, data=b_t)) # the differences in checkout amount are statistically significant!
t.test(checkoutAmount ~ controlGroup, data=b_t) # the differences in checkout amount are statistically significant!





table(b_t$campaignMov, b_t$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
prop.table(table(b_t$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%

with(b_t, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=b_t)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)



# CLEANING ----------------------------------------------------------------

# Drop columns with no information
b_t <- b_t[,-which(names(b_t) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TabSwitchPer.product.", "TimeToFirst.cart.","TabSwitchOnLastScreenCount","TotalTabSwitchCount"))] 
#summary(b_t)

#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(b_t, anyNA)))
# Check % of NA Columns
colMeans(is.na(b_t))
# Drop the high NA percentage Columns
cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
                 "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
                 "TimeToCartAdd","SecondsSinceTabSwitch") # get rid of these
b_t=b_t[,! names(b_t) %in% cols.dont.want, drop=F]
# Setting specific Column Null Values to 0, works for specificly defined columns
#b_t$InitCartNonEmpty <- ifelse(b_t$InitCartNonEmpty == c("NA"), "0", b_t$InitCartNonEmpty)
#colMeans(is.na(b_t)) #check --> worked

#Create Dummy Variables from NA Columns
#b_t$HasSessionBefore <- ifelse(b_t$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0


#mean imputation for low NA percentage Columns
#colMeans(is.na(b_t))
#summary(b_t)
#str(b_t)

for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}

# Feature Engineering -------------------------------------------
b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)

# b_t$campaignValue[b_t$campaignValue > 15] <- b_t$campaignValue[b_t$campaignValue > 15]/100 # correcting cases where discount precentages are given in two-digits instead of four (e.g. 15 instead of 1500)
# #bt$campaignValue <- bt$campaignValue/100
# b_t$basketValue <- bt$checkoutAmount + bt$checkoutDiscount
# b_t$discountPercentage  <- bt$checkoutDiscount / bt$basketValue



# library(lubridate)
# # create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond
# 
# b_t$month=as.factor(month(as_datetime(b_t$epochSecond)))
# table(b_t$month)
# str(b_t$month)



# Seperating the different treatments --> Later test with 2-Model-Approach if treatment effects are higher for different treatments
# b_t_5 <- b_t[b_t$campaignValue==500,] 
# b_t_0 <- b_t[b_t$campaignValue==0,]
#b_t <- b_t[b_t$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!

# Dropping further columns, we do not need anymore
b_t <- b_t[,-which(names(b_t) %in% c("controlGroup","campaignMov","campaignValue", "NormalizedCartSum"))] 


# correlation test and removal of highly correlated variables ------------------


# find and reduce attributes that are highly corrected (ideally >0.75)

correlationMatrix <- cor(b_t[,-which(names(b_t) %in% c("converted", "treatment","checkoutAmount"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
# summarize the correlation matrix
#print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

b_t <- b_t[,-which(names(b_t) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
                                     "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
                                     "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
                                     "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
                                     "TotalClickCount","ViewCount","ViewsOn.overview.",
                                     "ViewsOn.product.","targetViewCount"))]



# 1ST ROUND SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

train_indices_b_t <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillstrÃÂ¶m data because the variable indicates control group membership
xtabs(~converted+treatment, b_t)
sample_size_b_t <- as.numeric(xtabs(~converted+treatment, b_t))

for(i in 1:4){
  train_indices_b_t[[i]] <- sample(which(b_t$converted == combinations$Conversion[i] &
                                           b_t$treatment == combinations$Treatment[i])
                                   , size = round(0.6*sample_size_b_t[i]), replace=FALSE) 
} 


trainIndex_b_t <- c(train_indices_b_t, recursive=TRUE)

trainData_b_t <- b_t[trainIndex_b_t,] # temporarily the learning data is only a small partition!
testData_b_t <- b_t[-trainIndex_b_t,] # temporarily the learning data is only a small partition!

# FEATURE SELECTION -------------------------------------------------------
#http://topepo.github.io/caret/recursive-feature-elimination.html#rfe
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/

set.seed(7)
# load the library
library(mlbench)
library(caret)

library(doParallel) 
cl2 <- makeCluster(8, type='PSOCK')
registerDoParallel(cl2)

# load the data

subsets <- c(5,7,8,9,10,12,15,20)

set.seed(123)
seeds <- vector(mode = "list", length = 9)
for(i in 1:8) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[9]] <- sample.int(1000, 1)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=8, seeds=seeds, saveDetails = TRUE, allowParallel=TRUE)
#control <- rfeControl(functions=lmFuncs, method="cv", number=10)

# run the RFE algorithm
set.seed(1)
# str(trainData_f_a2)
names(trainData_b_t)
# summary(trainData_f_a2$label)
system.time(rfe_b_t.results2 <- rfe(trainData_b_t[,-c(2,3,4,55)], trainData_b_t[,4], sizes=subsets, rfeControl=control))


saveRDS(rfe_b_t.results2, "rfe_b_t.results_label.rds")

stopCluster(cl2)


# summarize the results
print(rfe_b_t.results2)
# list the chosen features
predictors(rfe_b_t.results2)
# plot the results
plot(rfe_b_t.results2, type=c("g", "o"))


# Average Treatment Effect (ATE) ---------------------------------------------------

experiment <- table(list("Control" = trainData_b_t2$controlGroup, "Converted" = trainData_b_t2$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==0])) - mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==1]))
mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==0]) - mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==1])

# or alternatively:
(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )




# DATA SAMPLE INVESTIGATION (DOES NOT WORK)-----------------------------------------------

# Check whether the data has been randomly assigned. This is an important assumptions in uplift modeling and, more generally, experimental designs. 
# To verify a random assignment, we have to check the balance of the A/B indicator.
# The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates.
# In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group

# n <- names(b_t)
# f <- as.formula(paste("controlGroup ~", paste(n[!n %in% c("controlGroup","converted")], collapse = " + "))) # checkBalance() throws an error with the syntax (controlGroup ~.-converted), so I save the formula separately. doesn't help.
# f
# 
# cb <- checkBalance(f, data = trainData_b_t2, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))
# 
# # Balance properties of first ten covariates 
# # Be aware that the results are saved as a tensor or '3D matrix'.
# dim(cb$results)
# round(cb$results[,,], 2)
# # The function automatically computes a chi-squared test for the conditional independence of the covariates to the treatment variable
# cb$overall


# SMOTE SAMPLING TRYOUT ---------------------------------------------

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 10,
#                      repeats = 10,
#                      verboseIter = FALSE,
#                      sampling = "smote")
# 
# set.seed(42)
# model_rf_smote <- caret::train(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel,
#                                data = trainData_mens,
#                                method = "glm",
#                                preProcess = c("scale", "center"),
#                                trControl = ctrl)


test <- SMOTE()


