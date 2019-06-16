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
#install.packages("BART")
library(BART)

getwd()
setwd("~/Desktop/apa_data/")
bt <- read.csv("BooksAndToys.csv", sep=",")

str(bt)

bt[,c(1:3)] <- NULL

bt$treatment = numeric(nrow(bt))

bt$treatment = ifelse(bt$controlGroup==0, 1, 0)

table(bt$treatment)

table(bt$campaignValue)

#View(bt)

prop.table(table(bt$checkoutAmount>0)) # 14.4% pos. checkoutAmount
prop.table(table(bt$checkoutDiscount>0)) # 6.7% received a Discount (treatment and control)
prop.table(table(bt$checkoutDiscount>0, bt$controlGroup)) 
# [69.5% treated, no discount]; [23.8% control, no discount]; 
# [5.3% treated, discount], [1.4% control, discount]

with(bt, prop.table(table(checkoutDiscount>0, controlGroup), margin=1)) # 25.5% of treated (35533) received discount, 20.4% (7933) of control group members
with(bt, table(checkoutDiscount>0, controlGroup))

#mean imputation for nulls
for(i in 1:ncol(bt)){
  bt[is.na(bt[,i]), i] <- mean(bt[,i], na.rm = TRUE)
}
# na finden 
colMeans(is.na(bt))
#mean imputation for NaNs
#for(i in 1:ncol(bt)){
 # bt[is.nan(bt[,i]), i] <- mean(bt[,i], nan.rm = TRUE)
#}
# does not work
#is.na(bt)

# feature creation --------------------------------------------------------
View(bt)
bt$campaignValue <- bt$campaignValue/100
bt$basketValue <- bt$checkoutAmount + bt$checkoutDiscount
bt$discountPercentage  <- bt$checkoutDiscount / bt$basketValue


# Discount Delta Function -------------------------------------------------

bt$discountDelta <- numeric(nrow(bt))

for (i in seq_along(1:nrow(bt))){
   if (bt$campaignUnit[i]=="PERCENT" & bt$checkoutDiscount[i] > 0) {
     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
   }else if(bt$campaignUnit[i]=="CURRENCY" & bt$checkoutDiscount[i] > 0) {
     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
   } 
}


#condition1 <- (bt$campaignUnit[i]=="PERCENT" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
#condition2 <- (bt$campaignUnit[i]=="CURRENCY" & bt$controlGroup==0 & bt$checkoutDiscount > 0)

#bt$discountDeltaPerc = numeric(nrow(bt))

#for (i in seq_along(1:nrow(bt))){
 # if (bt$campaignUnit[i]=="PERCENT"){
  #  bt$discountDeltaPerc[i] = bt$discountPercentage[i] - (bt$campaignValue[i]/100)
  #}else if (bt$campaignUnit[i]=="CURRENCY"){
   # bt$discountDeltaPerc[i] = bt$discountPercentage[i] - (bt$campaignValue[i]/bt$basketValue[i])
 # }
#}


#for (i in seq_along(1:nrow(bt))){
 # if (condition1[i]) {
  #bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
  #}
#else if(condition2[i]) {
 # bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
  #}
#}


summary(bt$discountDelta)

table(bt$discountDelta>0,bt$controlGroup)

table(bt$discountDelta<0, bt$controlGroup) # now false since weird cases removed

table(bt$discountPercentage>0.5,bt$controlGroup, bt$basketValue>50)


#bt_exclude <- bt[(bt$controlGroup==1 & bt$checkoutDiscount > 0),]
#bt <- bt[!(bt$controlGroup==1 & bt$checkoutDiscount > 0),] # Control Group hat Discount Code
#bt <- bt[!(bt$controlGroup==0 & bt$discountDelta < 0),] # Treated aber schlechteren Code verwendet
#bt <- bt[!(bt$controlGroup==1 & bt$discountDelta < 0),] # Control Group aber schlechteren Code verwendet

hist(bt$discountDelta[bt$discountDelta>0], breaks=700)
str(bt)

# Models  ---------------------------------------------------------------------

#install.packages("mlr")
library(mlr) 


cols.dont.want=c("campaignTags","campaignUnit", "campaignID", "trackerKey", "epochSecond", "campaignID") # get rid of these
bt=bt[,! names(bt) %in% cols.dont.want, drop=F]
str(bt)
library(grf)

# Data partitioning
# Partition the data into training and test set, using 25% of the data for testing 
# We want to stratify on both Conversion and Treatment to keep the distribution between train and testing equal
train_indices <- list()
combinations <- expand.grid(list("converted"=c(0,1), "treatment"= c(0,1)))
combinations

xtabs(~converted+treatment, bt)
sample_size <- as.numeric(xtabs(~converted+treatment, bt))
sample_size

for(i in 1:4){
  train_indices[[i]] <- sample( which(bt$converted == combinations$converted[i] &
                                        bt$treatment == combinations$treatment[i])
                                , size = round(0.75*sample_size[i])) 
}
train_indices
trainIndex <- c(train_indices, recursive=TRUE)

trainData <- bt[trainIndex,] 
str(trainData)
testData  <- bt[-trainIndex,]

summary(trainData[,c("converted","treatment")])
summary( testData[,c("converted","treatment")])

# Average Treatment Effect (ATE)

experiment = table(list("treatment" = bt$treatment, "converted" = bt$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
# for conversion the ATE is 0.04 (4%)
mean(bt$converted[bt$treatment==1]) - mean(bt$converted[bt$treatment==0])
# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )
# for revenue (checkoutAmount) the ATE is 3.2 (Dollars)
mean(bt$checkoutAmount[bt$treatment==1]) - mean(bt$checkoutAmount[bt$treatment==0])

# Check random assignment of groups ----------------------------

# This is an important assumptions in uplift modeling and, more generally, experimental designs. To verify a random assignment, we have to check the balance of the A/B indicator. The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates. In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group
library("uplift")
cb <- checkBalance(treatment~.-converted, data = trainData, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))

# Balance properties of first ten covariates 
# Be aware that the results are saved as a tensor or '3D matrix'.
dim(cb$results)
round(cb$results[,,], 2)
# The function automatically computes a chi-squared test for the conditional independence of the covariates to the treatment variable
cb$overall

# The test rejects the null hypothesis that the sample is balanced, i.e. truly randomized, at the 1% level. The reason is possibly that the customer were not assigned randomly to the treatment and control group, but based on some non-random procedure, e.g. an existing model or some decision rule.
# Think deeply about how this will impact your model!


# Uplift/Causal decision tree ------------
library(causalTree)
tree <- causalTree(converted~.-treatment, data = trainData, treatment = trainData$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = F, cv.Honest = F, split.Bucket = F, 
                   xval = NULL, cp = 0.005, minsize = 50, propensity = 0.5)

#opcp <- tree$cptable[,1][which.min(tree$cptable[,4])] # error

#opfit <- prune(tree, opcp) #error

rpart.plot(tree) # shows only one bin 0.04 with 100%

# casual tree on checkoutAmount
tree2 <- causalTree(checkoutAmount~.-treatment, data = trainData, treatment = trainData$treatment,
                   split.Rule = "TOT", cv.option = "TOT", split.Honest = F, cv.Honest = F, split.Bucket = F, 
                   xval = 5, cp = 0.005, minsize = 30, propensity = 0.5)
rpart.plot(tree2) # shows top bin of 3.3 with 100% --> similar to xperiment on checkoutAmount
# basketvalue <144 is most important variable w/ 97%

# Uplift Random Forest ----------------------------------------------------
#table(trainData$converted) # Binary
#table(trainData$treatment) # Binary

upliftRF <- upliftRF(converted ~ trt(treatment) +. -treatment,
                     data = trainData,
                     mtry = 5,
                     ntree = 100,
                     split_method = "KL", 
                     minsplit = 50,
                     verbose = TRUE)
# Error message want´s binary variables, but they are binary???
summary(upliftRF) 

# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred <- list()
pred_upliftRF <- predict(object = upliftRF, newdata = testData)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct1 gives an estimate for a person to convert when in the control group
head(pred_upliftRF) 

# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
pred[["upliftRF"]] <- pred_upliftRF[, 1] - pred_upliftRF[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred[["upliftRF"]])

# Performance Assessment for Uplift Models ----------------------------

# GRF -----------------------------------------------------------------
library("grf")
X <- bt[, !(colnames(bt) %in% c("checkoutAmount","treatment"))]
str(X)
Y <- bt$checkoutAmount
W <- bt$treatment

# Note that there are several ways to specify/ calculate the distribution of the test statistic of the independence test. In this case,
# we use Monte Carlo estimation (via approximate) with 500 repetitions.
cf <- causal_forest(X = X[trainIndex,], Y = Y[trainIndex], W=W[trainIndex],
                    num.trees = 100, 
                    mtry = 5,
                    sample.fraction = 0.5,
                    min.node.size = 50,
                    honesty = TRUE, honesty.fraction=NULL,
                    ci.group.size=2, compute.oob.predictions = TRUE, seed = 12)

# Predictions from a fitted CF model
pred_cf <- predict(cf, newdata = X[-trainIndex,], estimate.variance=FALSE)
head(pred_cf)
pred_cf <- pred_cf[,1]

# There seem to be many cases where the treatment decreases conversion!
# Remember that our treatment assignment was not random, so our results
# are likely biased.
summary(pred_cf)

# WILL ALS TARGET BINÄRE VARIABLE
#Performance Assessment for Uplift Models 
perf_cf <- uplift::performance(pr.y1_ct1 = pred_cf, pr.y1_ct0 = rep(0, times=length(pred_cf)), # causal forests estimate the difference in probability directly
                               y = Y[-trainIndex], ct = W[-trainIndex], direction = 1, groups = 10)
perf_cf # 9th Bin has uplift of 0.13 --> 13% more conversion

# Plot performance
#plot(perf_upliftRF[, "uplift"] ~ perf_upliftRF[, "group"], type ="l", xlab = "Decile (n*10% observations with top scores)", ylab = "Uplift")
#lines(perf_cf[, "uplift"] ~ perf_cf[, "group"], col = "red")
# It seems like the causal forest outperforms the uplift random forest for most decile choices. 

# We can compare the models using the Qini coefficients that we have calculated
# Similar to the AUC, the Qini coefficient may not be a good metric if the gain curves intersect
Qini[["causalForest"]] <- qini(perf_cf, plotit = FALSE)$Qini
Qini

#------------
fa <- read.csv("FashionA.csv", sep=",")
View(fa)
table(fa$checkoutAmount>0,fa$controlGroup)

fa[,c(1:3)] <- NULL

fa$treatment = numeric(nrow(fa))

fa$treatment = ifelse(fa$controlGroup==0, 1, 0)

## Average Treatment Effect (ATE)
experiment <- table(list("Treated" = fa$treatment, "Converted" = fa$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
mean(fa$converted[fa$treatment==1]) - mean(fa$converted[fa$treatment==0])
# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )

# Data partitioning
# Partition the data into training and test set, using 25% of the data for testing 
# We want to stratify on both Conversion and Treatment to keep the distribution between train and testing equal
train_indices <- list()
combinations <- expand.grid(list("converted"=c(0,1), "treatment"= c(0,1)))
combinations

xtabs(~converted+treatment, fa)
sample_size <- as.numeric(xtabs(~converted+treatment, fa))
sample_size

for(i in 1:4){
  train_indices[[i]] <- sample( which(fa$converted == combinations$converted[i] &
                                        fa$treatment == combinations$treatment[i])
                                , size = round(0.75*sample_size[i])) 
}
train_indices
trainIndex <- c(train_indices, recursive=TRUE)

trainData <- fa[trainIndex,] 
testData  <- fa[-trainIndex,]

summary(trainData[,c("converted","treatment")])
summary( testData[,c("converted","treatment")])


fb <- read.csv("FashionB.csv", sep=",")
NA_Columns = colMeans(is.na(fb))
NA_Columns
colnames_fb=colnames(fb)
na_columns_fb<- data.frame("Column"=colnames_fb, "na_percentage" = NA_Columns)
write.csv(na_columns_fb, file = "NA_Columns_Fashion_B.csv", row.names = FALSE)
