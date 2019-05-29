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

set.seed(666)
f_a <- read.csv("FashionA.csv", sep=",")
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

## Average Treatment Effect (ATE)
f_a$treatment = numeric(nrow(f_a))

f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)
experiment <- table(list("Treated" = f_a$treatment, "Converted" = f_a$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
mean(f_a$converted[f_a$treatment==1]) - mean(f_a$converted[f_a$treatment==0]) # 0.005% conversion uplift
# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )

mean(f_a$checkoutAmount[f_a$treatment==1]) - mean(f_a$checkoutAmount[f_a$treatment==0]) # 0.49 euro checkoutAmount uplift


treatment_uplift_a <- aggregate(checkoutAmount ~ controlGroup, data=f_a, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_a, mean)[2,2]
treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
summary(aov_a <- aov(checkoutAmount  ~ controlGroup, data=f_a)) # the differences in checkout amount are statistically significant!
ttest_a <- t.test(checkoutAmount ~ controlGroup, data=f_a) # the differences in checkout amount are statistically significant!
ttest_a

names(f_a)

table(f_a$campaignMov)

table(f_a$checkoutAmount>=105,f_a$controlGroup) # 105 --> Value to shop to be able to use discount code
#only 15k people qualify for the discountcode

#f_a$treatment <- numeric(nrow(f_a))
#f_a$treatment <- ifelse(f_a$controlGroup==1, 0, 1)

f_a <- f_a[,c(4:9,94,63,10:62,64:93,1,2,3)] # sorting new for better visibility of important columns
f_a <- f_a[,-which(names(f_a) %in% c("campaignUnit", "campaignTags", "trackerKey", "campaignId"))] #removing empty/useless factors to avoid issues with glm

f_a_5 <- f_a[f_a$campaignValue==500,] #separating the different treatment values
f_a_0 <- f_a[f_a$campaignValue==0,]
f_a <- f_a[f_a$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!

#NA Columns ----------------------------------
# Identify NA Columns
names(which(sapply(f_a, anyNA)))
# Check % of NA Columns
colMeans(is.na(f_a))
# Drop the high NA percentage Columns
cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
                 "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
                 "TimeToCartAdd","SecondsSinceTabSwitch") # get rid of these
f_a=f_a[,! names(f_a) %in% cols.dont.want, drop=F]
# Setting specific Column Null Values to 0, works for specificly defined columns
f_a$InitCartNonEmpty <- ifelse(f_a$InitCartNonEmpty == c("NA"), "0", f_a$InitCartNonEmpty)
colMeans(is.na(f_a)) #check --> worked

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0

#mean imputation for low NA percentage COlumns
for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}


# 1ST ROUND SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

train_indices_f_a <- list()

combinations <- expand.grid(list("converted"=c(0,1), "treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
xtabs(~converted+treatment, f_a)
sample_size_f_a <- as.numeric(xtabs(~converted+treatment, f_a))


for(i in 1:4){
  train_indices_f_a[[i]] <- sample(which(f_a$converted == combinations$converted[i] &
                                           f_a$treatment == combinations$treatment[i])
                                   , size = round(0.25*sample_size_f_a[i]), replace=FALSE) 
} 



trainIndex_f_a <- c(train_indices_f_a, recursive=TRUE)

trainData <- f_a[trainIndex_f_a,] # temporarily the train data is only a small partition!
testData  <- f_a[-trainIndex_f_a,]

# SAMPLE SPLITTING AND STRATIFICATION 2ND ROUND (FOR PRE-TRAINING) ---------------------------------------------------


#train_indices_f_a2 <- list()

#combinations_f_a2 <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(1,0))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
#sample_size_f_a2 <- as.numeric(xtabs(~converted+controlGroup, trainData))


#for(i in 1:4){
 # train_indices_f_a2[[i]] <- sample(which(trainData$converted == combinations$Conversion[i] &
  #                                          trainData$controlGroup == combinations$Treatment[i])
   #                                 , size = round(0.25*sample_size_f_a2[i]), replace=FALSE) 
#} 



#trainIndex_f_a2 <- c(train_indices_f_a2, recursive=TRUE)

#trainData_f_a2 <- trainData[-trainIndex_f_a2,] # temporarily the train data is only a small partition!
#testData_f_a2  <- trainData[trainIndex_f_a2,]

table(trainData$checkoutAmount>0, trainData$treatment)

summary(aov_a <- aov(checkoutAmount  ~ treatment, data=trainData)) # the differences in checkout amount are STILL statistically significant!
t.test(checkoutAmount ~ treatment, data=trainData) # the differences in checkout amount are still statistically significant, but less so compared to the total population

aggregate(checkoutAmount ~ treatment, data=trainData, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData, mean)[1,2] 
# 0.48 euro uplift


# Average Treatment Effect (ATE) ---------------------------------------------------

experiment <- table(list("Control" = trainData_f_a2$controlGroup, "Converted" = trainData_f_a2$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==0])) - mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==1]))
mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==0]) - mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==1])

# or alternatively:
(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )




# DATA SAMPLE INVESTIGATION (DOES NOT WORK)-----------------------------------------------

# Check whether the data has been randomly assigned. This is an important assumptions in uplift modeling and, more generally, experimental designs. 
# To verify a random assignment, we have to check the balance of the A/B indicator.
# The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates.
# In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group

# n <- names(f_a)
# f <- as.formula(paste("controlGroup ~", paste(n[!n %in% c("controlGroup","converted")], collapse = " + "))) # checkBalance() throws an error with the syntax (controlGroup ~.-converted), so I save the formula separately. doesn't help.
# f
# 
# cb <- checkBalance(f, data = trainData_f_a2, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))
# 
# # Balance properties of first ten covariates 
# # Be aware that the results are saved as a tensor or '3D matrix'.
# dim(cb$results)
# round(cb$results[,,], 2)
# # The function automatically computes a chi-squared test for the conditional independence of the covariates to the treatment variable
# cb$overall

# CARET SMOTE SAMPLING TRYOUT ---------------------------------------------

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
# 

# Causal Tree on checkoutAmount ------------------------------------------------------

str(trainData)

n <- names(trainData)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("campaignMov", "campaignValue","checkoutDiscount","controlGroup","checkoutAmount",
                                                            "epochSecond","label","aborted", "converted","confirmed","ViewedBefore.cart.", 
                                                            "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount")], collapse = " + ")))


tree_f_a1 = NULL
tree_f_a1 <- causalTree(f, data = trainData, treatment = trainData$treatment==1,
                        split.Rule = "TOT", cv.option = "TOT",  cv.Honest = F, split.Bucket = T, minbucket=2,
                        xval = 5, cp = 0.00017, minsize = 30)   # xval = 5, , propensity = 0.5, split.Honest = T
#cp = 0.0002 has decent size
# vector memory exhausted (limit reached?)

rpart.plot(tree_f_a1) #0.49 one node --> ARGHHHHHH
summary(tree_f_a1)
tree_f_a1$cptable #### MODELING ON CHECKOUT AMOUNT GIVES NO GOOD RESULTS! the cross validation error INCREASES with any split

# Causal Tree on label (transformed according to Gubela) ------------------------------------------------------

#f2 <- as.formula(paste("label ~", paste(n[!n %in% c("campaignMov", "campaignValue","checkoutDiscount","controlGroup","converted","checkoutAmount",
                                                  #  "epochSecond","label","ViewedBefore.cart.", 
                                                  #  "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount", "treatment")], collapse = " + ")))
#f2

#tree_f_a.1 <- causalTree(f2, data = trainData_f_a2, treatment = treatment,
                    #     split.Rule = "TOT", cv.option = "TOT",  cv.Honest = F, split.Bucket = F, minbucket=2,
                    #     xval = 5, cp = 0.0004, minsize = 30)   # xval = 5, , propensity = 0.5, split.Honest = T

#rpart.plot(tree_f_a.1)
#summary(tree_f_a.1)

#tree_f_a.1$cptable

#opcp <- tree_f_a.1$cptable[,1][which.min(tree_f_a$cptable[,3])] #BADS example minimizes cross validation error >> no results. If rel error is minimized, the results seem more logical.
#opfit <- prune(tree_f_a, cp=opcp)
#rpart.plot(opfit) ## this procedure always gives only one node for regression >> nonsense

pred= list()
pred_cT_f_a <- predict(object = tree_f_a1, newdata = testData)
str(pred_cT_f_a)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct0 gives an estimate for a person to convert when in the control group
head(pred_cT_f_a) 
summary(pred_cT_f_a)

# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
pred_f_a[["tree_f_a1"]] <- pred_cT_f_a[, 1] - pred_cT_f_a[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred_mens[["CausalTree"]])
head(pred_mens)


# UPLIFT RF --------------------------------------------------------
# str(trainData_mens)
# str(trainData_womens)
# table(trainData$z_var2)
# table(testData$z_var2)

f3 <- as.formula(paste("converted ~", paste(n[!n %in% c("campaignMov", "campaignValue","checkoutDiscount", "controlGroup",
                                                        "epochSecond","label","converted","checkoutAmount","confirmed","aborted","ViewedBefore.cart.", 
                                                        "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount")], collapse = " + ")))
f3
trainData2=trainData
cols.dont.want=c("campaignMov", "campaignValue","checkoutDiscount", "controlGroup",
                 "epochSecond","label","checkoutAmount","confirmed","aborted","ViewedBefore.cart.", 
                 "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount") # get rid of these
trainData2=trainData2[,! names(trainData2) %in% cols.dont.want, drop=F]

upliftRF_f_a2 <- upliftRF(converted ~ trt(treatment) +. -treatment ,
                          data = trainData2,
                          mtry = 9,
                          ntree = 300,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_f_a2)
### ONLY WORKS WITH BINARY TARGET
#Error in upliftRF.default(x = x, y = y, ct = ct, ...) : 
# uplift: upliftRF supports only binary response variables. Aborting...


# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF_f_a2, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred_uplift <- list()
pred_upliftRF_f_a2 <- predict(object = upliftRF_f_a2, newdata = testData)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct1 gives an estimate for a person to convert when in the control group
head(pred_upliftRF_mens) 
summary(pred_upliftRF_mens) 


# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
pred_mens[["upliftRF"]] <- pred_upliftRF_mens[, 1] - pred_upliftRF_mens[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred_mens[["upliftRF"]])
head(pred_mens)









# TWO MODEL APPROACH (REGRESSION AND DECISION TREES) ---------------------------------------------------------------

#### THIS IS MY OWN INTERPRETATION OF HOW THE MODEL WORKS
#### MIGHT VERY WELL BE VERY WRONG!

indx <- t(data.frame(lapply(trainData_f_a2, function(x) any(is.na(x)))))
names(indx)
names(indx[,])
indx[indx[,1=="TRUE"],]
#names of columns that contain TRUE

test <- trainData_f_a2[,apply(trainData_f_a2, 2, anyNA)==FALSE]

f
str(trainData_f_a2)
summary(trainData_f_a2)

data_treat=trainData_f_a2[trainData_f_a2$controlGroup==0,]
data_control=trainData_f_a2[trainData_f_a2$controlGroup==1,]

debug_contr_error(data_treat)

glm_f_a_treat <- glm(f,family = gaussian, data=trainData_f_a2[trainData_f_a2$controlGroup==1,], na.action=na.pass)
glm_f_a_contr <- glm(f, family = gaussian, data=trainData_f_a2[trainData_f_a2$controlGroup==1,], na.action=na.pass)

summary(glm_f_a_treat)
summary(glm_f_a_contr)

library(rpart)
rpart_contr = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=control, cp=0.0017, xval=10, model=TRUE)
prp(rpart)
summary(rpart)

rpart_men = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=trainData_mens[trainData_mens$treatment==1,], cp=0.0017, xval=10, model=TRUE)
prp(rpart_men)
summary(rpart_men)







# Causal Boosting ---------------------------------------------------------

library("parallelMap")
parallelStartSocket(3) #level = "causalLearning::causalBoosting"
library("parallel")
RNGkind("L'Ecuyer-CMRG")
clusterSetRNGStream(iseed = 1234567)

indx <- t(data.frame(lapply(trainData_f_a2, function(x) any(is.na(x)))))
names(indx)
names(indx[,])
indx[indx[,1=="TRUE"],]
#names of columns that contain TRUE

test <- trainData_f_a2[,apply(trainData_f_a2, 2, anyNA)==FALSE]



causalboost_f_a <- causalBoosting(test[,-which(names(test) %in% c("campaignMov", "campaignValue","checkoutDiscount","controlGroup","converted","checkoutAmount", "treatment",
                                                                  "epochSecond","label","ViewedBefore.cart.","TabSwitchPer.product.","TimeToFirst.cart.","SecondsSinceFirst.cart.","SecondsSinceTabSwitch","TabSwitchOnLastScreenCount"))],
                                  tx=test$treatment, 
                                  y=test$checkoutAmount)


parallelStop()


# Performance Assessment for Uplift Models  ---------------------------------------------

# Equivalent to the standard model lift, we can calculate the uplift for the sample deciles

treatment_effect_order_mens <- order(pred[['upliftRF']], decreasing=TRUE)
treatment_effect_groups_mens <- cbind(testData[treatment_effect_order, c("Conversion","Treatment")],               "effect_estimate"=pred[["upliftRF"]][treatment_effect_order])

head(treatment_effect_groups, 10)

# We cannot calculate the true treatment effect per person, but per group
treatment_effect_groups$Decile <- cut(treatment_effect_groups$effect_estimate, breaks = 10, labels=FALSE)
head(treatment_effect_groups)
tail(treatment_effect_groups, 4)

treatment_groups <- aggregate(treatment_effect_groups[,c("Conversion","effect_estimate")], 
                              by=list("Decile"=treatment_effect_groups$Decile, "Treatment"=treatment_effect_groups$Treatment), 
                              FUN=mean)
# Conversion of customer without a treatment/coupon ranked by prediction
{plot(treatment_groups$Conversion[10:1], type='l')
  # Conversion of customer with a treatment/coupon ranked by prediction
  lines(treatment_groups$Conversion[20:11], type='l', col="red")}
## -> The uplift is the area between the curves
treatment <- treatment_groups$Conversion[20:11] - treatment_groups$Conversion[10:1]

# {uplift} has a function to calculate the Qini coefficient
# Argument direction specifies whether we aim to maximize (P_treatment - P_control) or (P_control - P_treatment), or in other words
# whether we aim for a high (purchase) probability or low (churn) probability

perf_upliftRF <- uplift::performance(pr.y1_ct1 = pred_upliftRF[, 1], pr.y1_ct0 = pred_upliftRF[, 2], 
                                     # with/without treatment prob.
                                     y = testData$Conversion, ct = testData$Treatment, # outcome and treatment indicators
                                     direction = 1, # maximize (1) or minimize (2) the difference? 
                                     groups = 10)

perf_upliftRF
# Plot uplift random forest performance
plot(perf_upliftRF[, "uplift"] ~ perf_upliftRF[, "group"], type ="l", xlab = "Decile (n*10% observations with top scores)", ylab = "Uplift")
plot(treatment, col='red')

# The Qini coefficient (derived from the Gini coefficient to measure the deviation from an equal distribution) is 
# defined as the area between the incremental gains curve of the model and the area under the diagonal resulting from random targeting
# in relation to the percentage of the population targeted.
Qini_upliftRF <- qini(perf_upliftRF, plotit = TRUE) 

Qini <- list()
Qini[["upliftRF"]] <- Qini_upliftRF$Qini
# The results show that it is efficient to target the 70% of customers for which the model predictions are highest with our campaign (under the assumption that there is no budget constraint). Our model delivers much better results than random targeting which is represented in the red diagonal line here.


