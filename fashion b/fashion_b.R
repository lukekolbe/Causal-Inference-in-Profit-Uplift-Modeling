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
library(tools4uplift)


set.seed(666)

getwd()
f_b <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionB.csv", sep=",")
#f_b <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_b <- read.csv("H:\\Applied Predictive Analytics\\Data\\fashion\\FashionA.csv", sep=",")


table(f_b$controlGroup)
str(f_b)

table(f_b$campaignMov, f_b$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
table(f_b$campaignValue) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700

with(f_b, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=f_b)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)

#with(f_b, prop.table(table(converted,controlGroup, campaignValue), margin=1))

table(f_b$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(f_b$checkoutAmount>0, f_b$controlGroup)) #~5% have a positive checkout amount

treatment_uplift_a <- aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[2,2]
treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
summary(aov(checkoutAmount  ~ controlGroup, data=f_b)) # the differences in checkout amount are statistically significant!
t.test(checkoutAmount ~ controlGroup, data=f_b) # the differences in checkout amount are statistically significant!

names(f_b)

table(f_b$campaignMov)

table(f_b$checkoutAmount>=5,f_b$controlGroup) #14.800 people qualified for actually using the discount of 20€ through achieving the minimum order value
with(f_b, prop.table(table(checkoutAmount>=5,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do


f_b$treatment <- numeric(nrow(f_b))
f_b$treatment <- ifelse(f_b$controlGroup==1, 0, 1)

f_b <- f_b[,c(4:9,94,63,10:62,64:93,1,2,3)] # sorting new for better visibility of important columns

#f_b_5 <- f_b[f_b$campaignValue==500,-which(names(f_b) %in% c("campaignUnit", "campaignTags", "trackerKey", "campaignId", "campaignValue"))] #separating the different treatment values
#f_b_0 <- f_b[f_b$campaignValue==0,-which(names(f_b) %in% c("campaignUnit", "campaignTags", "trackerKey", "campaignId", "campaignValue"))]
f_b <- f_b[f_b$campaignValue==2000,-which(names(f_b) %in% c("campaignUnit", "campaignTags", "trackerKey", "campaignId", "campaignValue"))] # only work with those with campaign-value of "2000" as they are the largest uniform group!



# stratification for production -------------------------------------------

# 
# train_indices_f_b <- list()
# 
# combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
# xtabs(~converted+controlGroup, f_b)
# sample_size_f_b <- as.numeric(xtabs(~converted+controlGroup, f_b))
# 
# 
# for(i in 1:4){
#   train_indices_f_b[[i]] <- sample(which(f_b$converted == combinations$Conversion[i] &
#                                            f_b$controlGroup == combinations$Treatment[i])
#                                    , size = round(0.75*sample_size_f_b[i]), replace=FALSE) 
# } 
# 
# 
# 
# trainIndex_f_b <- c(train_indices_f_b, recursive=TRUE)
# 
# trainData <- f_b[trainIndex_f_b,]
# testData  <- f_b[-trainIndex_f_b,]
# 

# 1ST ROUND SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

train_indices_f_b <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
xtabs(~converted+treatment, f_b)
sample_size_f_b <- as.numeric(xtabs(~converted+treatment, f_b))


for(i in 1:4){
  train_indices_f_b[[i]] <- sample(which(f_b$converted == combinations$Conversion[i] &
                                           f_b$treatment == combinations$Treatment[i])
                                   , size = round(0.4*sample_size_f_b[i]), replace=FALSE) 
} 



trainIndex_f_b <- c(train_indices_f_b, recursive=TRUE)

trainData_small <- f_b[trainIndex_f_b,] # temporarily the learning data is only a small partition!

# SAMPLE SPLITTING AND STRATIFICATION 2ND ROUND (FOR PRE-TRAINING) ---------------------------------------------------

train_indices_f_b2 <- list()

#combinations_f_b2 <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(1,0))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
sample_size_f_b2 <- as.numeric(xtabs(~converted+treatment, trainData_small))


for(i in 1:4){
  train_indices_f_b2[[i]] <- sample(which(trainData_small$converted == combinations$Conversion[i] &
                                            trainData_small$treatment == combinations$Treatment[i])
                                    , size = round(0.25*sample_size_f_b2[i]), replace=FALSE) 
} 



trainIndex_f_b2 <- c(train_indices_f_b2, recursive=TRUE)

trainData_f_b2 <- trainData_small[-trainIndex_f_b2,] # temporarily the train data is only a small partition!
testData_f_b2  <- trainData_small[trainIndex_f_b2,]

table(trainData_f_b2$checkoutAmount>0, trainData_f_b2$controlGroup)

summary(aov(checkoutAmount  ~ controlGroup, data=trainData_f_b2)) # checking statistical significance of the differences in checkout amount
t.test(checkoutAmount ~ controlGroup, data=trainData_f_b2) # checking statistical significance of the differences in checkout amount

aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[2,2] 
aggregate(checkoutAmount ~ controlGroup, data=trainData_f_b2, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=trainData_f_b2, mean)[2,2] 
#total population uplift is slightly lower than in the complete sample



# Average Treatment Effect (ATE) ---------------------------------------------------

experiment <- table(list("Control" = trainData_f_b2$controlGroup, "Converted" = trainData_f_b2$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_f_b2$converted[trainData_f_b2$controlGroup==0])) - mean(as.numeric(trainData_f_b2$converted[trainData_f_b2$controlGroup==1]))
mean(trainData_f_b2$checkoutAmount[trainData_f_b2$controlGroup==0]) - mean(trainData_f_b2$checkoutAmount[trainData_f_b2$controlGroup==1])

# or alternatively:
(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )




# DATA SAMPLE INVESTIGATION (DOES NOT WORK)-----------------------------------------------

# Check whether the data has been randomly assigned. This is an important assumptions in uplift modeling and, more generally, experimental designs. 
# To verify a random assignment, we have to check the balance of the A/B indicator.
# The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates.
# In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group

# n <- names(f_b)
# f <- as.formula(paste("controlGroup ~", paste(n[!n %in% c("controlGroup","converted")], collapse = " + "))) # checkBalance() throws an error with the syntax (controlGroup ~.-converted), so I save the formula separately. doesn't help.
# f
# 
# cb <- checkBalance(f, data = trainData_f_b2, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))
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
