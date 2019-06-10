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
f_b <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionB.csv", sep=",")
#f_b <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_b <- read.csv("H:\\Applied Predictive Analytics\\Data\\fashion\\FashionB.csv", sep=",")

# Decriptive Analysis ------------------
str(f_b)
table(f_b$controlGroup)

table(f_b$campaignMov, f_b$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
prop.table(table(f_b$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%

with(f_b, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=f_b)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)

#with(f_b, prop.table(table(converted,controlGroup, campaignValue), margin=1))

table(f_b$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(f_b$checkoutAmount>0, f_b$controlGroup)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)


treatment_uplift_a <- aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[2,2]
treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
summary(aov(checkoutAmount  ~ controlGroup, data=f_b)) # the differences in checkout amount are statistically significant!
t.test(checkoutAmount ~ controlGroup, data=f_b) # the differences in checkout amount are statistically significant!

names(f_b)

table(f_b$campaignMov)


# CLEANING ----------------------------------------------------------------

# Drop columns with no information
f_b <- f_b[,-which(names(f_b) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TabSwitchPer.product.", "TimeToFirst.cart.","TabSwitchOnLastScreenCount","TotalTabSwitchCount"))] 
summary(f_b)
#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(f_b, anyNA)))
# Check % of NA Columns
colMeans(is.na(f_b))
# Drop the high NA percentage Columns
cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
                 "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
                 "TimeToCartAdd","SecondsSinceTabSwitch") # get rid of these
f_b=f_b[,! names(f_b) %in% cols.dont.want, drop=F]
# Setting specific Column Null Values to 0, works for specificly defined columns
#f_b$InitCartNonEmpty <- ifelse(f_b$InitCartNonEmpty == c("NA"), "0", f_b$InitCartNonEmpty)
#colMeans(is.na(f_b)) #check --> worked

#Create Dummy Variables from NA Columns
#f_b$HasSessionBefore <- ifelse(f_b$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_b[, varlist][is.na(f_b[,varlist])] = 0


#mean imputation for low NA percentage Columns
#colMeans(is.na(f_b))
#summary(f_b)
#str(f_b)

for(i in 1:ncol(f_b)){
  f_b[is.na(f_b[,i]), i] <- median(f_b[,i], na.rm = TRUE)
}

# Feature Engineering -------------------------------------------
f_b$treatment = numeric(nrow(f_b))
f_b$treatment = ifelse(f_b$controlGroup==0, 1, 0)

# library(lubridate)
# # create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond
# 
# f_b$month=as.factor(month(as_datetime(f_b$epochSecond)))
# table(f_b$month)
# str(f_b$month)



# Dropping further columns, we do not need anymore
f_b <- f_b[,-which(names(f_b) %in% c("controlGroup", "epochSecond","campaignMov","campaignValue","label", "NormalizedCartSum"))] 


# correlation test and removal of highly correlated variables ------------------


# find and reduce attributes that are highly corrected (ideally >0.75)

correlationMatrix <- cor(f_b[,-which(names(f_b) %in% c("converted", "treatment","checkoutAmount"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
# summarize the correlation matrix
#print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

#### RESULT OF CORRELATION CHECK IN f_b; F_B; B_T DATA: 17 FEATURES WITH HIGH CORRELATION POTENTIAL
# c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
# "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
# "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
# "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
# "TotalClickCount","ViewCount","ViewsOn.overview.",
# "ViewsOn.product.","targetViewCount",)

f_b <- f_b[,-which(names(f_b) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
                                     "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
                                     "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
                                     "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
                                     "TotalClickCount","ViewCount","ViewsOn.overview.",
                                     "ViewsOn.product.","targetViewCount"))]

cor_complete <- cor(f_b)


# 1ST ROUND SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

train_indices_f_b <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
xtabs(~converted+treatment, f_b)
sample_size_f_b <- as.numeric(xtabs(~converted+treatment, f_b))

for(i in 1:4){
  train_indices_f_b[[i]] <- sample(which(f_b$converted == combinations$Conversion[i] &
                                           f_b$treatment == combinations$Treatment[i])
                                   , size = round(0.25*sample_size_f_b[i]), replace=FALSE) 
} 


trainIndex_f_b <- c(train_indices_f_b, recursive=TRUE)

trainData_small <- f_b[trainIndex_f_b,] # temporarily the learning data is only a small partition!


# SAMPLE SPLITTING AND STRATIFICATION 2ND ROUND (FOR PRE-TRAINING) ---------------------------------------------------

train_indices_f_b2 <- list()

#combinations_f_b2 <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(1,0))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
sample_size_f_b2 <- as.numeric(xtabs(~converted+treatment, trainData_small))


for(i in 1:4){
  train_indices_f_b2[[i]] <- sample(which(trainData_small$converted == combinations$Conversion[i] &
                                            trainData_small$treatment == combinations$Treatment[i])
                                    , size = round(0.5*sample_size_f_b2[i]), replace=FALSE) 
} 


trainIndex_f_b2 <- c(train_indices_f_b2, recursive=TRUE)

trainData_f_b2 <- trainData_small[-trainIndex_f_b2,] # temporarily the train data is only a small partition!
testData_f_b2  <- trainData_small[trainIndex_f_b2,]

table(trainData_f_b2$checkoutAmount>0, trainData_f_b2$treatment)

prop.table(table(trainData_small$converted))
prop.table(table(trainData_f_b2$converted))
prop.table(table(f_b$converted))

summary(aov(checkoutAmount  ~ treatment, data=trainData_f_b2)) # checking statistical significance of the differences in checkout amount
t.test(checkoutAmount ~ treatment, data=trainData_f_b2) # checking statistical significance of the differences in checkout amount

#### WHY IS THE ATE SO DIFFERENT BETWEEN TEST AND TRAIN DATA?! LOOKS LIKE AN ERROR IN STRATIFICATION!!!!
aggregate(checkoutAmount ~ treatment, data=f_b, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=trainData_f_b2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_f_b2, mean)[1,2] 
aggregate(checkoutAmount ~ treatment, data=testData_f_b2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=testData_f_b2, mean)[1,2] 

#total population uplift is slightly lower than in the complete sample



# FEATURE SELECTION -------------------------------------------------------
#http://topepo.github.io/caret/recursive-feature-elimination.html#rfe
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
#https://stackoverflow.com/questions/32290513/making-recursive-feature-elimination-using-caret-parallel-on-windows

set.seed(7)
# load the library
library(mlbench)
library(caret)

library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

# load the data
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5, seeds=seeds, saveDetails = TRUE)
#control <- rfeControl(functions=lmFuncs, method="cv", number=10)
subsets <- c(5,6,8,10,12,15)

set.seed(123)
seeds <- vector(mode = "list", length = 6)
for(i in 1:5) seeds[[i]] <- sample.int(1000, length(subsets) + 1)
seeds[[6]] <- sample.int(1000, 1)

# run the RFE algorithm
set.seed(1)
system.time(rfe_f_b.results <- rfe(trainData_f_b2[,-c(1,2)], trainData_f_b2[,1], sizes=subsets, rfeControl=control))

saveRDS(rfe_f_b.results, "rfe_f_b.results.rds")

stopCluster(cl)

rfe_f_b <- readRDS("/Volumes/kolbeluk/rfe_f_b.results.rds")

# summarize the results
print(rfe_f_b)
# list the chosen features
predictors(rfe_f_b)
# plot the results
plot(rfe_f_b, type=c("g", "o"))
rfe_var <- rfe_f_b$variables
rfe_var[rfe_var$Variables==15,]

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


