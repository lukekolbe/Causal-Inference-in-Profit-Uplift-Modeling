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
f_a <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
#f_a <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_a <- read.csv("H:\\Applied Predictive Analytics\\Data\\fashion\\FashionA.csv", sep=",")


# variable transformation -------------------------------------------------

f_a$z_var <- 0
f_a$z_var <- ifelse(f_a$label>0, 1, 0)
summary(f_a$z_var)
          
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
          
          
          treatment_uplift_a <- aggregate(checkoutAmount ~ controlGroup, data=f_a, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_a, mean)[2,2]
          treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
          summary(aov(checkoutAmount  ~ controlGroup, data=f_a)) # the differences in checkout amount are statistically significant!
          t.test(checkoutAmount ~ controlGroup, data=f_a) # the differences in checkout amount are statistically significant!
          
          names(f_a)
          
          table(f_a$campaignMov)
          
          table(f_a$checkoutAmount>=105,f_a$controlGroup)#14.800 people qualified for actually using the discount of 20â¬ through achieving the minimum order value
          with(f_a, prop.table(table(checkoutAmount>=105,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do

# Drop columns with no information----------------------------------------------------------------
f_a <- f_a[,-which(names(f_a) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

#NA Columns ---------------------------------------------------
# # Identify NA Columns
# names(which(sapply(f_a, anyNA)))
# # Check % of NA Columns
# colMeans(is.na(f_a))
# # Drop the high NA percentage Columns
# cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
#                  "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
#                  "TimeToCartAdd","SecondsSinceTabSwitch",
#                  "TotalTabSwitchCount", "TabSwitchOnLastScreenCount", "TabSwitchPer.product.") # get rid of these because at least in ONE dataset the NA ration is >0.8
# f_a=f_a[,! names(f_a) %in% cols.dont.want, drop=F]
# Setting specific Column Null Values to 0, works for specificly defined columns
#f_a$InitCartNonEmpty <- ifelse(f_a$InitCartNonEmpty == c("NA"), "0", f_a$InitCartNonEmpty)
#colMeans(is.na(f_a)) #check --> worked

#Create Dummy Variables from NA Columns
#f_a$HasSessionBefore <- ifelse(f_a$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0

for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}

# Feature Engineering -------------------------------------------
f_a$treatment = numeric(nrow(f_a))
f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)

# library(lubridate)
# # create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond
# f_a$month=as.factor(month(as_datetime(f_a$epochSecond)))
# table(f_a$month)
# str(f_a$month)

# Seperating the different treatments --> Later test with 2-Model-Approach if treatment effects are higher for different treatments
# f_a_5 <- f_a[f_a$campaignValue==500,] 
# f_a_0 <- f_a[f_a$campaignValue==0,]
f_a <- f_a[f_a$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!

# correlation test and removal of highly correlated variables ------------------

# Dropping further columns, we do not need anymore
#f_a <- f_a[,-which(names(f_a) %in% c("controlGroup","campaignMov","campaignValue","label","NormalizedCartSum"))] 

# find and reduce attributes that are highly corrected (ideally >0.75)
correlationMatrix <- cor(f_a[,-which(names(f_a) %in% c("converted", "treatment","checkoutAmount"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
# summarize the correlation matrix
#print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

#### RESULT OF CORRELATION CHECK IN F_A; F_B; B_T DATA: 17 FEATURES WITH HIGH CORRELATION POTENTIAL
# c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
# "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
# "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
# "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
# "TotalClickCount","ViewCount","ViewsOn.overview.",
# "ViewsOn.product.","targetViewCount",)

f_a <- f_a[,-which(names(f_a) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
                                     "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
                                     "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
                                     "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
                                     "TotalClickCount","ViewCount","ViewsOn.overview.",
                                     "ViewsOn.product.","targetViewCount"))]
### this list is the results of comparing which columns were flagged via findCorrelation() in all three datasets, see data_correlation_selection.xlsx
#cor_complete <- cor(f_a)


# final selection after comparing importances -----------------------------
### this makes any code below obsolete when creating samples for actual model building
#comparing upliftRF, RFE, NIV values, building average ranks, picking top 20

f_a <- f_a[,which(names(f_a) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]



# classic stratification from BADS 1st round ----------------------------------------

# train_indices_f_a <- list()
# 
# combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
# xtabs(~converted+treatment, f_a)
# sample_size_f_a <- as.numeric(xtabs(~converted+treatment, f_a))
# 
# for(i in 1:4){
#   train_indices_f_a[[i]] <- sample(which(f_a$converted == combinations$Conversion[i] &
#                                            f_a$treatment == combinations$Treatment[i])
#                                    , size = round(0.15*sample_size_f_a[i]), replace=FALSE) 
# } 
# 
# 
# trainIndex_f_a <- c(train_indices_f_a, recursive=TRUE)
# 
# trainData_small <- f_a[trainIndex_f_a,] # temporarily the learning data is only a small partition!


# SAMPLE SPLITTING AND STRATIFICATION 2ND ROUND (FOR PRE-TRAINING) ---------------------------------------------------

# train_indices_f_a2 <- list()
# 
# #combinations_f_a2 <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(1,0))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
# sample_size_f_a2 <- as.numeric(xtabs(~converted+treatment, trainData_small))
# 
# 
# for(i in 1:4){
#   train_indices_f_a2[[i]] <- sample(which(trainData_small$converted == combinations$Conversion[i] &
#                                             trainData_small$treatment == combinations$Treatment[i])
#                                     , size = round(0.5*sample_size_f_a2[i]), replace=FALSE) 
# } 
# 
# 
# trainIndex_f_a2 <- c(train_indices_f_a2, recursive=TRUE)
# 
# trainData_f_a2 <- trainData_small[-trainIndex_f_a2,] # temporarily the train data is only a small partition!
# testData_f_a2  <- trainData_small[trainIndex_f_a2,]
# 
# table(trainData_f_a2$checkoutAmount>0, trainData_f_a2$treatment)
# 
# prop.table(table(trainData_small$converted))
# prop.table(table(trainData_f_a2$converted))
# prop.table(table(f_a$converted))
# 
# summary(aov(checkoutAmount  ~ treatment, data=trainData_f_a2)) # checking statistical significance of the differences in checkout amount
# t.test(checkoutAmount ~ treatment, data=trainData_f_a2) # checking statistical significance of the differences in checkout amount
# 
# #### WHY IS THE ATE SO DIFFERENT BETWEEN TEST AND TRAIN DATA?! LOOKS LIKE AN ERROR IN STRATIFICATION!!!!
# aggregate(checkoutAmount ~ treatment, data=f_a, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=trainData_f_a2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_f_a2, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=testData_f_a2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=testData_f_a2, mean)[1,2] 
# 
# #total population uplift is slightly lower than in the complete sample
# 
# 

# RFE FEATURE SELECTION -------------------------------------------------------
#http://topepo.github.io/caret/recursive-feature-elimination.html#rfe
#https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
#https://stackoverflow.com/questions/32290513/making-recursive-feature-elimination-using-caret-parallel-on-windows

set.seed(7)
# load the library
library(mlbench)
library(caret)

library(doParallel) 
cl <- makeCluster(8, type='PSOCK')
registerDoParallel(cl)

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
# names(trainData_f_a2)
# summary(trainData_f_a2$label)
system.time(rfe_f_a.results2 <- rfe(trainData_f_a2[,-c(2,3,4,55)], trainData_f_a2[,3], sizes=subsets, rfeControl=control))
# on label

saveRDS(rfe_f_a.results2, "rfe_f_a.results_label.rds")

stopCluster(cl)

rfe_f_a <- readRDS("/Volumes/kolbeluk/rfe_f_a.results.rds")

# summarize the results
print(rfe_f_a.results2)
# list the chosen features
predictors(rfe_f_a.results2)
# plot the results
plot(rfe_f_a.results2, type=c("g", "o"))
rfe_var <- rfe_f_b$variables
rfe_var[rfe_var$Variables==15,]



# Uplift NIV --------------------------------------------------------------


n <- names(f_a)
f_niv_fa <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))

fa_niv <- niv(f_niv_fa, f_a, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
    nbins = 10, continuous = 4, plotit = TRUE)

fa_niv$niv
fa_niv$nwoe



# Average Treatment Effect (ATE) ---------------------------------------------------

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==0])) - mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==1]))
mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==0]) - mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==1])

# or alternatively:
experiment <- table(list("Control" = trainData_f_a2$controlGroup, "Converted" = trainData_f_a2$converted))
experiment

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



# SMOTE SAMPLING ---------------------------------------------


#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3648438/pdf/1471-2105-14-106.pdf

# ctrl <- trainControl(method = "repeatedcv",
#                      number = 5,
#                      repeats = 5,
#                      verboseIter = FALSE,
#                      sampling = "smote")
# 
# set.seed(42)
# model_rf_smote <- caret::train(f_smote_fa,
#                                data = f_a.train_small,
#                                method = "rpart",
#                                #preProcess = c("scale", "center"),
#                                trControl = ctrl)


n <- names(f_a.train_small)
f_smote_f_a <- as.formula(paste("converted ~",paste(n[!n %in% c("converted","checkoutAmount","treatment","label","z_var")], collapse = " + ")))

f_a.train_small$converted <- as.factor(f_a.train_small$converted)
f_a.train_SMOTE <- SMOTE(f_smote_f_a,f_a.train_small,perc.over = 400,perc.under = 200)

# checking balance
prop.table(table(f_a.train_small$converted))
prop.table(table(test$converted))

prop.table(table(test$converted))
aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)
aggregate(checkoutAmount ~ treatment, data=test, mean)
aggregate(checkoutAmount ~ treatment, data=f_a, mean)

aggregate(converted ~ treatment, data=f_a.test_small, mean)
aggregate(converted ~ treatment, data=test, mean)
aggregate(converted ~ treatment, data=f_a, mean)

aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_a.test_small, mean)[1,2] 
with(test, prop.table(table(treatment,converted), margin=2)) 
with(test, prop.table(table(treatment,converted), margin=1))

with(f_a.test_small, prop.table(table(treatment,converted), margin=2)) 
with(f_a.test_small, prop.table(table(treatment,converted), margin=1))

t.test(checkoutAmount ~ treatment, data=f_a.test_small)
summary(aov(converted  ~ treatment, data=f_a.test_small))

t.test(checkoutAmount ~ treatment, data=test)
summary(aov(as.numeric(converted)  ~ treatment, data=test))
