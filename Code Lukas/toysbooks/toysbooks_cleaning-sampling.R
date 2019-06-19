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
b_t <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("H:\\Applied Predictive Analytics\\Data\\books and toys\\BooksAndToys.csv", sep=",")

# variable transformation -------------------------------------------------

b_t$z_var <- 0
b_t$z_var <- ifelse(b_t$label>0, 1, 0)
summary(b_t$z_var)

# Decriptive Analysis ------------------
str(b_t)
table(b_t$controlGroup)

table(b_t$campaignMov, b_t$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
prop.table(table(b_t$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%

with(b_t, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=b_t)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)

#with(b_t, prop.table(table(converted,controlGroup, campaignValue), margin=1))

table(b_t$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(b_t$checkoutAmount>0, b_t$treatment)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)


aggregate(checkoutAmount ~ treatment, data=b_t, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t, mean)[1,2]
treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
summary(aov(checkoutAmount  ~ controlGroup, data=b_t)) # the differences in checkout amount are statistically significant!
t.test(checkoutAmount ~ controlGroup, data=b_t) # the differences in checkout amount are statistically significant!

names(b_t)

table(b_t$campaignMov)

table(b_t$checkoutAmount>=105,b_t$controlGroup)#14.800 people qualified for actually using the discount of 20â¬ through achieving the minimum order value
with(b_t, prop.table(table(checkoutAmount>=105,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do

# Drop columns with no information----------------------------------------------------------------
b_t <- b_t[,-which(names(b_t) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

#NA Columns ---------------------------------------------------
# # Identify NA Columns
# names(which(sapply(b_t, anyNA)))
# # Check % of NA Columns
# colMeans(is.na(b_t))
# # Drop the high NA percentage Columns
# cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
#                  "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
#                  "TimeToCartAdd","SecondsSinceTabSwitch",
#                  "TotalTabSwitchCount", "TabSwitchOnLastScreenCount", "TabSwitchPer.product.") # get rid of these because at least in ONE dataset the NA ration is >0.8
# b_t=b_t[,! names(b_t) %in% cols.dont.want, drop=F]
# Setting specific Column Null Values to 0, works for specificly defined columns
#b_t$InitCartNonEmpty <- ifelse(b_t$InitCartNonEmpty == c("NA"), "0", b_t$InitCartNonEmpty)
#colMeans(is.na(b_t)) #check --> worked

#Create Dummy Variables from NA Columns
#b_t$HasSessionBefore <- ifelse(b_t$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0

for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}

# Feature Engineering -------------------------------------------
b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)

# library(lubridate)
# # create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond
# b_t$month=as.factor(month(as_datetime(b_t$epochSecond)))
# table(b_t$month)
# str(b_t$month)

# correlation test and removal of highly correlated variables ------------------

# Dropping further columns, we do not need anymore
#b_t <- b_t[,-which(names(b_t) %in% c("controlGroup","campaignMov","campaignValue","label","NormalizedCartSum"))] 

# find and reduce attributes that are highly corrected (ideally >0.75)
correlationMatrix <- cor(b_t[,-which(names(b_t) %in% c("converted", "treatment","checkoutAmount"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
# summarize the correlation matrix
#print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

#### RESULT OF CORRELATION CHECK IN b_t; F_B; B_T DATA: 17 FEATURES WITH HIGH CORRELATION POTENTIAL
# c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
# "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
# "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
# "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
# "TotalClickCount","ViewCount","ViewsOn.overview.",
# "ViewsOn.product.","targetViewCount",)

b_t <- b_t[,-which(names(b_t) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
                                     "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
                                     "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
                                     "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
                                     "TotalClickCount","ViewCount","ViewsOn.overview.",
                                     "ViewsOn.product.","targetViewCount"))]
### this list is the results of comparing which columns were flagged via findCorrelation() in all three datasets, see data_correlation_selection.xlsx
#cor_complete <- cor(b_t)


# final selection after comparing importances -----------------------------
### this makes any code below obsolete when creating samples for actual model building
#comparing upliftRF, RFE, NIV values, building average ranks, picking top 20

b_t <- b_t[,which(names(b_t) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]



# classic stratification from BADS 1st round ----------------------------------------

# train_indices_b_t <- list()
# 
# combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
# xtabs(~converted+treatment, b_t)
# sample_size_b_t <- as.numeric(xtabs(~converted+treatment, b_t))
# 
# for(i in 1:4){
#   train_indices_b_t[[i]] <- sample(which(b_t$converted == combinations$Conversion[i] &
#                                            b_t$treatment == combinations$Treatment[i])
#                                    , size = round(0.15*sample_size_b_t[i]), replace=FALSE) 
# } 
# 
# 
# trainIndex_b_t <- c(train_indices_b_t, recursive=TRUE)
# 
# trainData_small <- b_t[trainIndex_b_t,] # temporarily the learning data is only a small partition!


# SAMPLE SPLITTING AND STRATIFICATION 2ND ROUND (FOR PRE-TRAINING) ---------------------------------------------------

# train_indices_b_t2 <- list()
# 
# #combinations_b_t2 <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(1,0))) # treatment is ordered 1,0 compared to hillstrÃ¶m data because the variable indicates control group membership
# sample_size_b_t2 <- as.numeric(xtabs(~converted+treatment, trainData_small))
# 
# 
# for(i in 1:4){
#   train_indices_b_t2[[i]] <- sample(which(trainData_small$converted == combinations$Conversion[i] &
#                                             trainData_small$treatment == combinations$Treatment[i])
#                                     , size = round(0.5*sample_size_b_t2[i]), replace=FALSE) 
# } 
# 
# 
# trainIndex_b_t2 <- c(train_indices_b_t2, recursive=TRUE)
# 
# trainData_b_t2 <- trainData_small[-trainIndex_b_t2,] # temporarily the train data is only a small partition!
# testData_b_t2  <- trainData_small[trainIndex_b_t2,]
# 
# table(trainData_b_t2$checkoutAmount>0, trainData_b_t2$treatment)
# 
# prop.table(table(trainData_small$converted))
# prop.table(table(trainData_b_t2$converted))
# prop.table(table(b_t$converted))
# 
# summary(aov(checkoutAmount  ~ treatment, data=trainData_b_t2)) # checking statistical significance of the differences in checkout amount
# t.test(checkoutAmount ~ treatment, data=trainData_b_t2) # checking statistical significance of the differences in checkout amount
# 
# #### WHY IS THE ATE SO DIFFERENT BETWEEN TEST AND TRAIN DATA?! LOOKS LIKE AN ERROR IN STRATIFICATION!!!!
# aggregate(checkoutAmount ~ treatment, data=b_t, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_small, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=trainData_b_t2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=trainData_b_t2, mean)[1,2] 
# aggregate(checkoutAmount ~ treatment, data=testData_b_t2, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=testData_b_t2, mean)[1,2] 
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
# str(trainData_b_t2)
# names(trainData_b_t2)
# summary(trainData_b_t2$label)
system.time(rfe_b_t.results2 <- rfe(trainData_b_t2[,-c(2,3,4,55)], trainData_b_t2[,3], sizes=subsets, rfeControl=control))
# on label

saveRDS(rfe_b_t.results2, "rfe_b_t.results_label.rds")

stopCluster(cl)

rfe_b_t <- readRDS("/Volumes/kolbeluk/rfe_b_t.results.rds")

# summarize the results
print(rfe_b_t.results2)
# list the chosen features
predictors(rfe_b_t.results2)
# plot the results
plot(rfe_b_t.results2, type=c("g", "o"))
rfe_var <- rfe_f_b$variables
rfe_var[rfe_var$Variables==15,]



# Uplift NIV --------------------------------------------------------------


n <- names(b_t)
f_niv_fa <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))

fa_niv <- niv(f_niv_fa, b_t, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
              nbins = 10, continuous = 4, plotit = TRUE)

fa_niv$niv
fa_niv$nwoe



# Average Treatment Effect (ATE) ---------------------------------------------------

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==0])) - mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==1]))
mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==0]) - mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==1])

# or alternatively:
experiment <- table(list("Control" = trainData_b_t2$controlGroup, "Converted" = trainData_b_t2$converted))
experiment

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
#                                data = b_t.train_small,
#                                method = "rpart",
#                                #preProcess = c("scale", "center"),
#                                trControl = ctrl)


n <- names(b_t.train_small)
f_smote_b_t <- as.formula(paste("converted ~",paste(n[!n %in% c("converted","checkoutAmount","treatment","label","z_var")], collapse = " + ")))

b_t.train_small$converted <- as.factor(b_t.train_small$converted)
b_t.train_SMOTE <- SMOTE(f_smote_b_t,b_t.train_small,perc.over = 400,perc.under = 200)

# checking balance
prop.table(table(b_t.train_small$converted))
prop.table(table(test$converted))

prop.table(table(test$converted))
aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)
aggregate(checkoutAmount ~ treatment, data=test, mean)
aggregate(checkoutAmount ~ treatment, data=b_t, mean)

aggregate(converted ~ treatment, data=b_t.test_small, mean)
aggregate(converted ~ treatment, data=test, mean)
aggregate(converted ~ treatment, data=b_t, mean)

aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t.test_small, mean)[1,2] 
with(test, prop.table(table(treatment,converted), margin=2)) 
with(test, prop.table(table(treatment,converted), margin=1))

with(b_t.test_small, prop.table(table(treatment,converted), margin=2)) 
with(b_t.test_small, prop.table(table(treatment,converted), margin=1))

t.test(checkoutAmount ~ treatment, data=b_t.test_small)
summary(aov(converted  ~ treatment, data=b_t.test_small))

t.test(checkoutAmount ~ treatment, data=test)
summary(aov(as.numeric(converted)  ~ treatment, data=test))
