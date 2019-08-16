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


set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
### CHECK FOR CORRECT SEED
sample(20)
#SHOULD BE [1]  4 11  2  3 20 18 14 15  8  5  6 13 10 17  7 12 16  1  9 19


getwd()
bt <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
bt <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
bt <- read.csv("H:\\Applied Predictive Analytics\\Data\\books and toys\\BooksAndToys.csv", sep=",")

b_t <- bt

b_t.train2 <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/b_t_train.csv", sep=",")[,-1]
b_t.test2 <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/b_t_test.csv", sep=",")[,-1]

# Feature Engineering -------------------------------------------
b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)

#b_t$z_var <- 0
#b_t$z_var <- ifelse(b_t$label>0, 1, 0)

b_t$eligibility <- numeric(nrow(b_t))
b_t$eligibility[b_t$campaignUnit=="CURRENCY"] <- ifelse((b_t$checkoutAmount[b_t$campaignUnit=="CURRENCY"]+b_t$campaignValue[b_t$campaignUnit=="CURRENCY"]/100 >= b_t$campaignMov[b_t$campaignUnit=="CURRENCY"]/100&b_t$treatment[b_t$campaignUnit=="CURRENCY"]==1),1,0)
b_t$eligibility[b_t$campaignUnit=="PERCENT"] <- ifelse((b_t$treatment[b_t$campaignUnit=="PERCENT"]==1),1,0)  ### no ifelse needed, could be just "1"

# STREAMLINE DATA ROWS----------------------------------------------------------------

# merging fragmented rows from same campaignValue category
b_t$campaignValue[b_t$campaignValue==15] <- 1500

# deleting rows that are unusual

b_t <- b_t[-which(b_t$campaignUnit=="CURRENCY"  &b_t$campaignValue==0),] # zero € discount does not make sense in our case
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==0),] # only 21 rows
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==1000),] # cases with 10 percent discount were the only ones with MOV>0 (3616 rows)
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==1600),] # only 1 row

# creating another variable for the cost matrix: Expected Discount
# this is based on the assumption that everyone who is eligible for a discount also uses said discount

b_t$ExpectedDiscount <- numeric(nrow(b_t))
b_t$ExpectedDiscount[b_t$campaignUnit=="CURRENCY"&b_t$eligibility==1] <- b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$eligibility==1]/100
b_t$ExpectedDiscount[b_t$campaignUnit=="PERCENT"] <- (b_t$checkoutAmount[b_t$campaignUnit=="PERCENT"]/(1-b_t$campaignValue[b_t$campaignUnit=="PERCENT"]/10000))-b_t$checkoutAmount[b_t$campaignUnit=="PERCENT"]


# Decriptive Analysis ------------------
# str(b_t)
# table(b_t$controlGroup)
# 
# table(b_t$campaignMov, b_t$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# 
# table(b_t$campaignUnit, b_t$campaignValue)
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"],
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0], 
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$eligibility[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$eligibility[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0],
#       b_t$treatment[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1],
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1])
# 
# prop.table(table(b_t$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%
# 
# with(b_t, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
# summary(aov(campaignValue  ~ controlGroup, data=b_t)) 
# # there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)
# 
# #with(b_t, prop.table(table(converted,controlGroup, campaignValue), margin=1))
# 
# table(b_t$checkoutDiscount) #no checkout discounts in the data?!
# prop.table(table(b_t$checkoutAmount>0, b_t$treatment)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)
# 
# 
# aggregate(checkoutAmount ~ treatment, data=b_t, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t, mean)[1,2]
# treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
# summary(aov(checkoutAmount  ~ controlGroup, data=b_t)) # the differences in checkout amount are statistically significant!
# t.test(checkoutAmount ~ controlGroup, data=b_t) # the differences in checkout amount are statistically significant!
# 
# names(b_t)
# 
# table(b_t$campaignMov)
# 
# table(b_t$checkoutAmount>=105,b_t$controlGroup)#14.800 people qualified for actually using the discount of 20â¬ through achieving the minimum order value
# with(b_t, prop.table(table(checkoutAmount>=105,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do



# Drop columns with no information----------------------------------------------------------------
b_t <- b_t[,-which(names(b_t) %in% c("campaignTags","trackerKey","campaignId","checkoutDiscount", "controlGroup","label"))] #"campaignUnit" "campaignMov", "campaignValue"

#NA Columns ---------------------------------------------------
# # Identify NA Columns
#names(which(sapply(b_t, anyNA)))

# # Check % of NA Columns
#colMeans(is.na(b_t))
# Drop the high NA percentage Columns

cols.dont.want=c("TimeSinceLastConversion","TimeSinceOn.sale.",
                 "TimeToFirst.cart.", "SecondsSinceFirst.sale.","TimeToCartAdd",
                 "TimeToFirst.sale.","SecondsSinceFirst.cart.","SecondsSinceTabSwitch", #only NA
                 "ViewedBefore.cart.","TabSwitchPer.product.","TabSwitchOnLastScreenCount","TotalTabSwitchCount") # these columns only carry 0s


b_t=b_t[,! names(b_t) %in% cols.dont.want, drop=F]
#Setting specific Column Null Values to 0, works for specificly defined columns
b_t$InitCartNonEmpty <- ifelse(b_t$InitCartNonEmpty == c("NA"), "0", b_t$InitCartNonEmpty)
#colMeans(is.na(b_t)) #check --> worked



# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0

str(b_t)


# for(i in 1:ncol(b_t)-1){
#   b_t[is.na(b_t[,c(1:3,5:ncol(b_t))][i]), c(1:3,5:ncol(b_t))][i] <- median(b_t[,c(1:3,5:ncol(b_t))][i], na.rm = TRUE)
# }

b_t.campaignUnit <- data.frame(b_t$campaignUnit)
names(b_t.campaignUnit) <- "campaignUnit"
b_t <- b_t[,-which(names(b_t)%in%"campaignUnit")]
for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}
b_t <- cbind(b_t, b_t.campaignUnit)
str(b_t)
# correlation test and removal of highly correlated variables ------------------

# Dropping further columns, we do not need anymore
#b_t <- b_t[,-which(names(b_t) %in% c("controlGroup","label"))] #campaignUnit, campaignMov, campaignValue

# find and reduce attributes that are highly corrected (ideally >0.75)
correlationMatrix <- cor(b_t[,-which(names(b_t) %in% c("converted", "treatment","checkoutAmount","eligibility", "aborted", "confirmed", "ExpectedDiscount","campaignMov", "campaignValue", "campaignUnit"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
# summarize the correlation matrix
#print(correlationMatrix)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

#NEW VERSION
b_t <- b_t[,-which(names(b_t) %in% c(highlyCorrelated))]

#### RESULT OF CORRELATION CHECK IN b_t; F_B; B_T DATA: 17 FEATURES WITH HIGH CORRELATION POTENTIAL
# c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
# "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
# "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
# "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
# "TotalClickCount","ViewCount","ViewsOn.overview.",
# "ViewsOn.product.","targetViewCount",)

# b_t <- b_t[,-which(names(b_t) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
#                                      "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
#                                      "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
#                                      "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
#                                      "TotalClickCount","ViewCount","ViewsOn.overview.",
#                                      "ViewsOn.product.","targetViewCount"))]
### this list is the results of comparing which columns were flagged via findCorrelation() in all three datasets, see data_correlation_selection.xlsx
#cor_complete <- cor(b_t)


# final selection after comparing importances -----------------------------
### this makes any code below obsolete when creating samples for actual model building
#picking top25 from NIV process

b_t <- b_t[,which(names(b_t) %in% c("checkoutAmount", "converted", "treatment",
                                    #"aborted",
                                    "ScreenWidth",
                                    "TimeSpentOn.product.",
                                    "epochSecond",
                                    "DayOfWeek",
                                    "TimeSinceOn.product.",
                                    "TimeToFirst.overview.",
                                    "HourOfDay",
                                    "SecondsSinceClick",
                                    "TimeSpentOn.search.",
                                    "SecondsFor.3.",
                                    "TimeToFirst.search.",
                                    "InitPageWas.home.",
                                    "ViewCountLastVisit",
                                    "TimeSinceOn.overview.",
                                    "VisitorKnown",
                                    "TimeToFirst.product.",
                                    "DurationLastVisitInSeconds",
                                    "RecencyOfPreviousSessionInHrs",
                                    "TimeSpentOn.overview.",
                                    "TotalClickCount",
                                    "TriggerEventsSinceLastOnThisPage",
                                    "ViewsOn.search.",
                                    "PreviousVisitCount",
                                    "TimeSinceOn.search."
                                    ,"ClicksPer.product.",
                                    "eligibility",
                                    "ExpectedDiscount",
                                    "campaignMov",
                                    "campaignUnit",
                                    "campaignValue"
                                    ))]


# alternative correlation -------------------------------------------------

b_t <- data.frame(sapply(b_t, as.numeric))
str(b_t)
b_t2<-b_t
b_t <- b_t2

b_t_correlated <- correlate(data=b_t[,-which(names(b_t) %in% c("treatment","checkoutAmount","converted","confirmed","aborted"))], target=z_var)
plot_correlation_funnel(b_t_correlated, interactive = F)

corrnames <- t(b_t_correlated[-1,1])
colnames(corrnames) <- corrnames[1,]
b_t <- b_t[,which(names(b_t) %in% c(corrnames[c(1:25)], "checkoutAmount", "treatment", "converted"))]
#b_t <- b_t[,-which(names(b_t) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount"))]

names(b_t)


#  Splitting into train & test set ----------------------------------------

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")


strat_split <- stratified(b_t, c("treatment", "converted"), 0.667, bothSets=TRUE)
b_t.train <- as.data.frame(strat_split[[1]])
b_t.test <- as.data.frame(strat_split[[2]])


write.csv(b_t.train, "b_t_train_28.csv")
write.csv(b_t.test, "b_t_test_eval.csv")



#  ALTERNATIVE Splitting into train FOR WHEN ESTIMATION SET NEEDED ----------------------------------------


strat_split <- stratified(b_t, c("treatment", "converted"), 0.75, bothSets=TRUE)
b_t.train_alt <- as.data.frame(strat_split[[1]])
b_t.test_alt <- as.data.frame(strat_split[[2]])


strat_split <- stratified(b_t.train_alt, c("treatment", "converted"), 0.7, bothSets=TRUE)
b_t.train_alt <- as.data.frame(strat_split[[1]])
b_t.estimate <- as.data.frame(strat_split[[2]])


write.csv(b_t.train_alt, "b_t_train_alt_28.csv")
write.csv(b_t.test_alt, "b_t_test_alt_eval.csv")
write.csv(b_t.estimate, "b_t_estimate_28.csv")


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
n
f_niv_bt <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount")], collapse = " + ")))

bt_niv <- niv(f_niv_bt, b_t, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
              nbins = 10, continuous = 4, plotit = TRUE)

b_t_niv <- bt_niv$niv
#bt_niv$nwoe

write.csv2(b_t_niv,"bt_niv3.csv")


# Average Treatment Effect (ATE) ---------------------------------------------------

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==0])) - mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==1]))
mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==0]) - mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==1])

# or alternatively:
experiment <- table(list("Control" = trainData_b_t2$controlGroup, "Converted" = trainData_b_t2$converted))
experiment

(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )






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










# # Discount Delta Function -------------------------------------------------
# 
# bt$discountDelta <- numeric(nrow(bt))
# 
# # for (i in seq_along(1:nrow(bt))){
# #   if (bt$campaignUnit[i]=="PERCENT" & bt$checkoutDiscount[i] > 0) {
# #     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
# #   }
# #   else if(bt$campaignUnit[i]=="CURRENCY" & bt$checkoutDiscount[i] > 0) {
# #     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
# #   }
# # }
# 
# 
# 
# condition1 <- (bt$campaignUnit[i]=="PERCENT" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
# condition2 <- (bt$campaignUnit[i]=="CURRENCY" & bt$controlGroup==0 & bt$checkoutDiscount > 0)
# 
# 
# for (i in seq_along(1:nrow(bt))){
#   if (condition1[i]) {
#     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$basketValue[i] * (bt$campaignValue[i]/100)
#   }
#   else if(condition2[i]) {
#     bt$discountDelta[i] <- bt$checkoutDiscount[i] - bt$campaignValue[i]
#   }
# }
# 
# table(bt$campaignUnit)
# 
# summary(bt$discountDelta)
# 
# table(bt$discountDelta>0,bt$controlGroup)
# table(bt$discountDelta<0, bt$controlGroup)
# 
# table(bt$discountPercentage>0.5,bt$controlGroup, bt$basketValue>50)
# 





