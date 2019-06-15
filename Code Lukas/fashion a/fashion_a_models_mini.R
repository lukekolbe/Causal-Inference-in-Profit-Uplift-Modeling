library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library(randomForest)

set.seed(101010)

#load smote data if needed for respective model
f_a.train_SMOTE <- read_csv2("H:\Applied Predictive Analytics\Data\SMOTE\f_a.train_SMOTE.csv")
f_a.train_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_a.train_SMOTE.csv")
#f_a.validate_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_a.validate_SMOTE.csv")


# second part of stratification -------------------------------------------

strat_trainsplit_small <- stratified(f_a.train, c("treatment", "converted"), 0.85, bothSets=TRUE)
f_a.train_discard <- as.data.frame(strat_trainsplit_small[[1]]) # we cannot use this data, too many rows, to expensive to compute.
f_a.train_small <- as.data.frame(strat_trainsplit_small[[2]])

#OR

strat_trainsplit_smote <- stratified(f_a.train_SMOTE, c("treatment", "converted"), 0.89, bothSets=TRUE)
f_a.train_discard <- as.data.frame(strat_trainsplit_smote[[1]]) # we cannot use this data, too many rows, to expensive to compute.
f_a.train_small <- as.data.frame(strat_trainsplit_smote[[2]])

# Data & formulas ---------------------------------------------------------

names(f_a.train_small)

#PICK ONE:
data <- f_a.train_small[,-c(3,4,24)] #removing targets label, converted, z_var
#data_f_a <- f_a.train_small[,-c(2,3,24)] #leaving converted, removing checkoutAmount


n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
system.time(tree_f_a1 <- causalTree(f, data = traindata2, treatment = traindata2$treatment,
                        split.Rule = "CT", cv.option = "CT",  cv.Honest = T, split.Bucket = T,
                        xval = 5))

# Causal Forest -----------------------------------------------------------

system.time(cf_f_a <- causal_forest(
  X = data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
  Y = data$checkoutAmount,
  W = data$treatment,
  num.trees = 1000,
  mtry=5,
  honesty = TRUE,
  honesty.fraction = NULL,
  seed = 1839
))

cf_f_a.preds <- predict(object = cf_f_a, ### buggy, throws Error in if (more || nchar(output) > 80) { : missing value where TRUE/FALSE needed
                        newdata=f_a.test_small,
                        estimate.variance = TRUE)



# CausalBoosting ----------------------------------------------------------

system.time(cv.cb_f_a <- cv.causalBoosting(data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
                               tx=data$treatment,
                               y=data$checkoutAmount,
                               num.trees=500,
                               eps=0.3))

saveRDS(cv.cb_f_a, "cv.cb_f_a.rds")
cv.cb_f_a <- readRDS("cv.cb_f_a.rds")

cb_f_a.pred <- predict(cv.cb_f_a,
                       newx = f_a.test_small,
                       type = "treatment.effect",
                       num.trees = 500,
                       honest = FALSE,
                       naVal = 0)




# UpliftRF ----------------------------------------------------------------

f3 <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))
f3

upliftRF_f_a2 <- upliftRF(f3,
                          data = traindata2,
                          mtry = 10,
                          ntree = 1000,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_men)
