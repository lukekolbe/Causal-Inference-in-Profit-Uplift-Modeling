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
f_b.train_SMOTE <- read_csv2("H:\Applied Predictive Analytics\Data\SMOTE\f_b.train_SMOTE.csv")
f_b.train_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_b.train_SMOTE.csv")
#f_b.validate_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_b.validate_SMOTE.csv")



# second part of stratification -------------------------------------------

strat_trainsplit_small <- stratified(f_b.train, c("treatment", "converted"), 0.23, bothSets=TRUE)
f_b.train_small <- as.data.frame(strat_split_small[[1]])
f_b.discard <- as.data.frame(strat_split_small[[2]])

#OR

strat_trainsplit_smote <- stratified(f_b.train_SMOTE, c("treatment", "converted"), 0.83, bothSets=TRUE)
f_b.train_discard <- as.data.frame(strat_trainsplit_smote[[1]]) # we cannot use this data, too many rows, to expensive to compute.
f_b.train_small <- as.data.frame(strat_trainsplit_smote[[2]])

# Data & formulas ---------------------------------------------------------

names(f_b.train_small)

#PICK ONE:
data <- f_b.train_small[,-c(3,4,24)] #removing targets label, converted, z_var
#data_f_b <- f_b.train_small[,-c(2,3,24)] #leaving converted, removing checkoutAmount

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
tree_f_b1 <- causalTree(f, data = data, treatment = data$treatment,
                        split.Rule = "CT", cv.option = "CT",  cv.Honest = T, split.Bucket = T,
                        xval = 5)

# Causal Forest -----------------------------------------------------------

system.time(cf_f_b <- causal_forest(
  X = data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
  Y = data$checkoutAmount,
  W = data$treatment,
  mtry=5,
  num.trees = 1000,
  honesty = TRUE,
  honesty.fraction = NULL,
  seed = 1839
))

cf_f_b.preds <- predict(object = cf_f_b, ### buggy, throws Error in if (more || nchar(output) > 80) { : missing value where TRUE/FALSE needed
                        newdata=f_b.test_small,
                        estimate.variance = TRUE)



# CausalBoosting ----------------------------------------------------------

system.time(cv.cb_f_b <- cv.causalBoosting(data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
                               tx=data$treatment,
                               y=data$checkoutAmount,
                               num.trees=500,
                               eps=0.3))

saveRDS(cv.cb_f_b, "cv.cb_f_b.rds")
cv.cb_f_b <- readRDS("cv.cb_f_b.rds")

cb_f_b.pred <- predict(cv.cb_f_b,
                       newx = f_b.test_small,
                       type = "treatment.effect",
                       num.trees = 500,
                       honest = FALSE,
                       naVal = 0)




# UpliftRF ----------------------------------------------------------------

f3 <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))
f3

upliftRF_f_b2 <- upliftRF(f3,
                          data = traindata2,
                          mtry = 10,
                          ntree = 1000,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_men)
