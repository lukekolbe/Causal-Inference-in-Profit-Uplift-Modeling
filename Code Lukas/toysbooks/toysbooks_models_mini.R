library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library(randomForest)

set.seed(101010)


b_t.train_SMOTE <- read_csv2("H:\Applied Predictive Analytics\Data\SMOTE\b_t.train_SMOTE.csv")
b_t.train_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/b_t.train_SMOTE.csv")
#b_t.validate_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/b_t.validate_SMOTE.csv")



# second part of stratification -------------------------------------------

#### NOT NEEDED FOR B_T Data, as it is small!

# strat_split_small <- stratified(b_t.learn.sub, c("treatment", "converted"), 0.5, bothSets=TRUE)
# b_t.train_small <- as.data.frame(strat_split_small[[1]])
# b_t.test_small <- as.data.frame(strat_split_small[[2]])
# 
# #OR
# 
# strat_trainsplit_smote <- stratified(f_b.train_SMOTE, c("treatment", "converted"), 0.8, bothSets=TRUE)
# f_b.train_discard <- as.data.frame(strat_trainsplit_smote[[1]]) # we cannot use this data, too many rows, to expensive to compute.
# f_b.train_small <- as.data.frame(strat_trainsplit_smote[[2]])

# Data & formulas ---------------------------------------------------------

names(b_t.train)

#PICK ONE:
data <- b_t.train[,-c(3,4,24)] #removing targets label, converted, z_var
#data_b_t <- b_t.train_small[,-c(2,3,24)] #leaving converted, removing checkoutAmount

data <- b_t.train_SMOTE[,-c(3,4,24)] #removing targets label, converted, z_var


n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
tree_b_t1 <- causalTree(f, data = data, treatment = data$treatment,
                        split.Rule = "CT", cv.option = "CT",  cv.Honest = T, split.Bucket = T,
                        xval = 5)

# Causal Forest -----------------------------------------------------------

system.time(cf_b_t <- causal_forest(
  X = data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
  Y = data$checkoutAmount,
  W = data$treatment,
  mtry=5,
  num.trees = 1000,
  honesty = TRUE,
  honesty.fraction = NULL,
  seed = 1839
))

cf_b_t.preds <- predict(object = cf_b_t, ### buggy, throws Error in if (more || nchar(output) > 80) { : missing value where TRUE/FALSE needed
                        newdata=b_t.test_small,
                        estimate.variance = TRUE)



# CausalBoosting ----------------------------------------------------------

system.time(cv.cb_b_t <- cv.causalBoosting(data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
                               tx=data$treatment,
                               y=data$checkoutAmount,
                               num.trees=500,
                               eps=0.3))

saveRDS(cv.cb_b_t, "cv.cb_b_t.rds")
cv.cb_b_t <- readRDS("cv.cb_b_t.rds")

cb_b_t.pred <- predict(cv.cb_b_t,
                       newx = b_t.test_small,
                       type = "treatment.effect",
                       num.trees = 500,
                       honest = FALSE,
                       naVal = 0)




# UpliftRF ----------------------------------------------------------------

f3 <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))
f3

upliftRF_b_t2 <- upliftRF(f3,
                          data = traindata2,
                          mtry = 10,
                          ntree = 1000,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_men)
