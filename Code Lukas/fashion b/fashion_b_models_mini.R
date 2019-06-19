library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library(randomForest)
library(bartCause)


set.seed(101010)


#load smote data if needed for respective model
f_b.train_SMOTE <- read_csv2("H:\\Applied Predictive Analytics\\Data\\SMOTE\\f_b.train_SMOTE.csv")
f_b.train_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_b.train_SMOTE.csv")
#f_b.validate_SMOTE <- read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/f_b.validate_SMOTE.csv")



# second part of stratification -------------------------------------------

strat_trainsplit_small <- stratified(f_b.train, c("treatment", "converted"), 0.23, bothSets=TRUE)
f_b.train_small <- as.data.frame(strat_trainsplit_small[[1]])
#f_b.discard <- as.data.frame(strat_trainsplit_small[[2]])

#OR

strat_trainsplit_smote <- stratified(f_b.train_SMOTE, c("treatment", "converted"), 0.83, bothSets=TRUE)
#f_b.train_discard <- as.data.frame(strat_trainsplit_smote[[1]]) # we cannot use this data, too many rows, to expensive to compute.
f_b.train_small <- as.data.frame(strat_trainsplit_smote[[2]])

# Data & formulas ---------------------------------------------------------

names(f_b.train_small)

#PICK ONE:
data <- f_b.train_small[,-c(3,4,24)] #removing targets label, converted, z_var
#data_f_b <- f_b.train_small[,-c(2,3,24)] #leaving converted, removing checkoutAmount

#SMOTE:
data <- f_b.train_small[,-c(1,4,5,25)] #removing targets label, converted, z_var, "X"


n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
ct_model.frame <- model.frame(f,data)

system.time(ct_f_b <- causalTree(formula=ct_model.frame, 
                                 data=data,
                                 treatment = data$treatment,
                                 split.Rule = "CT", 
                                 cv.option = "CT",  
                                 cv.Honest = T, 
                                 split.Bucket = T,
                                 xval = 5))

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

library(doParallel)
registerDoParallel(cores=4)

system.time(cf_f_b_SMOTE <- foreach(ntree=rep(1000,4),
                                    .combine=function(a,b,c,d)grf::merge_forests(list(a,b,c,d)),
                                    .multicombine=TRUE,.packages='grf') %dopar% {
                                      causal_forest(
                                        X = data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
                                        Y = data$checkoutAmount,
                                        W = data$treatment,
                                        num.trees = ntree,
                                        honesty = TRUE,
                                        honesty.fraction = NULL,
                                        seed = 1839
                                      )
                                    }
)
stopImplicitCluster()

cf_f_b <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_f_b.RDS")
cf_f_b_SMOTE <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_f_b_SMOTE.RDS")

cf_f_b.preds <- predict(object = cf_f_b,
                        newdata=f_b.test[-c(2,3,4,22,24)],
                        estimate.variance = TRUE)

cf_f_b.preds_SMOTE <- predict(object = cf_f_b_SMOTE,
                              newdata=f_b.test[-c(2,3,4,22,24)],
                              estimate.variance = TRUE)

write.csv2(cf_f_b.preds, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/cf_f_b_preds.csv")
write.csv2(cf_f_b.preds_SMOTE, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/cf_f_b_preds_SMOTE.csv")


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


# BART --------------------------------------------------------------------

conf<-as.matrix(data[,-c(2,22)])

system.time(f_b_bart <- bartc(spend, treatment, conf, data=data,
                              method.rsp = "bart",
                              method.trt = "bart",
                              estimand   = "att",
                              p.scoreAsCovariate = TRUE, 
                              keepCall = TRUE, 
                              verbose = TRUE))


# TWO MODEL APPROACH (REGRESSION AND DECISION TREES) ---------------------------------------------------------------

glm_f_b_t <- glm(f, family = gaussian, data=data[data$treatment==1,])
glm_f_b_c <- glm(f, family = gaussian, data=data[data$treatment==0,])

summary(glm_f_b_t)
summary(glm_f_b_c)

glm_f_b_t_pred <- predict(glm_f_b_t, f_b.test)
glm_f_b_c_pred <- predict(glm_f_b_c, f_b.test)

glm_f_b_uplift <- glm_f_b_t_pred-glm_f_b_c_pred

lm_f_b_t <- lm(f, data=data[data$treatment==1,])
lm_f_b_c <- lm(f, data=data[data$treatment==0,])

summary(lm_f_b_t)
summary(lm_f_b_c)

lm_f_b_t_pred <- predict(lm_f_b_t, f_b.test)
lm_f_b_c_pred <- predict(lm_f_b_c, f_b.test)

lm_f_b_uplift <- lm_f_b_t_pred-lm_f_b_c_pred
head(lm_f_b_uplift)

library(rpart)
rpart_f_b_t = rpart(f, data=data[data$treatment==1,], cp=0.002, xval=10, model=TRUE)
#prp(rpart_f_b_t)
#summary(rpart_f_b_t)

rpart_f_b_c = rpart(f, data=data[data$treatment==0,], cp=0.002, xval=10, model=TRUE)
#prp(rpart_f_b_c)
#summary(rpart_f_b_c)

rpart_f_b_t_pred <- predict(rpart_f_b_t, f_b.test)
rpart_f_b_c_pred <- predict(rpart_f_b_c, f_b.test)

rpart_f_b_uplift <- rpart_f_b_t_pred-rpart_f_b_c_pred
head(rpart_f_b_uplift)


