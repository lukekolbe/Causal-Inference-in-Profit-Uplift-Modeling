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


b_t.train_SMOTE <- read_csv2("H:\\Applied Predictive Analytics\\Data\\SMOTE\\b_t.train_SMOTE.csv")
b_t.train_SMOTE <- data.frame(read_csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/b_t.train_SMOTE.csv"))
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

data <- b_t.train_SMOTE[,-c(1,4,5,25)] #removing targets label, converted, z_var


n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
ct_model.frame <- model.frame(f,data)

system.time(ct_b_t <- causalTree(formula=ct_model.frame, 
                                 data=data,
                                 treatment = data$treatment,
                                 split.Rule = "CT", 
                                 cv.option = "CT",  
                                 cv.Honest = T, 
                                 split.Bucket = T,
                                 xval = 5))

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

library(doParallel)
registerDoParallel(cores=4)

system.time(cf_b_t_SMOTE <- foreach(ntree=rep(1000,4),
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


cf_b_t <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_b_t.RDS")
cf_b_t_SMOTE <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_b_t_SMOTE.RDS")

cf_b_t.preds <- predict(object = cf_b_t,
                        newdata=b_t.validate[-c(2,3,4,22,24)],
                        estimate.variance = TRUE)

cf_b_t.preds_SMOTE <- predict(object = cf_b_t_SMOTE,
                              newdata=b_t.validate[-c(2,3,4,22,24)],
                              estimate.variance = TRUE)

write.csv2(cf_b_t.preds, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/cf_b_t_preds.csv")
write.csv2(cf_b_t.preds_SMOTE, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/cf_b_t_preds_SMOTE.csv")


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


# BART --------------------------------------------------------------------

conf<-as.matrix(data[,-c(2,22)])

system.time(b_t_bart <- bartc(spend, treatment, conf, data=data,
                              method.rsp = "bart",
                              method.trt = "bart",
                              estimand   = "att",
                              p.scoreAsCovariate = TRUE, 
                              keepCall = TRUE, 
                              verbose = TRUE))


# TWO MODEL APPROACH (REGRESSION AND DECISION TREES) ---------------------------------------------------------------

glm_b_t_t <- glm(f, family = gaussian, data=data[data$treatment==1,])
glm_b_t_c <- glm(f, family = gaussian, data=data[data$treatment==0,])

summary(glm_b_t_t)
summary(glm_b_t_c)

glm_b_t_t_pred <- predict(glm_b_t_t, b_t.test)
glm_b_t_c_pred <- predict(glm_b_t_c, b_t.test)

glm_b_t_uplift <- glm_b_t_t_pred-glm_b_t_c_pred

lm_b_t_t <- lm(f, data=data[data$treatment==1,])
lm_b_t_c <- lm(f, data=data[data$treatment==0,])

summary(lm_b_t_t)
summary(lm_b_t_c)

lm_b_t_t_pred <- predict(lm_b_t_t, b_t.test)
lm_b_t_c_pred <- predict(lm_b_t_c, b_t.test)

lm_b_t_uplift <- lm_b_t_t_pred-lm_b_t_c_pred
head(lm_b_t_uplift)

library(rpart)
rpart_b_t_t = rpart(f, data=data[data$treatment==1,], cp=0.002, xval=10, model=TRUE)
#prp(rpart_b_t_t)
#summary(rpart_b_t_t)

rpart_b_t_c = rpart(f, data=data[data$treatment==0,], cp=0.002, xval=10, model=TRUE)
#prp(rpart_b_t_c)
#summary(rpart_b_t_c)

rpart_b_t_t_pred <- predict(rpart_b_t_t, b_t.test)
rpart_b_t_c_pred <- predict(rpart_b_t_c, b_t.test)

rpart_b_t_uplift <- rpart_b_t_t_pred-rpart_b_t_c_pred
head(rpart_b_t_uplift)


