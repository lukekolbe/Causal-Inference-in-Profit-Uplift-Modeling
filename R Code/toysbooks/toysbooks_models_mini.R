library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library(randomForest)
library(bartCause)
install.packages("doParallel")



set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
### CHECK FOR CORRECT SEED
sample(20)
#SHOULD BE [1]  4 11  2  3 20 18 14 15  8  5  6 13 10 17  7 12 16  1  9 19


# load pre-processed data (61 features)
b_t.train <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/large set/b_t_train_60.csv")[,-1]
b_t.test <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/large set/b_t_test_60.csv")[,-1]

b_t.train <- read.csv("H:\\Applied Predictive Analytics\\working data\\large set\\b_t_train_60.csv")[,-1]
b_t.test<- read.csv("H:\\Applied Predictive Analytics\\working data\\large set\\b_t_test_60.csv")[,-1]


# load pre-processed data (28 features)
b_t.train <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/small set/b_t_train_28.csv")[,-1]
b_t.test <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/small set/b_t_test_28.csv")[,-1]

b_t.train <- read.csv("H:\\Applied Predictive Analytics\\working data\\small set\\b_t_train_28.csv")[,-1]
b_t.test<- read.csv("H:\\Applied Predictive Analytics\\working data\\small set\\b_t_test_28.csv")[,-1]




# ALTERNATIVE FOR CAUSALTREE load pre-processed data (61 features)
b_t.train <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_train_alt_60.csv")[,-1]
b_t.test <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_test_alt_60.csv")[,-1]

b_t.train <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_train_60.csv")[,-1]
b_t.test <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_test_60.csv")[,-1]

b_t.estimate <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_estimate_60.csv")[,-1]
b_t.estimate <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_estimate_60.csv")[,-1]


# ALTERNATIVE FOR CAUSALTREE load pre-processed data (28 features)
b_t.train <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_train_alt_28.csv")[,-1]
b_t.test <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_test_alt_28.csv")[,-1]

b_t.train <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_train_alt_28.csv")[,-1]
b_t.test <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_test_alt_28.csv")[,-1]

b_t.estimate <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/working data/BT special/b_t_estimate_28.csv")[,-1]
b_t.estimate <- read.csv("H:\\Applied Predictive Analytics\\working data\\BT special\\b_t_estimate_28.csv")[,-1]


# Data & formulas ---------------------------------------------------------

names(b_t.train)

#PICK ONE:
#data <- b_t.train[,-c(3,4,24)] #removing targets label, converted, z_var
#data_b_t <- b_t.train_small[,-c(2,3,24)] #leaving converted, removing checkoutAmount

data <- b_t.train


n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed")], collapse = " + ")))


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


system.time(ct_b_t <- honest.causalTree(formula=ct_model.frame,
                                        data=data,
                                        treatment = data$treatment,
                                        est_data = f_b.estimation,#[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))],
                                        est_treatment = f_b.estimation$treatment,
                                        HonestSampleSize = nrow(f_b.estimation),
                                        cp = 0.0000001,
                                        split.Rule = "CT", 
                                        split.Honest = T,
                                        minsize=10, #more = quicker?
                                        cv.option = "CT",
                                        cv.Honest = T,
                                        xval=10))

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

b_t_ct.hon.pred_7_0_1_28 <- predict(ct_b_t, b_t.test[,-which(names(b_t.test) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))])
write.csv(b_t_ct.hon.pred_7_0_1_28, "b_t_ct.hon.pred_7_0_1_28.csv")

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

system.time(cf_b_t_dopar <- foreach(ntree=rep(1000,4),
                                    .combine=function(a,b,c,d)grf::merge_forests(list(a,b,c,d)),
                                    .multicombine=TRUE,.packages='grf') %dopar% {
                                      causal_forest(
                                        X = data[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))], #removing checkoutAmount and treatment from covariates
                                        Y = data$checkoutAmount,
                                        W = data$treatment,
                                        num.trees = ntree,
                                        honesty = TRUE,
                                        honesty.fraction = NULL,
                                        tune.parameters = TRUE,
                                        seed = 1839
                                      )
                                    }
)
stopImplicitCluster()


cf_b_t <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_b_t.RDS")
cf_b_t <- readRDS("/Users/lukaskolbe/Documents/HU APA/CausalForests/cf_b_t_dopar2.RDS")

cf_b_t.preds2 <- predict(object = cf_b_t,
                              newdata=b_t.test[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))],
                              estimate.variance = TRUE)

write.csv2(cf_b_t.preds2, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/NEW in August/cf_b_t_preds_56.csv")
write.csv2(cf_b_t.preds_SMOTE, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/predictions/cf_b_t_preds_SMOTE.csv")


# CausalBoosting ----------------------------------------------------------

data <- b_t.train

system.time(cv.cb_b_t <- cv.causalBoosting(data[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))], # more data causes significant increase in computation time: 30k extra rows = ten-fold computation time
                                           tx=data$treatment,
                                           y=data$checkoutAmount,
                                           num.trees=20, # linearer Anstieg
                                           splitSpread = 0.1,
                                           maxleaves = 4,
                                           eps=0.3,
                                           nfolds=5)) # linearer Anstieg)

saveRDS(cv.cb_b_t, "cv.cb_b_t.rds")
cv.cb_b_t <- readRDS("cv.cb_b_t.rds")

cb_b_t.pred <- predict(object=cv.cb_b_t, 
                       newx=b_t.test[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))], 
                       newtx=b_t.test$treatment, 
                       type="treatment.effect", 
                       num.trees=50)




# UpliftRF ----------------------------------------------------------------

f3 <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed")], collapse = " + ")))
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

install.packages("testthat")
context("common support diagnostics")
library(testthat)
library(bartCause)

source(system.file("common", "linearData.R", package = "bartCause"))

data <- b_t.train
conf<-as.matrix(data[,-which(names(data) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))])

x <- conf
y <- data$checkoutAmount
z <- data$treatment

x.new <- b_t.test[,-which(names(b_t.test) %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed"))]
#n.test <- nrow(x.new)


n.samples <- 20L
n.chains  <- 8L
system.time(fit <- bartc(y, z, x, method.trt = "bart", method.rsp = "bart",
                          estimand="att",
                          n.samples = n.samples, n.chains = n.chains, 
                          n.burn = 10L,
                          n.threads = 4L, n.trees = 1000L, 
                          keepTrees = TRUE,
                          verbose = FALSE))


# check predict for single row
expect_equal(length(predict(fit, x.new[1,], value = "y0")), n.samples * n.chains)

p.score <- predict(fit, x.new, value = "p.score")
#y      <- predict(fit, x.new, value = "y", combineChains = FALSE)
y1_ate  <- predict(fit, x.new, value = "y1", combineChains = TRUE)
y0      <- predict(fit, x.new, value = "y0", combineChains = TRUE)
ite     <- predict(fit, x.new, value = "indiv.diff", combineChains = TRUE)

pred_ate <- data.frame(rowMeans(y1_ate))
pred_att <- data.frame(rowMeans(y1_att))
write.csv(pred, "bart_b_t_pred.csv")



# system.time(b_t_bart <- bartc(checkoutAmount, treatment, conf, data=data,
#                               method.rsp = "bart",
#                               method.trt = "bart",
#                               #estimand   = "att",
#                               p.scoreAsCovariate = TRUE, 
#                               keepCall = TRUE, 
#                               keepTrees = TRUE,
#                               verbose = TRUE))


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


# RIDGE/LASSO -------------------------------------------------------------
data <- b_t.train

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("converted","checkoutAmount","treatment", "eligibility", "ExpectedDiscount", "aborted", "confirmed")], collapse = " + ")))
f

names(data)
library(glmnet)

ridge_model.matrix_t <- model.matrix(f,data[data$treatment==1,])[,-1]
ridge_model.matrix_c <- model.matrix(f,data[data$treatment==0,])[,-1]

names(ridge_model.matrix_t)

yt <- data[data$treatment==1,which(names(data) %in% c("checkoutAmount"))]
yc <- data[data$treatment==0,which(names(data) %in% c("checkoutAmount"))]

lambdas <- 10^seq(3, -2, by = -.1)

ridge_b_t_t <- cv.glmnet(x=ridge_model.matrix_t, y=yt, alpha = 0, lambda = lambdas)
ridge_b_t_c <- cv.glmnet(x=ridge_model.matrix_c, y=yc, alpha = 0, lambda = lambdas)

plot(ridge_b_t_t)
plot(ridge_b_t_c)

opt_lambda_c <- ridge_b_t_c$lambda.min
opt_lambda_t <- ridge_b_t_t$lambda.min


ridge_prediction.matrix <- model.matrix(f,b_t.test)[,-1]

ridge_b_t_t_predict <- predict(ridge_b_t_t, s = opt_lambda_t, newx = ridge_prediction.matrix)
ridge_b_t_c_predict <- predict(ridge_b_t_c, s = opt_lambda_c, newx = ridge_prediction.matrix)


ridge_b_t_uplift <- ridge_b_t_t_predict-ridge_b_t_c_predict
head(ridge_b_t_uplift)
write.csv2(ridge_b_t_uplift, "/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/final predictions/Two-model ridge/ridge_b_t_uplift_28.csv")



