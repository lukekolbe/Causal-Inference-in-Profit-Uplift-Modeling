# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# install.packages("tools4uplift")



library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)




# Causal Tree on checkoutAmount ------------------------------------------------------

str(trainData_f_b2)

n <- names(trainData_f_b2)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% c("campaignMov", "campaignValue",
                                                            "checkoutDiscount","controlGroup",
                                                            "treatment","converted",
                                                            "checkoutAmount","epochSecond",
                                                            "label","ViewedBefore.cart.", 
                                                            "TabSwitchPer.product.", "TimeToFirst.cart.", 
                                                            "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch",
                                                            "TabSwitchOnLastScreenCount")], collapse = " + ")))


tree_f_b1 <- causalTree(f, data = trainData_f_b2, treatment = trainData_f_b2$treatment,
                        split.Rule = "TOT", cv.option = "TOT",  cv.Honest = F, split.Bucket = T, minbucket=2,
                        xval = 5, cp = 0.00017, minsize = 30)   # xval = 5, , propensity = 0.5, split.Honest = T
#cp = 0.0002 has decent size

saveRDS(tree_f_b1, file = "tree_f_b1.rds")

rpart.plot(tree_f_b)
summary(tree_f_b)

tree_f_b$cptable #### MODELING ON CHECKOUT AMOUNT GIVES NO GOOD RESULTS! the cross validation error INCREASES with any split

# Causal Tree on label (transformed according to Gubela) ------------------------------------------------------

f2 <- as.formula(paste("label ~", paste(n[!n %in% c("campaignMov", "campaignValue","checkoutDiscount","controlGroup","converted","checkoutAmount",
                                                    "epochSecond","label","ViewedBefore.cart.", 
                                                    "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount", "treatment")], collapse = " + ")))
f2

tree_f_b.1 <- causalTree(f2, data = trainData_f_b2, treatment = treatment,
                         split.Rule = "TOT", cv.option = "TOT",  cv.Honest = F, split.Bucket = F, minbucket=2,
                         xval = 5, cp = 0.0004, minsize = 30)   # xval = 5, , propensity = 0.5, split.Honest = T

rpart.plot(tree_f_b.1)
summary(tree_f_b.1)

tree_f_b.1$cptable

opcp <- tree_f_b.1$cptable[,1][which.min(tree_f_b$cptable[,3])] #BADS example minimizes cross validation error >> no results. If rel error is minimized, the results seem more logical.
opfit <- prune(tree_f_b, cp=opcp)
rpart.plot(opfit) ## this procedure always gives only one node for regression >> nonsense


pred_cT_f_b <- predict(object = tree_f_b, newdata = testData_f_b2)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct0 gives an estimate for a person to convert when in the control group
head(pred_cT_f_b) 
summary(pred_cT_f_b)

# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
preds_f_b[["CausalTree"]] <- pred_cT_f_b[, 1] - pred_cT_f_b[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred_mens[["CausalTree"]])
head(pred_mens)


# UPLIFT RF --------------------------------------------------------
# str(trainData_mens)
# str(trainData_womens)
# table(trainData$z_var2)
# table(testData$z_var2)

names(trainData_f_b2)

trainData_all[,-which(names(trainData_all) %in% c("conversion","spend","treatment", "segment","history_segment","zip_code","channel"))]


# upliftRF_hllstrm <- upliftRF(conversion ~ trt(treatment) +.,
#                              data = trainData_all[,-which(names(trainData_all) %in% c("spend","segment","history_segment","zip_code","channel"))],
#                              mtry = 6,
#                              ntree = 1000,
#                              split_method = "KL",
#                              minsplit = 50,
#                              verbose = TRUE)

summary(upliftRF_hllstrm)


n <- names(trainData_f_b2)

f3 <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment")], collapse = " + ")))
f3

upliftRF_f_b2 <- upliftRF(f3,
                          data = trainData_f_b2,
                          mtry = 10,
                          ntree = 1000,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_men)
### ONLY WORKS WITH BINARY TARGET


# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF_men, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred_mens <- list()
pred_upliftRF_mens <- predict(object = upliftRF_men, newdata = testData_mens)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct1 gives an estimate for a person to convert when in the control group
head(pred_upliftRF_mens) 
summary(pred_upliftRF_mens) 


# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
pred_mens[["upliftRF"]] <- pred_upliftRF_mens[, 1] - pred_upliftRF_mens[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred_mens[["upliftRF"]])
head(pred_mens)



# UpliftRF transformed target ---------------------------------------------

summary(upliftRF_hllstrm)


n <- names(trainData_f_b2)

f4 <- as.formula(paste("var_z ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","epochSecond","treatment")], collapse = " + ")))
f4

upliftRF_f_b2 <- upliftRF(f3,
                          data = trainData_f_b2,
                          mtry = 10,
                          ntree = 1000,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_men)


# CausalForest ------------------------------------------------------------

names(trainData_all)
str(trainData_all)


cf_f_b <- causal_forest(
  X = f_b.train_small, #excluding factors (dummified above) and Y-Variables
  Y = f_b.train_small$checkoutAmount,
  W = f_b.train_small$treatment,
  num.trees = 1000,
  honesty = TRUE,
  honesty.fraction = NULL,
  tune.parameters=TRUE,
  seed = 1839
)

summary(cf_hillstrom)

cf_f_b.preds <- predict(object = cf_f_b, ### buggy, throws Error in if (more || nchar(output) > 80) { : missing value where TRUE/FALSE needed
                        newdata=f_b.test_small,
                        estimate.variance = TRUE)

# Causal Boosting---------------------------------------------------------


# library("parallelMap")
# parallelStartSocket(3) #level = "causalLearning::causalBoosting"
# library("parallel")
# RNGkind("L'Ecuyer-CMRG")
# clusterSetRNGStream(iseed = 1234567)

# indx <- t(data.frame(lapply(trainData_f_b2, function(x) any(is.na(x)))))
# names(indx)
# names(indx[,])
# indx[indx[,1=="TRUE"],]
#names of columns that contain is.na==TRUE


system.time(cv.cb_f_b <- cv.causalBoosting(f_b.train_small,
                               tx=f_b.train_small$treatment,
                               y=f_b.train_small$checkoutAmount,
                               num.trees=500,
                               eps=0.3))

saveRDS(cv.cb_f_b, "cv.cb_f_b.rds")
cv.cb_f_b <- readRDS("cv.cb_f_b.rds")

summary(cv.cb_f_b)


cb_f_b.pred <- predict(cv.cb_f_b,
                       newx = f_b.test_small,
                       type = "treatment.effect",
                       num.trees = 500,
                       honest = FALSE,
                       naVal = 0)


#parallelStop() 



# BART --------------------------------------------------------------------




# Performance Assessment for Uplift Models  ---------------------------------------------

# Equivalent to the standard model lift, we can calculate the uplift for the sample deciles

treatment_effect_order_mens <- order(pred[['upliftRF']], decreasing=TRUE)
treatment_effect_groups_mens <- cbind(testData[treatment_effect_order, c("Conversion","Treatment")],"effect_estimate"=pred[["upliftRF"]][treatment_effect_order])

head(treatment_effect_groups, 10)

# We cannot calculate the true treatment effect per person, but per group
treatment_effect_groups$Decile <- cut(treatment_effect_groups$effect_estimate, breaks = 10, labels=FALSE)
head(treatment_effect_groups)
tail(treatment_effect_groups, 4)

treatment_groups <- aggregate(treatment_effect_groups[,c("Conversion","effect_estimate")], 
                              by=list("Decile"=treatment_effect_groups$Decile, "Treatment"=treatment_effect_groups$Treatment), 
                              FUN=mean)
# Conversion of customer without a treatment/coupon ranked by prediction
{plot(treatment_groups$Conversion[10:1], type='l')
  # Conversion of customer with a treatment/coupon ranked by prediction
  lines(treatment_groups$Conversion[20:11], type='l', col="red")}
## -> The uplift is the area between the curves
treatment <- treatment_groups$Conversion[20:11] - treatment_groups$Conversion[10:1]

# {uplift} has a function to calculate the Qini coefficient
# Argument direction specifies whether we aim to maximize (P_treatment - P_control) or (P_control - P_treatment), or in other words
# whether we aim for a high (purchase) probability or low (churn) probability

perf_upliftRF <- uplift::performance(pr.y1_ct1 = pred_upliftRF[, 1], pr.y1_ct0 = pred_upliftRF[, 2], 
                                     # with/without treatment prob.
                                     y = testData$Conversion, ct = testData$Treatment, # outcome and treatment indicators
                                     direction = 1, # maximize (1) or minimize (2) the difference? 
                                     groups = 10)

perf_upliftRF
# Plot uplift random forest performance
plot(perf_upliftRF[, "uplift"] ~ perf_upliftRF[, "group"], type ="l", xlab = "Decile (n*10% observations with top scores)", ylab = "Uplift")
plot(treatment, col='red')

# The Qini coefficient (derived from the Gini coefficient to measure the deviation from an equal distribution) is 
# defined as the area between the incremental gains curve of the model and the area under the diagonal resulting from random targeting
# in relation to the percentage of the population targeted.
Qini_upliftRF <- qini(perf_upliftRF, plotit = TRUE) 

Qini <- list()
Qini[["upliftRF"]] <- Qini_upliftRF$Qini
# The results show that it is efficient to target the 70% of customers for which the model predictions are highest with our campaign (under the assumption that there is no budget constraint). Our model delivers much better results than random targeting which is represented in the red diagonal line here. 
