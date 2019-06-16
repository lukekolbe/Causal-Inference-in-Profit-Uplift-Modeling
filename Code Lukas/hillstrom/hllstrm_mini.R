# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# library(devtools) 
install.packages("BART")


library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
library(tools4uplift)
library(causalLearning)
library(causalTree)
library("BART")


set.seed(101010)

getwd()
hllstrm <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillström Data/hillstrm.csv", sep=",")
hllstrm <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillström Data/hillstrm.csv", sep=",")

#for use with SMOTE DATA
h_s.train <- read.csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/h_s.train_SMOTE.csv")
h_s.train_SMOTE <- read.csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/h_s.train_SMOTE.csv")
h_s.train <- read.csv2("H:\\Applied Predictive Analytics\\Data\\SMOTE\\h_s.train_SMOTE.csv")


str(hllstrm)
str(b_t)
str(b_t.train)
str(h_s.train)

str(h_s.train_SMOTE)
str(b_t.train_SMOTE)

summary(hllstrm)


summary(hllstrm$segment)
summary(hllstrm$spend)
table(hllstrm$spend>0)
table(hllstrm$segment)

hllstrm$treatment <- integer(length=nrow(hllstrm))
hllstrm$treatment <- ifelse(hllstrm$segment=="No E-Mail", 0, 1)

### stupid dummyfication of factors for causalboosting

# for(level in unique(hllstrm$history_segment)){
#   hllstrm[paste("history_segment", level, sep = "_")] <- ifelse(hllstrm$history_segment == level, 1, 0)
# }

for(level in unique(hllstrm$zip_code)){
  hllstrm[paste("zip_code", level, sep = "_")] <- ifelse(hllstrm$zip_code == level, 1, 0)
}

for(level in unique(hllstrm$channel)){
  hllstrm[paste("channel", level, sep = "_")] <- ifelse(hllstrm$channel == level, 1, 0)
}

# for(level in unique(hllstrm$segment)){
#   hllstrm[paste("segment", level, sep = "_")] <- ifelse(hllstrm$segment == level, 1, 0)
# }

# THE NEW DATA HAS TO BE MADE INTEGERS; SO THAT SMOTE DOES NOT 

# REMONING UNNECESSARY COLUMNS --------------------------------------------

hllstrm <- hllstrm[,-c(2,6,8,9)] #channel & zip_code not needed after dummification; history_segment redundant; segment replaced by "treatment"
hllstrm[,c(1,3:7,9:15)] <- apply(hllstrm[,c(1,3:7,9:15)],2, as.numeric)
str(hllstrm)

# stratification ----------------------------------------------------------

train_indices_all <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1)))
sample_size_all <- as.numeric(xtabs(~conversion+treatment, hllstrm))

for(i in 1:4){
  train_indices_all[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
                                           hllstrm$treatment == combinations$Treatment[i])
                                   , size = round(0.25*sample_size_all[i]), replace=FALSE) 
} 


trainIndex_all <- c(train_indices_all, recursive=TRUE)

h_s.train <- hllstrm[-trainIndex_all,]
h_s.test  <- hllstrm[trainIndex_all,]

table(h_s.train$spend>0, h_s.train$segment)

str(h_s.train)
str(h_s.test)

summary(hllstrm[,c("conversion","treatment")])
summary(h_s.test[,c("conversion","treatment")])
summary(h_s.train[,c("conversion","treatment")])


# SMOTE SAMPLING ---------------------------------------------
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3648438/pdf/1471-2105-14-106.pdf

n <- names(hllstrm)
f_smote_hllstrm <- as.formula(paste("conversion ~",paste(n[!n %in% c("conversion","spend","treatment")], collapse = " + ")))

h_s.train$conversion <- as.factor(h_s.train$conversion)
h_s.train_SMOTE <- SMOTE(f_smote_hllstrm,h_s.train,perc.over = 2000 ,perc.under = 450)
#good

# checking balance
prop.table(table(h_s.train$conversion))
prop.table(table(h_s.train_SMOTE$conversion))


h_s.test$conversion <- as.factor(h_s.test$conversion)
h_s.test_SMOTE <- SMOTE(f_smote_hllstrm,h_s.test,perc.over = 2000,perc.under = 450) # data about 10% bigger than before

write.csv2(h_s.train_SMOTE, "h_s.train_SMOTE.csv")
write.csv2(h_s.test_SMOTE, "h_s.test_SMOTE.csv")

# Data & formulas ---------------------------------------------------------

#PICK ONE:
data <- h_s.train


n <- names(data)
f <- as.formula(paste("spend ~", paste(n[!n %in% c("conversion","treatment", "spend")], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
ct_model.frame <- model.frame(f,data)

system.time(ct_h_s <- causalTree(formula=ct_model.frame, 
                                 data=data,
                                 treatment = data$treatment,
                                 split.Rule = "CT", 
                                 cv.option = "CT",  
                                 cv.Honest = T, 
                                 split.Bucket = T,
                                 xval = 5))

tree_all$cptable
rpart.plot(tree_all)
summary(tree_all)


# CausalForest ------------------------------------------------------------

# library(doParallel)
# registerDoParallel(cores=4)
# 
# system.time(cf_f_a_SMOTE <- foreach(ntree=rep(1000,4),
#                                     .combine=function(a,b,c,d)grf::merge_forests(list(a,b,c,d)),
#                                     .multicombine=TRUE,.packages='grf') %dopar% {
#                                       forests[i]<-causal_forest(
#                                         X = data[,-c(2,22)], #removing checkoutAmount and treatment from covariates
#                                         Y = data$checkoutAmount,
#                                         W = data$treatment,
#                                         num.trees = ntree,
#                                         honesty = TRUE,
#                                         honesty.fraction = NULL,
#                                         seed = 1839
#                                       )
#                                     }
# )
# stopImplicitCluster()

names(data)
str(data)


cf_hillstrom <- causal_forest(
  X = data[, -c(7,8,9)], #excluding factors (dummified above) and Y-Variables
  Y = data$spend,
  W = data$treatment,
  num.trees = 4000,
  honesty = TRUE,
  honesty.fraction = NULL,
  tune.parameters=TRUE,
  seed = 1839
)

cf_hillstrom_SMOTE <- causal_forest(
  X = data[, -c(1,8,9,10)], #excluding factors (dummified above), X introduced by SMOTE and Y-Variables
  Y = data$spend,
  W = data$treatment,
  num.trees = 4000,
  honesty = TRUE,
  honesty.fraction = NULL,
  tune.parameters=TRUE,
  seed = 1839
)

summary(cf_hillstrom)
cf_hillstrom$tunable.params

cf_hillstrom_preds <- predict(object = cf_hillstrom,
                              newdata=h_s.test[, -c(2,6,8,9,11,12,13)],
                              estimate.variance = TRUE)



saveRDS(cf_hillstrom, "cf_hillstrom.RDS")


# UPLIFT RF (used for variable importance) --------------------------------------------------------
# str(trainData_mens)
# str(trainData_womens)
# table(trainData$z_var2)
# table(testData$z_var2)

h_s.train[,-which(names(h_s.train) %in% c("conversion","spend","treatment", "segment","history_segment","zip_code","channel"))]


upliftRF_hllstrm <- upliftRF(conversion ~ trt(treatment) +.,
                             data = h_s.train[,-which(names(h_s.train) %in% c("spend","segment","history_segment","zip_code","channel"))],
                             mtry = 6,
                             ntree = 1000,
                             split_method = "KL",
                             minsplit = 50,
                             verbose = TRUE)

summary(upliftRF_hllstrm)

saveRDS(upliftRF_hllstrm, "upliftRF_hllstrm.RDS")
upliftRF_hllstrm <- readRDS("upliftRF_hllstrm.RDS")

varImportance(upliftRF_hllstrm, plotit = FALSE, normalize = TRUE)


# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF_hllstrm, plotit = FALSE, normalize = TRUE)


# TWO MODEL APPROACH (REGRESSION AND DECISION TREES) ---------------------------------------------------------------

#### THIS IS MY OWN INTERPRETATION OF HOW THE MODEL WORKS
#### MIGHT VERY WELL BE VERY WRONG!

glm_men_treat <- glm(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, family = gaussian, data=trainData_mens[trainData_mens$treatment==1,])
glm_men_contr <- glm(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, family = gaussian, data=control)

summary(glm_men_treat)
summary(glm_men_contr)

library(rpart)
rpart_contr = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=control, cp=0.0017, xval=10, model=TRUE)
prp(rpart)
summary(rpart)

rpart_men = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=trainData_mens[trainData_mens$treatment==1,], cp=0.0017, xval=10, model=TRUE)
prp(rpart_men)
summary(rpart_men)





# causalboosting ----------------------------------------------------------


str(h_s.train)
names(h_s.train[,-which(names(h_s.train) %in% c("segment","history_segment","zip_code","channel"))])
names(h_s.train)

prop.table(table(h_s.train$treatment))

cv.cb_hillstrom <- cv.causalBoosting(h_s.train[, -c(2,6,8,9,11,12,13)],
                                     tx=h_s.train$treatment, 
                                     y=h_s.train$spend,
                                     num.trees=500,
                                     eps=0.3)

saveRDS(cb_hillstrom, "cb_hillstrom.rds")
cb_hillstrom <- readRDS("cb_hillstrom.rds")

cv.cb_hillstrom <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillstrom results/cv.cb_hillstrom.rds")


summary(cb_hillstrom)


cb_hllstrm_pred <- predict(cv.cb_hillstrom, 
                           newx = h_s.test[, -c(2,6,8,9,11,12,13)], 
                           type = "treatment.effect",
                           num.trees = 500,
                           honest = FALSE,
                           naVal = 0)

summary(cb_hllstrm_pred)
summary(h_s.train$spend)

# BART --------------------------------------------------------------------

