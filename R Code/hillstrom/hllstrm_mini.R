source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")


set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

getwd()
#load full data
hllstrm <- read.csv("working data/Datasets/hillstrm.csv", sep=",")

summary(hllstrm$segment)
summary(hllstrm$spend)
table(hllstrm$spend>0)
table(hllstrm$segment)

hllstrm$treatment <- integer(length=nrow(hllstrm))
hllstrm$treatment <- ifelse(hllstrm$segment=="No E-Mail", 0, 1)

### dummyfication of factors for causalboosting

for(level in unique(hllstrm$zip_code)){
  hllstrm[paste("zip_code", level, sep = "_")] <- ifelse(hllstrm$zip_code == level, 1, 0)
}

for(level in unique(hllstrm$channel)){
  hllstrm[paste("channel", level, sep = "_")] <- ifelse(hllstrm$channel == level, 1, 0)
}

# REMOVING UNNECESSARY COLUMNS --------------------------------------------

hllstrm <- hllstrm[,-c(2,6,8,9)] #channel & zip_code not needed after dummification; history_segment redundant; segment replaced by "treatment"

# stratification for training of all models except causaltree----------------------------------------------------------

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

# stratification for training of causaltree----------------------------------------------------------
#honest.causaltree needs an additional estimation set, for which a new split of 2:1:1 is done

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

train_indices_all <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1)))
sample_size_all <- as.numeric(xtabs(~conversion+treatment, hllstrm))
for(i in 1:4){
  train_indices_all[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
                                           hllstrm$treatment == combinations$Treatment[i])
                                   , size = round(0.5*sample_size_all[i]), replace=FALSE) 
} 
trainIndex_all <- c(train_indices_all, recursive=TRUE)

h_s.train <- hllstrm[trainIndex_all,]
h_s.rest  <- hllstrm[-trainIndex_all,]

train_indices_small <- list()
sample_size_small <- as.numeric(xtabs(~conversion+treatment, h_s.rest))
for(i in 1:4){
  train_indices_small[[i]] <- sample(which(h_s.rest$conversion == combinations$Conversion[i] &
                                             h_s.rest$treatment == combinations$Treatment[i]),
                                     size = round(0.5*sample_size_small[i]), replace=FALSE) 
} 
trainIndex_small <- c(train_indices_small, recursive=TRUE)
h_s.test <- h_s.rest[trainIndex_small,]
h_s.estimate  <- h_s.rest[-trainIndex_small,]


# MODEL TRAINING: Data & formulas ---------------------------------------------------------

# load test and train data for model training
h_s.test <- read.csv("working data/h_s.test.csv")
h_s.train  <- read.csv("working data/h_s.train.csv")

h_s.estimate_ct <- read.csv("working data/h_s.estimate_ct.csv")

#PICK ONE:
names(h_s.train)
data <- h_s.train

n <- names(data)
f <- as.formula(paste("spend ~", paste(n[!n %in% c("conversion","treatment", "spend", "X")], collapse = " + ")))

# Causal Tree -------------------------------------------------------------
ct_model.frame <- model.frame(f,data)

system.time(ct_h_s <- honest.causalTree(formula=ct_model.frame,
                                        data=data,
                                        treatment = data$treatment,
                                        est_data = h_s.estimate,
                                        est_treatment = h_s.estimate$treatment,
                                        HonestSampleSize = nrow(h_s.estimate),
                                        cp = 0.0000001,
                                        split.Rule = "CT", 
                                        split.Honest = T,
                                        minsize=10, #more = quicker
                                        cv.option = "CT",
                                        cv.Honest = T,
                                        xval=10))

opcp <-  ct_h_s$cptable[,1][which.min(ct_h_s$cptable[,4])]
opTree <- prune(ct_h_s, opcp)
rpart.plot(opTree) #pruning does not reduce the size of the tree

h_s_ct.hon.pred_8_0_1 <- predict(ct_h_s, h_s.test_ct[,-c(1,8,9,10)])

# CausalForest ------------------------------------------------------------

library(doParallel)
registerDoParallel(cores=2)

system.time(cf_hillstrom <- foreach(ntree=rep(4000,2),
                                    .combine=function(a,b)grf::merge_forests(list(a,b)),
                                    .multicombine=TRUE,.packages='grf') %dopar% {
                                      causal_forest(
                                        X = data[,-c(7,8,9)], #removing spend, conversion and treatment from covariates
                                        Y = data$spend,
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

cf_h_s.preds <- predict(object = cf_hillstrom,
                        newdata=h_s.test[,-which(names(h_s.test) %in% c("X","spend","conversion","treatment"))],
                        estimate.variance = TRUE)

# TWO MODEL RIDGE -------------------------------------------------------------

data <- h_s.train

n <- names(data)
f <- as.formula(paste("spend ~", paste(n[!n %in% c("X","conversion","treatment", "spend")], collapse = " + ")))

library(glmnet)

ridge_model.matrix_t <- model.matrix(f,data[data$treatment==1,])[,-1]
ridge_model.matrix_c <- model.matrix(f,data[data$treatment==0,])[,-1]

yt <- data[data$treatment==1,8]
yc <- data[data$treatment==0,8]

lambdas <- 10^seq(3, -2, by = -.1)

ridge_h_s_t <- cv.glmnet(x=ridge_model.matrix_t, y=yt, alpha = 0, lambda = lambdas)
ridge_h_s_c <- cv.glmnet(x=ridge_model.matrix_c, y=yc, alpha = 0, lambda = lambdas)

plot(ridge_h_s_t)
plot(ridge_h_s_c)

opt_lambda_t <- ridge_h_s_t$lambda.min
opt_lambda_c <- ridge_h_s_c$lambda.min

ridge_prediction.matrix <- model.matrix(f,h_s.test3)[,-1]

ridge_h_s_t_predict <- predict(ridge_h_s_t, s = opt_lambda_t, newx = ridge_prediction.matrix)
ridge_h_s_c_predict <- predict(ridge_h_s_c, s = opt_lambda_c, newx = ridge_prediction.matrix)


ridge_h_s_uplift <- ridge_h_s_t_predict-ridge_h_s_c_predict #uplift is the prediction

# causalboosting ----------------------------------------------------------

cv.cb_hillstrom <- cv.causalBoosting(data[,-c(7,8,9)],
                                     tx=data$treatment,
                                     y=data$spend,
                                     num.trees=50, # linearer Anstieg
                                     splitSpread = 0.1,
                                     maxleaves = 8,
                                     eps=0.3,
                                     nfolds=5)

cb_hllstrm_pred <-predict(cv.cb_hillstrom,
                          newx = h_s.test[,-which(names(data) %in% c("X","conversion","spend","treatment"))], 
                          newtx = h_s.test$treatment,
                          type = "treatment.effect",
                          num.trees = 50)

# BART --------------------------------------------------------------------


conf<-as.matrix(data[,-which(names(data) %in% c("spend","conversion","treatment"))])

x <- conf
y <- data$spend
z <- data$treatment

x.new <- h_s.test[,-which(names(h_s.test) %in% c("X","spend","conversion","treatment"))]

n.samples <- 20L
n.chains  <- 8L
system.time(fit <- bartc(y, z, x, method.trt = "bart", method.rsp = "bart",
                         estimand="att",
                         n.samples = n.samples, n.chains = n.chains, 
                         n.burn = 10L,
                         n.threads = 4L, n.trees = 1000L, 
                         keepTrees = TRUE,
                         verbose = FALSE))

#p.score <- predict(fit, x.new, value = "p.score")
#y      <- predict(fit, x.new, value = "y", combineChains = FALSE)
y1_att  <- predict(fit, x.new, value = "y1", combineChains = TRUE)
#y0      <- predict(fit, x.new, value = "y0", combineChains = TRUE)
#ite     <- predict(fit, x.new, value = "indiv.diff", combineChains = TRUE)

pred_att <- data.frame(rowMeans(y1_att))
