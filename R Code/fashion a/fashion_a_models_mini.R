source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

# load pre-processed data (28 features)
f_a.train_small <- read.csv("working data/small set/f_a_train_small_28.csv")[,-1]
f_a.test <- read.csv("working data/small set/f_a_test_28.csv")[,-1]
#f_a.estimation <- read.csv("working data/small set/f_a_estimation_28.csv")[,-1] #only for honest.causalTree

# load pre-processed data (~60 features)
f_a.train_small <- read.csv("working data/large set/f_a_train_small_60.csv")[,-1]
f_a.test <- read.csv("working data/large set/f_a_test_60.csv")[,-1]
#f_a.estimation <- read.csv("working data/large set/f_a_estimation_60.csv")[,-1] #only for honest.causalTree

data <- f_a.train_small

exclude.vars <- c("converted","checkoutAmount","treatment",
                  "eligibility", "ExpectedDiscount", "aborted", 
                  "confirmed", "campaignMov", "campaignValue", "campaignUnit")

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% exclude.vars], collapse = " + ")))

# Causal Tree -------------------------------------------------------------

ct_model.frame <- model.frame(f,data)

system.time(ct_f_a <- honest.causalTree(formula=ct_model.frame,
                                        data=data,
                                        treatment = data$treatment,
                                        est_data = f_b.estimation,
                                        est_treatment = f_b.estimation$treatment,
                                        HonestSampleSize = nrow(f_b.estimation),
                                        cp = 0.0000001,
                                        split.Rule = "CT", 
                                        split.Honest = T,
                                        minsize=10, #more = quicker?
                                        cv.option = "CT",
                                        cv.Honest = T,
                                        xval=10))

opcp <-  ct_f_a$cptable[,1][which.min(ct_f_a$cptable[,4])]
opTree <- prune(ct_f_a, opcp)
rpart.plot(opTree)

f_a_ct.hon.pred_6_0_1_28 <- predict(ct_f_a, f_a.test[,-which(names(f_a.test) %in% exclude.vars)])

# Causal Forest -----------------------------------------------------------

library(doParallel)
registerDoParallel(cores=4)

system.time(cf_f_a_dopar <- foreach(ntree=rep(1000,4),
                                    .combine=function(a,b,c,d)grf::merge_forests(list(a,b,c,d)),
                                    .multicombine=TRUE,.packages='grf') %dopar% {
                                      causal_forest(
                                        X = data[,-which(names(data) %in% exclude.vars)], #removing checkoutAmount and treatment from covariates
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

cf_f_a.preds <- predict(object = cf_f_a,
                        newdata=f_a.test[,-which(names(data) %in% exclude.vars)], # removing, checkoutAmount, converted, confirmed, aborted, treatment, eligibility, expectedDiscout
                        estimate.variance = TRUE)

# CausalBoosting ----------------------------------------------------------

system.time(cv.cb_f_a <- cv.causalBoosting(data[,-which(names(data) %in% exclude.vars)], # more data causes significant increase in computation time: 30k extra rows = ten-fold computation time
                                           tx=data$treatment,
                                           y=data$checkoutAmount,
                                           num.trees=50, # linearer Anstieg
                                           splitSpread = 0.1,
                                           maxleaves = 8,
                                           eps=0.3,
                                           nfolds=5)) # linearer Anstieg


cb_f_a.pred <- predict(cv.cb_f_a,
                       newx = f_a.test[,-which(names(data) %in% exclude.vars)], 
                       newtx = f_a.test$treatment,
                       type = "treatment.effect",
                       num.trees = 50)

# BART --------------------------------------------------------------------

data <- f_a.train_small
conf<-as.matrix(data[,-which(names(data) %in% exclude.vars)])

x <- conf
y <- data$checkoutAmount
z <- data$treatment

x.new <- f_a.test[,-which(names(f_a.test) %in% exclude.vars)]

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


# RIDGE/LASSO TWO MODEL -------------------------------------------------------------

data <- f_a.train_small

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% exclude.vars], collapse = " + ")))

ridge_model.matrix_t <- model.matrix(f,data[data$treatment==1,])[,-1]
ridge_model.matrix_c <- model.matrix(f,data[data$treatment==0,])[,-1]

yt <- data[data$treatment==1,which(names(data) %in% c("checkoutAmount"))]
yc <- data[data$treatment==0,which(names(data) %in% c("checkoutAmount"))]

lambdas <- 10^seq(3, -2, by = -.1)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
ridge_f_a_t <- cv.glmnet(x=ridge_model.matrix_t, y=yt, alpha = 0, lambda = lambdas)
ridge_f_a_c <- cv.glmnet(x=ridge_model.matrix_c, y=yc, alpha = 0, lambda = lambdas)

plot(ridge_f_a_t)
plot(ridge_f_a_c)

opt_lambda_t <- ridge_f_a_t$lambda.min
opt_lambda_c <- ridge_f_a_c$lambda.min

ridge_prediction.matrix <- model.matrix(f,f_a.test)[,-1]

ridge_f_a_t_predict <- predict(ridge_f_a_t, s = opt_lambda_t, newx = ridge_prediction.matrix)
ridge_f_a_c_predict <- predict(ridge_f_a_c, s = opt_lambda_c, newx = ridge_prediction.matrix)

ridge_f_a_uplift <- ridge_f_a_t_predict-ridge_f_a_c_predict


