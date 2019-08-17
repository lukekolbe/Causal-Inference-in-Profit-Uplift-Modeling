source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")


# load pre-processed data (28 features)
b_t.train <- read.csv("working data/small set/b_t_train_28.csv")[,-1]
b_t.test <- read.csv("working data/small set/b_t_test_28.csv")[,-1]

# ALTERNATIVE FOR CAUSALTREE load pre-processed data (28 features)
b_t.train <- read.csv("working data/BT special/b_t_train_alt_28.csv")[,-1]
b_t.test <- read.csv("working data/BT special/b_t_test_alt_28.csv")[,-1]
b_t.estimate <- read.csv("working data/BT special/b_t_estimate_28.csv")[,-1]


# load pre-processed data (61 features)
b_t.train <- read.csv("working data/large set/b_t_train_60.csv")[,-1]
b_t.test <- read.csv("working data/large set/b_t_test_60.csv")[,-1]

# ALTERNATIVE FOR CAUSALTREE load pre-processed data (61 features)
b_t.train <- read.csv("working data/BT special/b_t_train_alt_60.csv")[,-1]
b_t.test <- read.csv("working data/BT special/b_t_test_alt_60.csv")[,-1]
b_t.estimate <- read.csv("working data/BT special/b_t_estimate_60.csv")[,-1]


# Data & formulas ---------------------------------------------------------

data <- b_t.train

exclude.vars <- c("converted","checkoutAmount","treatment",
                  "eligibility", "ExpectedDiscount", "aborted", 
                  "confirmed", "campaignMov", "campaignValue", "campaignUnit")

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% exclude.vars], collapse = " + ")))


# Causal Tree -------------------------------------------------------------
ct_model.frame <- model.frame(f,data)

system.time(ct_b_t <- honest.causalTree(formula=ct_model.frame,
                                        data=data,
                                        treatment = data$treatment,
                                        est_data = f_b.estimation,#[,-which(names(data) %in% exclude.vars)],
                                        est_treatment = f_b.estimation$treatment,
                                        HonestSampleSize = nrow(f_b.estimation),
                                        cp = 0.0000001, # the number of 0s defines the name of the prediction file; more numbers= bigger trees
                                        split.Rule = "CT", 
                                        split.Honest = T,
                                        minsize=10, #more = quicker?
                                        cv.option = "CT",
                                        cv.Honest = T,
                                        xval=10))

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

b_t_ct.hon.pred_7_0_1_28 <- predict(ct_b_t, b_t.test[,-which(names(b_t.test) %in% exclude.vars)])
write.csv(b_t_ct.hon.pred_7_0_1_28, "b_t_ct.hon.pred_7_0_1_28.csv")

# Causal Forest -----------------------------------------------------------

library(doParallel)
registerDoParallel(cores=4)

system.time(cf_b_t_dopar <- foreach(ntree=rep(1000,4),
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

cf_b_t.preds <- predict(object = cf_b_t_dopar,
                              newdata=b_t.test[,-which(names(data) %in% exclude.vars)],
                              estimate.variance = TRUE)

# CausalBoosting ----------------------------------------------------------

data <- b_t.train

system.time(cv.cb_b_t <-cv.causalBoosting(data[,-which(names(data) %in% exclude.vars)], # more data causes significant increase in computation time: 30k extra rows = ten-fold computation time
                                          tx=data$treatment,
                                          y=data$checkoutAmount,
                                          num.trees=50, # linearer Anstieg
                                          splitSpread = 0.1,
                                          maxleaves = 8,
                                          eps=0.3,
                                          nfolds=5)) # linearer Anstieg

cb_b_t.pred <- predict(object=cv.cb_b_t, 
                       newx=b_t.test[,-which(names(data) %in% exclude.vars)], 
                       newtx=b_t.test$treatment, 
                       type="treatment.effect", 
                       num.trees=50)


# BART --------------------------------------------------------------------

conf<-as.matrix(data[,-which(names(data) %in% exclude.vars)])

x <- conf
y <- data$checkoutAmount
z <- data$treatment

x.new <- b_t.test[,-which(names(b_t.test) %in% exclude.vars)]

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
y1_ate  <- predict(fit, x.new, value = "y1", combineChains = TRUE)
#y0      <- predict(fit, x.new, value = "y0", combineChains = TRUE)
#ite     <- predict(fit, x.new, value = "indiv.diff", combineChains = TRUE)

pred_att <- data.frame(rowMeans(y1_att))

# RIDGE/LASSO -------------------------------------------------------------

n <- names(data)
f <- as.formula(paste("checkoutAmount ~", paste(n[!n %in% exclude.vars], collapse = " + ")))

ridge_model.matrix_t <- model.matrix(f,data[data$treatment==1,])[,-1]
ridge_model.matrix_c <- model.matrix(f,data[data$treatment==0,])[,-1]
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

