source("R Code/evaluation-formulas.R")

require(ggplot2)
require(scales)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

data.zoo <- c("FashionA", "FashionB", "BooksToys", "Hillstrom")
model.zoo <- c("2Model_Ridge","2Model_Ridge_60","causalTree", "causalTree_large", "causalForest","causalForest_60", "causalBart", "causalBart_60", "causalBoosting", "ensemble")

qini.matrix <- setNames(data.frame(matrix(ncol = 4, nrow = 10)), data.zoo)
rownames(qini.matrix) <- model.zoo

max.profit.model <- setNames(data.frame(matrix(ncol = 4, nrow = 10)), data.zoo)
rownames(max.profit.model) <- model.zoo

treatment.decision <- setNames(data.frame(matrix(ncol = 4, nrow = 10)), data.zoo)
rownames(treatment.decision) <- model.zoo

# loading data F_A 25 features ------------------------------------------------------------

f_a.test <- read.csv("working data/eval/f_a_test_eval.csv")[,-1]

f_a_pred.matrix <- setNames(data.frame(matrix(ncol = 10, nrow = nrow(f_a.test))), model.zoo)

f_a_pred.matrix$`2Model_Ridge` <- read.csv2("final predictions/f_a/ridge_f_a_uplift_28.csv")[,2]
f_a_pred.matrix$`2Model_Ridge_60` <- read.csv2("final predictions/f_a/ridge_f_a_uplift_60.csv")[,2]
f_a_pred.matrix$causalTree <- read.csv("final predictions/f_a/f_a_ct.hon.pred_6_0_1_28.csv")[,2]
f_a_pred.matrix$causalTree_large <- read.csv("final predictions/f_a/f_a_ct.hon.pred_7_0_1_28.csv")[,2]
f_a_pred.matrix$causalForest <- read.csv2("final predictions/f_a/cf_f_a.preds_28.csv")[,2]
f_a_pred.matrix$causalForest_60 <- read.csv2("final predictions/f_a/cf_f_a.preds_60.csv")[,2]
f_a_pred.matrix$causalBart <- read.csv("final predictions/f_a/bart_f_a_preds_28.csv")[,2]
f_a_pred.matrix$causalBart_60 <- read.csv("final predictions/f_a/bart_f_a_preds_60.csv")[,2]
f_a_pred.matrix$causalBoosting <- read.csv("final predictions/f_a/cv.cb_f_a.preds.csv")[,2]
f_a_pred.matrix$ensemble <- rowMeans(f_a_pred.matrix[,1:9])

f_a_eval.2M <- list(title = c("2Model_Ridge")  ,           name=c("FashionA") ,  predictions = f_a_pred.matrix$`2Model_Ridge`)
f_a_eval.2M_60 <- list(title = c("2Model_Ridge_60")  ,     name=c("FashionA") ,  predictions = f_a_pred.matrix$`2Model_Ridge_60`)
f_a_eval.CT <- list(title = c("causalTree")    ,           name=c("FashionA") ,  predictions = f_a_pred.matrix$causalTree)
f_a_eval.CT_large <- list(title = c("causalTree_large"),   name=c("FashionA") ,  predictions = f_a_pred.matrix$causalTree_large)
f_a_eval.CF <- list(title = c("causalForest")  ,           name=c("FashionA") ,  predictions = f_a_pred.matrix$causalForest)
f_a_eval.CF_60 <- list(title = c("causalForest_60")  ,     name=c("FashionA") ,  predictions = f_a_pred.matrix$causalForest_60)
f_a_eval.BC <- list(title = c("causalBart")    ,           name=c("FashionA") ,  predictions = f_a_pred.matrix$causalBart)
f_a_eval.BC_60 <- list(title = c("causalBart_60"),         name=c("FashionA") ,  predictions = f_a_pred.matrix$causalBart_60)
f_a_eval.CB <- list(title = c("causalBoosting"),           name=c("FashionA") ,  predictions = f_a_pred.matrix$causalBoosting)
f_a_eval.ensemble <- list(title = c("ensemble"),           name=c("FashionA") ,  predictions = f_a_pred.matrix$ensemble)


f_a_eval_FULL <- list(f_a_eval.2M,f_a_eval.2M_60, f_a_eval.CT, f_a_eval.CT_large ,f_a_eval.CF, f_a_eval.CF_60, f_a_eval.BC,f_a_eval.BC_60, f_a_eval.CB, f_a_eval.ensemble, test.data=f_a.test)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
for(i in seq_along(1:10)){
  # general performance of model for plots and Qini
  model.performance <- predEval(t.d=f_a_eval_FULL$test.data,p=f_a_eval_FULL[[i]]$predictions)
  # plotting Average Revenue Per Decile (ARPD)
  predARPD(n=f_a_eval_FULL[[i]]$name,t=f_a_eval_FULL[[i]]$title)
  # plotting Uplift Per Decile (UPD)
  predUPD(n=f_a_eval_FULL[[i]]$name,t=f_a_eval_FULL[[i]]$title)
  # Computing Qini Score for all models
  qini.matrix <- predQini_score(n=f_a_eval_FULL[[i]]$name,t=f_a_eval_FULL[[i]]$title)
  # Plotting Qini Curve and saving Plot to getwd()
  predQini_plot(n=f_a_eval_FULL[[i]]$name,t=f_a_eval_FULL[[i]]$title)
  # calculating Profit per Model and best Deciles to treat for profit maximization
  model.profit <- predProfit(n=f_a_eval_FULL[[i]]$name, t=f_a_eval_FULL[[i]]$title, t.d=f_a_eval_FULL$test.data, p=f_a_eval_FULL[[i]]$predictions)
  max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
  treatment.decision <- model.profit[[2]]
}

# loading data F_B 25 features ------------------------------------------------------------
f_b.test <- read.csv("working data/eval/f_b_test_eval.csv")[,-1]

f_b_pred.matrix <- setNames(data.frame(matrix(ncol = 10, nrow = nrow(f_b.test))), model.zoo)

f_b_pred.matrix$`2Model_Ridge` <- read.csv2("final predictions/f_b/ridge_f_b_uplift_28.csv")[,2]
f_b_pred.matrix$`2Model_Ridge_60` <- read.csv2("final predictions/f_b/ridge_f_b_uplift_60.csv")[,2]
f_b_pred.matrix$causalTree <- read.csv("final predictions/f_b/f_b_ct.hon.pred_6_0_1_28.csv")[,2]
f_b_pred.matrix$causalTree_large <- read.csv("final predictions/f_b/f_b_ct.hon.pred_7_0_1_28.csv")[,2]
f_b_pred.matrix$causalForest <- read.csv2("final predictions/f_b/cf_f_b.preds_28.csv")[,2]
f_b_pred.matrix$causalForest_60 <- read.csv2("final predictions/f_b/cf_f_b.preds_60.csv")[,2]
f_b_pred.matrix$causalBart <- read.csv("final predictions/f_b/bart_f_b_preds_28.csv")[,2]
f_b_pred.matrix$causalBart_60 <- read.csv("final predictions/f_b/bart_f_b_preds_60.csv")[,2]
f_b_pred.matrix$causalBoosting <- read.csv("final predictions/f_b/cv.cb_f_b_preds.csv")[,2]
f_b_pred.matrix$ensemble <- rowMeans(f_b_pred.matrix[,1:9])

f_b_eval.2M <- list(title = c("2Model_Ridge")  ,        name=c("FashionB") ,  predictions = f_b_pred.matrix$`2Model_Ridge`)
f_b_eval.2M_60 <- list(title = c("2Model_Ridge_60")  ,  name=c("FashionB") ,  predictions = f_b_pred.matrix$`2Model_Ridge_60`)
f_b_eval.CT <- list(title = c("causalTree")    ,        name=c("FashionB") ,  predictions = f_b_pred.matrix$causalTree)
f_b_eval.CT_large <- list(title = c("causalTree_large"),name=c("FashionB") ,  predictions = f_b_pred.matrix$causalTree_large)
f_b_eval.CF <- list(title = c("causalForest")  ,        name=c("FashionB") ,  predictions = f_b_pred.matrix$causalForest)
f_b_eval.CF_60 <- list(title = c("causalForest_60")  ,  name=c("FashionB") ,  predictions = f_b_pred.matrix$causalForest_60)
f_b_eval.BC <- list(title = c("causalBart")    ,        name=c("FashionB") ,  predictions = f_b_pred.matrix$causalBart)
f_b_eval.BC_60 <- list(title = c("causalBart_60")    ,  name=c("FashionB") ,  predictions = f_b_pred.matrix$causalBart_60)
f_b_eval.CB <- list(title = c("causalBoosting"),        name=c("FashionB") ,  predictions = f_b_pred.matrix$causalBoosting)
f_b_eval.ensemble <- list(title = c("ensemble"),        name=c("FashionB") ,  predictions = f_b_pred.matrix$ensemble)

f_b_eval_FULL <- list(f_b_eval.2M,f_b_eval.2M_60,f_b_eval.CT, f_b_eval.CT_large, f_b_eval.CF,f_b_eval.CF_60, f_b_eval.BC, f_b_eval.BC_60, f_b_eval.CB, f_b_eval.ensemble, test.data=f_b.test)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
for(i in seq_along(1:10)){
  # general performance of model for plots and Qini
  model.performance <- predEval(t.d=f_b_eval_FULL$test.data,p=f_b_eval_FULL[[i]]$predictions)
  # plotting Average Revenue Per Decile (ARPD)
  predARPD(n=f_b_eval_FULL[[i]]$name,t=f_b_eval_FULL[[i]]$title)
  # plotting Uplift Per Decile (UPD)
  predUPD(n=f_b_eval_FULL[[i]]$name,t=f_b_eval_FULL[[i]]$title)
  # Computing Qini Score for all models
  qini.matrix <- predQini_score(n=f_b_eval_FULL[[i]]$name,t=f_b_eval_FULL[[i]]$title)
  # Plotting Qini Curve and saving Plot to getwd()
  predQini_plot(n=f_b_eval_FULL[[i]]$name,t=f_b_eval_FULL[[i]]$title)
  # calculating Profit per Model and best Deciles to treat for profit maximization
  model.profit <- predProfit(n=f_b_eval_FULL[[i]]$name, t=f_b_eval_FULL[[i]]$title, t.d=f_b_eval_FULL$test.data, p=f_b_eval_FULL[[i]]$predictions)
  max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
  treatment.decision <- model.profit[[2]]
}

# loading data B_T 25 features ------------------------------------------------------------
b_t.test <- read.csv("working data/eval/b_t_test_eval.csv")[,-1]

b_t_pred.matrix <- setNames(data.frame(matrix(ncol = 10, nrow = nrow(b_t.test))), model.zoo)
#can only add four sets since B_T needed different (smaller) training sets for CausalTree, as an additional estimation set was needed...

b_t_pred.matrix$`2Model_Ridge` <- read.csv2("final predictions/b_t/ridge_b_t_uplift_28.csv")[,2]
b_t_pred.matrix$`2Model_Ridge_60` <- read.csv2("final predictions/b_t/ridge_b_t_uplift_60.csv")[,2]
b_t_pred.matrix$causalForest <- read.csv2("final predictions/b_t/cf_b_t.preds_28.csv")[,2]
b_t_pred.matrix$causalForest_60 <- read.csv2("final predictions/b_t/cf_b_t.preds_60.csv")[,2]
b_t_pred.matrix$causalBart <- read.csv("final predictions/b_t/bart_b_t_preds_28.csv")[,2]
b_t_pred.matrix$causalBart_60 <- read.csv("final predictions/b_t/bart_b_t_preds_60.csv")[,2]
b_t_pred.matrix$causalBoosting <- read.csv("final predictions/b_t/cv.cb_b_t_preds.csv")[,2]
b_t_pred.matrix$ensemble <- rowMeans(b_t_pred.matrix[,c(1,2,5:9)]) # ensemble without causaltree

b_t_eval.2M <- list(title = c("2Model_Ridge")  , name=c("BooksToys")  , predictions = b_t_pred.matrix$`2Model_Ridge`)
b_t_eval.2M_60 <- list(title = c("2Model_Ridge_60")  , name=c("BooksToys")  , predictions = b_t_pred.matrix$`2Model_Ridge_60`)
b_t_eval.CF <- list(title = c("causalForest")  , name=c("BooksToys") ,  predictions = b_t_pred.matrix$causalForest)
b_t_eval.CF_60 <- list(title = c("causalForest_60")  , name=c("BooksToys") ,  predictions = b_t_pred.matrix$causalForest_60)
b_t_eval.BC <- list(title = c("causalBart")    , name=c("BooksToys") ,  predictions = b_t_pred.matrix$causalBart)
b_t_eval.BC_60 <- list(title = c("causalBart_60")    , name=c("BooksToys") ,  predictions = b_t_pred.matrix$causalBart_60)
b_t_eval.CB <- list(title = c("causalBoosting"), name=c("BooksToys") ,  predictions = b_t_pred.matrix$causalBoosting)
b_t_eval.ensemble <- list(title = c("ensemble"), name=c("BooksToys") ,  predictions = b_t_pred.matrix$ensemble)

b_t_eval_FULL <- list(b_t_eval.2M, b_t_eval.2M_60, b_t_eval.CF, b_t_eval.CF_60, b_t_eval.BC, b_t_eval.BC_60, b_t_eval.CB, b_t_eval.ensemble, test.data=b_t.test)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
# running the complete evaluation functions
for(i in seq_along(1:8)){
  # general performance of model for plots and Qini
  model.performance <- predEval(t.d=b_t_eval_FULL$test.data,p=b_t_eval_FULL[[i]]$predictions)
  # plotting Average Revenue Per Decile (ARPD)
  predARPD(n=b_t_eval_FULL[[i]]$name,t=b_t_eval_FULL[[i]]$title)
  # plotting Uplift Per Decile (UPD)
  predUPD(n=b_t_eval_FULL[[i]]$name,t=b_t_eval_FULL[[i]]$title)
  # Computing Qini Score for all models
  qini.matrix <- predQini_score(n=b_t_eval_FULL[[i]]$name,t=b_t_eval_FULL[[i]]$title)
  # Plotting Qini Curve and saving Plot to getwd()
  predQini_plot(n=b_t_eval_FULL[[i]]$name,t=b_t_eval_FULL[[i]]$title)
  # calculating Profit per Model and best Deciles to treat for profit maximization
  model.profit <- predProfit(n=b_t_eval_FULL[[i]]$name, t=b_t_eval_FULL[[i]]$title, t.d=b_t_eval_FULL$test.data, p=b_t_eval_FULL[[i]]$predictions)
  max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
  treatment.decision <- model.profit[[2]]
}

#Manual computation of causalTree Evaluation
b_t.test_ct <- read.csv("working data/eval/b_t_test_alt_eval.csv")[,-1]
b_t_pred.causalTree <- read.csv("final predictions/b_t/b_t_ct.hon.pred_7_0_1_28.csv")[,2]
title <- c("causalTree")   ; test.data = b_t.test_ct ; name=c("BooksToys")  ;  predictions = b_t_pred.causalTree

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
model.performance <- predEval(t.d=test.data,p=predictions)
predARPD(name,title)
predUPD(name,title)
qini.matrix <- predQini_score(name,title)
predQini_plot(name,title)
model.profit <- predProfit(n=name, t=title, t.d=test.data, p=predictions)
max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
treatment.decision <- model.profit[[2]]



# loading data Hillstrom ------------------------------------------------------------
h_s.test <- read.csv("working data/h_s.test.csv")[,-1]
h_s.test$checkoutAmount <-h_s.test$spend
h_s.test$eligibility <- h_s.test$treatment # everyone who is treated is also eligible for "discount"
h_s.test$campaignMov <- 0
h_s.test$campaignUnit <- "CURRENCY"
h_s.test$campaignUnit <- factor(h_s.test$campaignUnit, levels=("CURRENCY"))
h_s.test$campaignValue <- 5 # 5 cent
h_s.test$ExpectedDiscount <- 0.05 #in €, there is no discount, but we assume a cost of 0.05€ for each treated individual


h_s_pred.matrix<- setNames(data.frame(matrix(ncol = 10, nrow = nrow(h_s.test))), model.zoo)
#can only add four sets since B_T needed different (smaller) training sets for CausalTree, as an additional estimation set was needed...

h_s_pred.matrix$`2Model_Ridge` <- read.csv("final predictions/h_s/ridge_h_s_uplift.csv")[,2]
h_s_pred.matrix$causalForest <- read.csv("final predictions/h_s/cf_h_s.preds.csv")[,2]
h_s_pred.matrix$causalBart <- read.csv("final predictions/h_s/bart_h_s_pred.csv")[,2]
h_s_pred.matrix$causalBoosting <- read.csv("final predictions/h_s/cb_hllstrm_pred_50trees.csv")[,2]
h_s_pred.matrix$ensemble <- rowMeans(h_s_pred.matrix[,c(1,5,7,9)]) # ensemble without causaltree

h_s_eval.2M <- list(title = c("2Model_Ridge")  , name=c("Hillstrom")  , predictions = h_s_pred.matrix$`2Model_Ridge`)
h_s_eval.CF <- list(title = c("causalForest")  , name=c("Hillstrom") ,  predictions = h_s_pred.matrix$causalForest)
h_s_eval.BC <- list(title = c("causalBart")    , name=c("Hillstrom") ,  predictions = h_s_pred.matrix$causalBart)
h_s_eval.CB <- list(title = c("causalBoosting"), name=c("Hillstrom") ,  predictions = h_s_pred.matrix$causalBoosting)
h_s_eval.ensemble <- list(title = c("ensemble"), name=c("Hillstrom") ,  predictions = h_s_pred.matrix$ensemble)

h_s_eval_FULL <- list(h_s_eval.2M, h_s_eval.CF, h_s_eval.BC, h_s_eval.CB,h_s_eval.ensemble, test.data=h_s.test)

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
for(i in seq_along(1:5)){
  # general performance of model for plots and Qini
  model.performance <- predEval(t.d=h_s_eval_FULL$test.data,p=h_s_eval_FULL[[i]]$predictions)
  # plotting Average Revenue Per Decile (ARPD)
  predARPD(n=h_s_eval_FULL[[i]]$name,t=h_s_eval_FULL[[i]]$title)
  # plotting Uplift Per Decile (UPD)
  predUPD(n=h_s_eval_FULL[[i]]$name,t=h_s_eval_FULL[[i]]$title)
  # Computing Qini Score for all models
  qini.matrix <- predQini_score(n=h_s_eval_FULL[[i]]$name,t=h_s_eval_FULL[[i]]$title)
  # Plotting Qini Curve and saving Plot to getwd()
  predQini_plot(n=h_s_eval_FULL[[i]]$name,t=h_s_eval_FULL[[i]]$title)
  # calculating Profit per Model and best Deciles to treat for profit maximization
  model.profit <- predProfit(n=h_s_eval_FULL[[i]]$name, t=h_s_eval_FULL[[i]]$title, t.d=h_s_eval_FULL$test.data, p=h_s_eval_FULL[[i]]$predictions)
  max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
  treatment.decision <- model.profit[[2]]
}

#Manual computation of causalTree Evaluation
h_s.test_ct <- read.csv("working data/h_s.test_ct.csv")[,-1]
h_s.test_ct$checkoutAmount <-h_s.test_ct$spend
h_s.test_ct$eligibility <- h_s.test_ct$treatment # everyone who is treated is also eligible for "discount"
h_s.test_ct$campaignMov <- 0
h_s.test_ct$campaignUnit <- "CURRENCY"
h_s.test_ct$campaignUnit <- factor(h_s.test$campaignUnit, levels=("CURRENCY"))
h_s.test_ct$campaignValue <- 5 #5 cent
h_s.test_ct$ExpectedDiscount <- 0.05 # in € there is no discount, but we assume a cost of 1€ for each treated individual

h_s_pred.causalTree <- read.csv("final predictions/h_s/ct_h_s_hon.pred_8_0_1.csv")[,2]
title <- c("causalTree")   ; test.data = h_s.test_ct ; name=c("Hillstrom")  ;  predictions = h_s_pred.causalTree

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")
model.performance <- predEval(t.d=test.data,p=predictions)
predARPD(name,title)
predUPD(name,title)
qini.matrix <- predQini_score(name,title)
predQini_plot(name,title)
model.profit <- predProfit(n=name, t=title, t.d=test.data, p=predictions)
max.profit.model <- model.profit[[1]] # extracting results from predProfit (returns list() with two elements) and storing
treatment.decision <- model.profit[[2]]
