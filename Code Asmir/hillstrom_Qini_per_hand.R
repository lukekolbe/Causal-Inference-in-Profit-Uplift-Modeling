# install.packages("grf")
# install.packages("uplift")
# install.packages("devtools")
# install.packages("caret")
# install_github("susanathey/causalTree")
# install_github("saberpowers/causalLearning")
# library(devtools) 

library(devtools)
library(causalTree)
library(caret)
library(grf)
library("uplift")
library(causalLearning)
library(tidyverse)
#install.packages("tools4uplift")
#library(tools4uplift)
library(causalLearning)
library(causalTree)

set.seed(101010)

getwd()
setwd("~/Desktop/apa_data")
hllstrm = read.csv("Hillstrom.csv", sep=",")

str(hllstrm)
summary(hllstrm)

# hllstrm$mens <- as.factor(hllstrm$mens)
# hllstrm$womens <- as.factor(hllstrm$womens)
# hllstrm$newbie <- as.factor(hllstrm$newbie)
# hllstrm$conversion <- as.factor(hllstrm$conversion)

summary(hllstrm$segment)
summary(hllstrm$spend)
table(hllstrm$spend>0)
table(hllstrm$segment)

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

# TARGET VARIABLE TRANSFORMATION ACCORDING TO GUBELA ----------------------

# hllstrm$z_var1 <- 0
# 
# for (i in seq_along(1:nrow(hllstrm))){
#   
#   if(hllstrm$treatment[i]==1&&hllstrm$spend[i]>0){ 
#     hllstrm$z_var1[i] <- abs(hllstrm$spend[i])
#   } 
#   else if(hllstrm$treatment[i]==0&&hllstrm$spend[i]>0){
#     hllstrm$z_var1[i] <- -abs(hllstrm$spend[i])
#   } 
#   else{
#     hllstrm$z_var1[i] <- 0
#   }
# }
# 
# 
# summary(hllstrm$z_var1)
# str(hllstrm)
# 
# 
# hllstrm$z_var2 <- ifelse(hllstrm$z_var1>0, 1, 0)
# summary(hllstrm$z_var2)
# str(hllstrm)
# 


# SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

# mens <- rbind(hllstrm[hllstrm$segment=="Mens E-Mail",],hllstrm[hllstrm$segment=="No E-Mail",])
# womens <- rbind(hllstrm[hllstrm$segment=="Womens E-Mail",],hllstrm[hllstrm$segment=="No E-Mail",])
# control <- hllstrm[hllstrm$segment=="No E-Mail",]
# 
# table(mens$spend>0, mens$segment)
# table(womens$spend>0, womens$segment)
# table(control$spend>0, control$segment)

#### THIS TRIES TO STRATIFY ALONG THE VARIABLES CONVERSION AND TREATMENT (WITH THREE GROUPS)
#### it fails
#### WE SHOULD INSTEAD SPLIT THE POPULATION ALONG THE LINES OF THE TREATMENT AND DO 50/50 SPLITS
# train_indices <- list()
# combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c("Mens E-Mail","No E-Mail","Womens E-Mail")))
# 
# xtabs(~conversion+treatment, hllstrm)
# sample_size <- as.numeric(xtabs(~conversion+treatment, hllstrm))
# 
# for(i in 1:6){
#   train_indices[[i]] <- sample( which(hllstrm$conversion == combinations$Conversion[i] &
#                                         hllstrm$segment == combinations$Treatment[i])
#                                 , size = round(0.25*sample_size[i]), replace=FALSE)
# }
# trainIndex <- c(train_indices, recursive=TRUE)
# 
# trainData <- hllstrm[trainIndex,]
# testData  <- hllstrm[-trainIndex,]
# 
# summary(trainData[,c("Conversion","Treatment")])
# summary( testData[,c("Conversion","Treatment")])

train_indices_all <- list()
# train_indices_mens <- list()
# train_indices_womens <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1)))

# xtabs(~conversion+treatment, hllstrm)
# xtabs(~conversion+treatment, mens)
# xtabs(~conversion+treatment, womens)
# xtabs(~conversion+treatment, control)

sample_size_all <- as.numeric(xtabs(~conversion+treatment, hllstrm))
# sample_size_mens <- as.numeric(xtabs(~conversion+treatment, mens))
# sample_size_womens <- as.numeric(xtabs(~conversion+treatment, womens))

# for(i in 1:4){
#   train_indices[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
#                                             hllstrm$treatment == combinations$Treatment[i])
#                                     , size = round(0.75*sample_size[i]), replace=FALSE) 
# } 

for(i in 1:4){
  train_indices_all[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
                                           hllstrm$treatment == combinations$Treatment[i])
                                   , size = round(0.25*sample_size_all[i]), replace=FALSE) 
} 

# for(i in 1:4){
#   train_indices_mens[[i]] <- sample(which(mens$conversion == combinations$Conversion[i] &
#                                             mens$treatment == combinations$Treatment[i])
#                                     , size = round(0.25*sample_size_mens[i]), replace=FALSE) 
# } 
# 
# for(i in 1:4){
#   train_indices_womens[[i]] <- sample(which(womens$conversion == combinations$Conversion[i] &
#                                               womens$treatment == combinations$Treatment[i])
#                                       , size = round(0.25*sample_size_womens[i]), replace=FALSE) 
# } 

# trainIndex <- c(train_indices, recursive=TRUE)

trainIndex_all <- c(train_indices_all, recursive=TRUE)
# trainIndex_mens <- c(train_indices_mens, recursive=TRUE)
# trainIndex_womens <- c(train_indices_womens, recursive=TRUE)

# trainData <- hllstrm[trainIndex,]
# testData  <- hllstrm[-trainIndex,]

trainData_all <- hllstrm[-trainIndex_all,]
testData_all  <- hllstrm[trainIndex_all,]

# trainData_mens <- mens[-trainIndex_mens,]
# testData_mens  <- mens[trainIndex_mens,]
# 
# trainData_womens <- womens[-trainIndex_womens,]
# testData_womens  <- womens[trainIndex_womens,]

# table(trainData_mens$spend>0, trainData_mens$segment)
# table(trainData_womens$spend>0, trainData_womens$segment)
table(trainData_all$spend>0, trainData_all$segment)


summary(hllstrm[,c("conversion","treatment")])
summary(testData_all[,c("conversion","treatment")])
summary(trainData_all[,c("conversion","treatment")])

#cleaning the mens and womens set of any control group >> necessary for two-model
# mens <- mens[mens$segment=="Mens E-Mail",]
# womens <- womens[hllstrm$segment=="Womens E-Mail",]

# Average Treatment Effect (ATE) ---------------------------------------------------

experiment <- table(list("Treated" = hllstrm$treatment, "Converted" = hllstrm$conversion))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
mean(as.numeric(hllstrm$conversion[hllstrm$treatment==1])) - mean(as.numeric(hllstrm$conversion[hllstrm$treatment==0]))
mean(hllstrm$spend[hllstrm$treatment==1]) - mean(hllstrm$spend[hllstrm$treatment==0]) 
#mean(mens$spend[mens$treatment==1]) - mean(mens$spend[mens$treatment==0]) # men have a higher average treatment effect
#mean(womens$spend[womens$treatment==1]) - mean(womens$spend[womens$treatment==0]) # women have a lower average treatment effect than men or the average

# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )




# DATA SAMPLE INVESTIGATION -----------------------------------------------

# Check whether the data has been randomly assigned. This is an important assumptions in uplift modeling and, more generally, experimental designs. To verify a random assignment, we have to check the balance of the A/B indicator. The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates. In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group
#library("uplift")
cb <- checkBalance(treatment~.-conversion, data = trainData_mens, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))

# Balance properties of first ten covariates 
# Be aware that the results are saved as a tensor or '3D matrix'.
dim(cb$results)
round(cb$results[,,], 2)
# The function automatically computes a chi-squared test for the conditional independence of the covariates to the treatment variable
cb$overall

# The test rejects the null hypothesis that the sample is balanced, i.e. truly randomized, at the 1% level. The reason is possibly that the customer were not assigned randomly to the treatment and control group, but based on some non-random procedure, e.g. an existing model or some decision rule.
# Think deeply about how this will impact your model!


# Causal Tree ALL ------------------------------------------------------

library(causalTree)

names(trainData_all)

tree_all <- causalTree(spend~recency + history + history_segment + zip_code + channel + mens + womens + newbie, data = trainData_all, treatment = trainData_all$treatment,
                       split.Rule = "TOT", cv.option = "TOT", split.Honest = T, cv.Honest = F, split.Bucket = F,
                       xval = 10, cp = 0.0001, minsize = 50, propensity = 0.66667)

tree_all <- causalTree(spend~recency + history + history_segment + zip_code + channel + mens + womens + newbie, data = trainData_all, treatment = trainData_all$treatment,
                       split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = F, split.Bucket = F,
                       xval = 10, cp = 0.0001, minsize = 50, propensity = 0.66667)

tree_all$cptable
rpart.plot(tree_all)
summary(tree_all)


# Causal Tree MENS ------------------------------------------------------

# 
# str(trainData_mens)
# str(trainData_womens)
# 
# 
# 
# tree_men <- causalTree(spend~recency + history + history_segment + zip_code + channel + mens + womens + newbie, data = trainData_mens, treatment = trainData_mens$treatment,
#                        split.Rule = "TOT", cv.option = "TOT", split.Honest = T, cv.Honest = F, split.Bucket = F,
#                        xval = 10, cp = 0.0002, minsize = 30, propensity = 0.5)
# 
# tree_men <- causalTree(spend~recency + history + history_segment + zip_code + channel + mens + womens + newbie,
#                        data = trainData_mens, treatment = trainData_mens$treatment,
#                        split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = F, split.Bucket = F,
#                        cp = 0, xval = 5 , minsize = 20, propensity = 0.5)
# tree_men$cptable
# 
# rpart.plot(tree_men)
# summary(tree_men)
# 
# 
# 
# opcp <- tree_men$cptable[,1][which.min(tree_men$cptable[,4])]
# opfit <- prune(tree_men, cp=opcp)
# rpart.plot(opfit)
# 
# 
# pred_CausalTree_mens <- predict(object = tree_men, newdata = testData_mens)
# # The predictions differentiate between the treatment and control condition
# # pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# # pr.y1_ct1 gives an estimate for a person to convert when in the control group
# head(pred_CausalTree_mens) 
# summary(pred_CausalTree_mens)
# 
# # Our goal is to identify the people for whom the treatment will lead to a large increase 
# # in conversion probability, i.e. where the difference between the treatment prob. and the
# # control prob. is positive and high
# pred_mens[["CausalTree"]] <- pred_CausalTree_mens[, 1] - pred_CausalTree_mens[, 2]
# # We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
# summary(pred_mens[["CausalTree"]])
# head(pred_mens)

# Causal Tree WOMENS ------------------------------------------------------

# tree_women <- causalTree(spend~recency + history + history_segment + zip_code + channel + mens + womens + newbie, 
#                          data = trainData_womens, treatment = trainData_womens$treatment,
#                          split.Rule = "TOT", cv.option = "TOT", split.Honest = T, cv.Honest = F, split.Bucket = F, 
#                          xval = 10, cp = 0.0005, minsize = 30, propensity = 0.5)
# 
# #### STEEP FALLOFF depending on cp and minsize tuning!
# 
# #opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]
# #opfit <- prune(tree, opcp)
# 
# 
# tree_women$cptable
# rpart.plot(tree_women)
# summary(tree_women)





# CausalForest ------------------------------------------------------------

names(trainData_all)
str(trainData_all)


cf_hillstrom <- causal_forest(
  X = trainData_all[, -c(2,6,8,9,11,12,13)], #excluding factors (dummified above) and Y-Variables
  Y = trainData_all$spend,
  W = trainData_all$treatment,
  num.trees = 1000,
  honesty = TRUE,
  honesty.fraction = NULL,
  tune.parameters=TRUE,
  seed = 1839
)
# RDS LADEN für CF
cf_hillstrom <- readRDS("cf_hillstrom.RDS")
summary(cf_hillstrom)

cf_hillstrom_preds <- predict(object = cf_hillstrom,
                              newdata=testData_all[, -c(2,6,8,9,11,12,13)],
                              estimate.variance = TRUE)
cf_hillstrom_preds = cf_hillstrom_preds[,1]

set.seed(123) # because of randomness involved 

# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_cf = cbind(uplift = cf_hillstrom_preds,
           target = testData_all$spend,
           treatment = testData_all$treatment,
           uplift_rank = length(cf_hillstrom_preds) +1 - rank(cf_hillstrom_preds, ties.method = "random"))

#head(mm)
#View(mm)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bk_cf = unique(quantile(mm_cf[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bk_cf)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mm_cf = cbind(mm_cf, decile = cut(mm_cf[,4], breaks = bk_cf, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mm_cf)
#View(mm_cf)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

cf_n.y1_ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
cf_n.y1_ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
cf_r.y1_ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
cf_r.y1_ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
cf_n.ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
cf_n.ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
cf_n.y1_ct0
cf_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
cf_r.y1_ct0 <- ifelse(is.na(cf_r.y1_ct0), 0, cf_r.y1_ct0)
cf_r.y1_ct1 <- ifelse(is.na(cf_r.y1_ct1), 0, cf_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
df_cf <- merge(cbind(cf_n.y1_ct0, cf_r.y1_ct0, cf_n.ct0), cbind(cf_n.y1_ct1, cf_r.y1_ct1, cf_n.ct1), by= "row.names", all = TRUE)             

df_cf$Row.names <- as.numeric(df_cf$Row.names)
df_cf[, c(2, 4, 5, 7)][is.na(df_cf[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

df_cf$uplift = df_cf$cf_r.y1_ct1 - df_cf$cf_r.y1_ct0

df_cf <- df_cf[order(df_cf$Row.names), ] # Ordering according to row-names.

perf_cf <- cbind(group   = df_cf$Row.names,
              n.ct1    = df_cf$cf_n.ct1,
              n.ct0    = df_cf$cf_n.ct0, 
              n.y1_ct1 = df_cf$cf_n.y1_ct1,
              n.y1_ct0 = df_cf$cf_n.y1_ct0,
              r.y1_ct1 = df_cf$cf_r.y1_ct1, 
              r.y1_ct0 = df_cf$cf_r.y1_ct0,
              uplift   = df_cf$uplift)

class(perf_cf) <- "performance"

perf_cf

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.df_cf.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf[,6], Group = "treatment")
temp.df_cf.control <- data.frame(Decile = seq(1:10), averageRevenue = perf[,7], Group = "control")
temp.df_cf <- rbind(temp.df_cf.treatment, temp.df_cf.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df_cf, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,3), name="Average Revenue (Euro)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Average Revenue Per Decile - Causal Forest") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.df_cf.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf[,6] - perf[,7])
require(ggplot2)
require(scales)
ggplot(temp.df_cf.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-1,3), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Causal Forest") + theme(plot.title = element_text(face="bold", size=20)) +
  guides(fill=FALSE)
## QINI PLOT
# One way of representing the performance of an upift technique as a single number is through
# the QINI Coefficient and the accompanging QINI Curve

# The ideal is to calculate the incremental gains:

# - First the cumulitative sum of the treated and the control groups are calculated with respect
# to the total population in each group at the specified decile.
# - Afterwards we calculcate the percentage of the total amount of people (control and treated) present in each decile.
#cumsum(perf[,"n.y1_ct1"])
#cumsum(perf[,"n.ct1"])
#cumsum(perf[,"n.y1_ct0"])
#cumsum(perf[,"n.ct0"])
cf_r.cumul.y1_ct1 <- cumsum(perf_cf[,"n.y1_ct1"]) / cumsum(perf_cf[,"n.ct1"])
#r.cumul.y1_ct1
cf_r.cumul.y1_ct0 <- cumsum(perf_cf[,"n.y1_ct0"]) / cumsum(perf_cf[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

cf_r.cumul.y1_ct1[is.na(cf_r.cumul.y1_ct1)] <- 0
cf_r.cumul.y1_ct0[is.na(cf_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
cf_inc.gains = c(0.0,(cf_r.cumul.y1_ct1 - cf_cr.cumul.y1_ct0) * deciles)
cf_inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
cf_overall.inc.gains <- sum(perf_cf[, "n.y1_ct1"]) / sum(perf_cf[, "n.ct1"]) - sum(perf_cf[, "n.y1_ct0"]) / sum(perf_cf[, "n.ct0"])
cf_overall.inc.gains

### Random incremental gains
cf_random.inc.gains <- c(0, cumsum(rep(cf_overall.inc.gains / groups, groups)))
cf_random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
cf_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
cf_x
cf_y <- cf_inc.gains
cf_y

cf_auuc <- 0
cf_auuc.rand <- 0

for (i in 2:length(x)) {
  cf_auuc <- cf_auuc + 0.5 * (cf_x[i] - cf_x[i-1]) * (cf_y[i] + cf_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
cf_y.rand <- cf_random.inc.gains

for (i in 2:length(x)) {
  cf_auuc.rand <- cf_auuc.rand + 0.5 * (cf_x[i] - cf_x[i-1]) * (cf_y.rand[i] + cf_y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
cf_Qini <- cf_auuc - cf_auuc.rand
cf_Qini
cf_miny <- 100 * min(c(cf_random.inc.gains, cf_inc.gains))
cf_maxy <- 100 * max(c(cf_random.inc.gains, cf_inc.gains))

# The last step is to plot the QINI-curve
plot(cf_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(cf_miny, cf_maxy))
lines(cf_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("Model", "Random"), 
       col=c("blue", "red"), lty=c(2,1))
#saveRDS(cf_hillstrom, "cf_hillstrom.RDS")

# CAUSAL BOOSRING (QINI HANDMADE) -------------------------------
# RDS LADEN für CAUSAL BOOSTING
#str(trainData_all)
#names(trainData_all[,-which(names(trainData_all) %in% c("segment","history_segment","zip_code","channel"))])
#names(trainData_all)

#prop.table(table(trainData_all$treatment))

#cv.cb_hillstrom <- cv.causalBoosting(trainData_all[, -c(2,6,8,9,11,12,13)],
                                     #tx=trainData_all$treatment, 
                                     #y=trainData_all$spend,
                                     #num.trees=500,
                                     #eps=0.3)

#saveRDS(cb_hillstrom, "cb_hillstrom.rds")
cv.cb_hillstrom <- readRDS("cv.cb_hillstrom.rds")

summary(cv.cb_hillstrom)


cb_hillstrom_preds <- predict(cv.cb_hillstrom, 
                           newx = testData_all[, -c(2,6,8,9,11,12,13)], 
                           type = "treatment.effect",
                           num.trees = 500,
                           honest = FALSE,
                           naVal = 0)
head(cb_hillstrom_preds)

summary(cb_hillstrom_preds)
summary(trainData_all$spend)

set.seed(123) # because of randomness involved 

#table(ifelse(testData_all$treatment == 1, 0, 1))


# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm = cbind(uplift = cb_hillstrom_preds,
           target = testData_all$spend,
           treatment = testData_all$treatment,
           uplift_rank = length(cb_hillstrom_preds) +1 - rank(cb_hillstrom_preds, ties.method = "random"))
           
#head(mm)
#View(mm)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bk = unique(quantile(mm[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bk)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mm = cbind(mm, decile = cut(mm[,4], breaks = bk, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mm)
View(mm)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

n.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
n.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
r.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
r.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
n.ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
n.ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
n.y1_ct0
n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
r.y1_ct0 <- ifelse(is.na(r.y1_ct0), 0, r.y1_ct0)
r.y1_ct1 <- ifelse(is.na(r.y1_ct1), 0, r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
df <- merge(cbind(n.y1_ct0, r.y1_ct0, n.ct0), cbind(n.y1_ct1, r.y1_ct1, n.ct1), by= "row.names", all = TRUE)             

df$Row.names <- as.numeric(df$Row.names)
df[, c(2, 4, 5, 7)][is.na(df[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

df$uplift = df$r.y1_ct1 - df$r.y1_ct0

df <- df[order(df$Row.names), ] # Ordering according to row-names.

perf <- cbind(group   = df$Row.names,
              n.ct1    = df$n.ct1,
              n.ct0    = df$n.ct0, 
              n.y1_ct1 = df$n.y1_ct1,
              n.y1_ct0 = df$n.y1_ct0,
              r.y1_ct1 = df$r.y1_ct1, 
              r.y1_ct0 = df$r.y1_ct0,
              uplift   = df$uplift)

class(perf) <- "performance"

perf

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.df.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf[,6], Group = "treatment")
temp.df.control <- data.frame(Decile = seq(1:10), averageRevenue = perf[,7], Group = "control")
temp.df <- rbind(temp.df.treatment, temp.df.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,8), name="Average Revenue (Euro)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Average Revenue Per Decile - Two Model Approach") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.df.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf[,6] - perf[,7])
require(ggplot2)
require(scales)
ggplot(temp.df.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-3,7), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Two Model Approach") + theme(plot.title = element_text(face="bold", size=20)) +
  guides(fill=FALSE)
## QINI PLOT
# One way of representing the performance of an upift technique as a single number is through
# the QINI Coefficient and the accompanging QINI Curve

# The ideal is to calculate the incremental gains:

# - First the cumulitative sum of the treated and the control groups are calculated with respect
# to the total population in each group at the specified decile.
# - Afterwards we calculcate the percentage of the total amount of people (control and treated) present in each decile.
#cumsum(perf[,"n.y1_ct1"])
#cumsum(perf[,"n.ct1"])
#cumsum(perf[,"n.y1_ct0"])
#cumsum(perf[,"n.ct0"])
r.cumul.y1_ct1 <- cumsum(perf[,"n.y1_ct1"]) / cumsum(perf[,"n.ct1"])
#r.cumul.y1_ct1
r.cumul.y1_ct0 <- cumsum(perf[,"n.y1_ct0"]) / cumsum(perf[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

r.cumul.y1_ct1[is.na(r.cumul.y1_ct1)] <- 0
r.cumul.y1_ct0[is.na(r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
inc.gains = c(0.0,(r.cumul.y1_ct1 - r.cumul.y1_ct0) * deciles)
inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
overall.inc.gains <- sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) - sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"])
overall.inc.gains

### Random incremental gains
random.inc.gains <- c(0, cumsum(rep(overall.inc.gains / groups, groups)))
random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
x <- c(0.0, seq(1 / groups, 1, 1 / groups))
x
y <- inc.gains
y

auuc <- 0
auuc.rand <- 0

for (i in 2:length(x)) {
  auuc <- auuc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
y.rand <- random.inc.gains

for (i in 2:length(x)) {
  auuc.rand <- auuc.rand + 0.5 * (x[i] - x[i-1]) * (y.rand[i] + y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
Qini <- auuc - auuc.rand
Qini
miny <- 100 * min(c(random.inc.gains, inc.gains))
maxy <- 100 * max(c(random.inc.gains, inc.gains))

# The last step is to plot the QINI-curve
plot(inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(miny, maxy))
lines(random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("Model", "Random"), 
       col=c("blue", "red"), lty=c(2,1))

# UPLIFT RF (used for variable importance) --------------------------------------------------------
# str(trainData_mens)
# str(trainData_womens)
# table(trainData$z_var2)
# table(testData$z_var2)

trainData_all[,-which(names(trainData_all) %in% c("conversion","spend","treatment", "segment","history_segment","zip_code","channel"))]


upliftRF_hllstrm <- upliftRF(conversion ~ trt(treatment) +.,
                             data = trainData_all[,-which(names(trainData_all) %in% c("spend","segment","history_segment","zip_code","channel"))],
                             mtry = 6,
                             ntree = 1000,
                             split_method = "KL",
                             minsplit = 50,
                             verbose = TRUE)

summary(upliftRF_hllstrm)

saveRDS(upliftRF_hllstrm, "upliftRF_hllstrm.RDS")
upliftRF_hllstrm <- readRDS("upliftRF_hllstrm.RDS")

varImportance(upliftRF_hllstrm, plotit = FALSE, normalize = TRUE)

pred_all <- list()
pred_upliftRF <- predict(object = upliftRF_hllstrm, newdata = testData_all)
head(upliftRF_hllstrm)

pred_womens[["upliftRF"]] <- upliftRF_hllstrm[, 1] - upliftRF_hllstrm[, 2]
summary(pred_womens[["upliftRF"]])

### ONLY WORKS WITH BINARY TARGET


# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF_hllstrm, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred_mens <- list()
pred_upliftRF_mens <- predict(object = upliftRF_hllstrm, newdata = testData_mens)
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


# UPLIFT-RF WOMENS --------------------------------------------------------

# upliftRF_women <- upliftRF(conversion ~ trt(treatment) +recency + history +history_segment + mens + womens + zip_code + newbie + channel,
#                            data = trainData_womens,
#                            mtry = 5,
#                            ntree = 300,
#                            split_method = "KL",
#                            minsplit = 50,
#                            verbose = TRUE)
# summary(upliftRF_women)
varImportance(upliftRF_women, plotit = FALSE, normalize = TRUE)

pred_womens <- list()
pred_upliftRF_womens <- predict(object = upliftRF_women, newdata = testData_womens)
head(pred_upliftRF_womens)

pred_womens[["upliftRF"]] <- pred_upliftRF_womens[, 1] - pred_upliftRF_womens[, 2]
summary(pred_womens[["upliftRF"]])



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


str(trainData_all)
names(trainData_all[,-which(names(trainData_all) %in% c("segment","history_segment","zip_code","channel"))])
names(trainData_all)

prop.table(table(trainData_all$treatment))

cv.cb_hillstrom <- cv.causalBoosting(trainData_all[, -c(2,6,8,9,11,12,13)],
                                     tx=trainData_all$treatment, 
                                     y=trainData_all$spend,
                                     num.trees=500,
                                     eps=0.3)

saveRDS(cb_hillstrom, "cb_hillstrom.rds")
cb_hillstrom <- readRDS("cb_hillstrom.rds")

summary(cb_hillstrom)


cb_hllstrm_pred <- predict(cb_hillstrom, 
                           newx = testData_all[, -c(2,6,8,9,11,12,13)], 
                           type = "treatment.effect",
                           num.trees = 500,
                           honest = FALSE,
                           naVal = 0)

summary(cb_hllstrm_pred)
summary(trainData_all$spend)

# BART --------------------------------------------------------------------



# prediction --------------------------------------------------------------

cb_hillstrom <-read_rds("/Users/lukaskolbe/Dropbox/Uni/APA/cb_hillstrom.rds")



# CARET SMOTE SAMPLING TRYOUT ---------------------------------------------

ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 10,
                     verboseIter = FALSE,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel,
                               data = trainData_mens,
                               method = "glm",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)




# Performance Assessment for Uplift Models  ---------------------------------------------

# Equivalent to the standard model lift, we can calculate the uplift for the sample deciles

treatment_effect_order_mens <- order(pred[['upliftRF']], decreasing=TRUE)
treatment_effect_groups_mens <- cbind(testData[treatment_effect_order, c("Conversion","Treatment")],               "effect_estimate"=pred[["upliftRF"]][treatment_effect_order])

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


