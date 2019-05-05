install.packages("grf")
install.packages("uplift")
install.packages("devtools")
install.packages("caret")


library(grf)
library(devtools) 
library("uplift")

install_github("susanathey/causalTree")
library(causalTree)

install_github("saberpowers/causalLearning")
library(causalLearning)


getwd()
setwd("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data")
hllstrm <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillström Data/hillstrm.csv", sep=",")


str(hllstrm)
summary(hllstrm)

# hllstrm$mens <- as.factor(hllstrm$mens)
# hllstrm$womens <- as.factor(hllstrm$womens)
# hllstrm$newbie <- as.factor(hllstrm$newbie)
# hllstrm$conversion <- as.factor(hllstrm$conversion)

summary(hllstrm$segment)
summary(hllstrm$spend)
table(hllstrm$spend>0)


hllstrm$treatment <- ifelse(hllstrm$segment=="No E-Mail", 0, 1)

hllstrm$z_var1 <- 0

for (i in seq_along(1:nrow(hllstrm))){
  
  if(hllstrm$treatment[i]==1&&hllstrm$spend[i]>0){ 
    hllstrm$z_var1[i] <- abs(hllstrm$spend[i])
  } 
  else if(hllstrm$treatment[i]==0&&hllstrm$spend[i]>0){
    hllstrm$z_var1[i] <- -abs(hllstrm$spend[i])
  } 
  else{
    hllstrm$z_var1[i] <- 0
  }
}


summary(hllstrm$z_var1)
str(hllstrm)


hllstrm$z_var2 <- ifelse(hllstrm$z_var1>0, 1, 0)
summary(hllstrm$z_var2)
str(hllstrm)



      #### THIS TRIES TO STRATIFY ALONG THE VARIABLES CONVERSION AND TREATMENT (WITH THREE GROUPS)
      #### it fails
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


train_indices <- list()
combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1)))

xtabs(~conversion+treatment, hllstrm)
sample_size <- as.numeric(xtabs(~conversion+treatment, hllstrm))

for(i in 1:4){
  train_indices[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
                                        hllstrm$treatment == combinations$Treatment[i])
                                , size = round(0.75*sample_size[i]), replace=FALSE) 
} 
#### this has issues in all cases where the sample size is not a multiple of 4 (0.75 = 3/4)
#### I assume rounding error


trainIndex <- c(train_indices, recursive=TRUE)

trainData <- hllstrm[trainIndex,]
testData  <- hllstrm[-trainIndex,]

summary(trainData[,c("conversion","treatment")])
summary( testData[,c("conversion","treatment")])


## Average Treatment Effect (ATE)
experiment <- table(list("Treated" = hllstrm$treatment, "Converted" = hllstrm$conversion))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
mean(as.numeric(hllstrm$conversion[hllstrm$treatment==1])) - mean(as.numeric(hllstrm$conversion[hllstrm$treatment==0]))
mean(hllstrm$spend[hllstrm$treatment==1]) - mean(hllstrm$spend[hllstrm$treatment==0])

# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )





# Check whether the data has been randomly assigned. This is an important assumptions in uplift modeling and, more generally, experimental designs. To verify a random assignment, we have to check the balance of the A/B indicator. The function checkBalance calculates standardized mean differences along each covariate and tests for conditional independence of the treatment variable and the covariates. In randomized empirical experiments the treatment and control groups should be roughly similar (i.e. balanced) in their distributions of covariates.
# Of course, we would expect the conversion rate to be different between the treatment and control group
#library("uplift")
cb <- checkBalance(treatment~.-conversion, data = trainData, report = c("adj.means", "adj.mean.diffs", "p.values", "chisquare.test"))

# Balance properties of first ten covariates 
# Be aware that the results are saved as a tensor or '3D matrix'.
dim(cb$results)
round(cb$results[,,], 2)
# The function automatically computes a chi-squared test for the conditional independence of the covariates to the treatment variable
cb$overall

# The test rejects the null hypothesis that the sample is balanced, i.e. truly randomized, at the 1% level. The reason is possibly that the customer were not assigned randomly to the treatment and control group, but based on some non-random procedure, e.g. an existing model or some decision rule.
# Think deeply about how this will impact your model!



library(causalTree)
tree1 <- causalTree(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel + segment+ visit, data = trainData, treatment = trainData$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = F, split.Bucket = F, 
                   xval = NULL, cp = 0.005, minsize = 20, propensity = 0.5)

#opcp <- tree$cptable[,1][which.min(tree$cptable[,4])]

#opfit <- prune(tree, opcp)

rpart.plot(tree1)
summary(tree1)

str(trainData)
summary(trainData)

upliftRF <- upliftRF(conversion ~ trt(treatment) +recency + history +history_segment + mens + womens + zip_code + newbie + channel + segment+ visit,
                     data = trainData,
                     mtry = 5,
                     ntree = 100,
                     split_method = "KL", 
                     minsplit = 50,
                     verbose = TRUE)
summary(upliftRF)

table(trainData$z_var2)
table(testData$z_var2)




# upliftRF2 <- upliftRF(z_var2 ~ trt(treatment) +. -treatment,
#                      data = trainData,
#                      mtry = 5,
#                      ntree = 100,
#                      split_method = "KL", 
#                      minsplit = 10,
#                      verbose = TRUE)
# summary(upliftRF2)
###### does not work!! probably because the treatment distinction is already included in z_var2, 
###### meaning that no control participants have 0


# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred <- list()
pred_upliftRF <- predict(object = upliftRF, newdata = testData)
# The predictions differentiate between the treatment and control condition
# pr.y1_ct1 gives an estimate for a person to convert when in the treatment group
# pr.y1_ct1 gives an estimate for a person to convert when in the control group
head(pred_upliftRF) 

# Our goal is to identify the people for whom the treatment will lead to a large increase 
# in conversion probability, i.e. where the difference between the treatment prob. and the
# control prob. is positive and high
pred[["upliftRF"]] <- pred_upliftRF[, 1] - pred_upliftRF[, 2]
# We can see that there are indeed customers who are expected to not buy if targeted by our ads (negative difference)
summary(pred[["upliftRF"]])




# TWO MODEL ---------------------------------------------------------------

treatment <- hllstrm[hllstrm$treatment==1,]
control <- hllstrm[hllstrm$treatment==0,]

str(treatment)
str(control)
summary(treatment)
summary(control)

glm_treat <- lm(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel + visit, family = gaussian, data=treatment)
glm_contr <- lm(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel + visit, family = gaussian, data=control)

summary(glm_contr)
summary(glm_treat)


library(rpart)

rpart = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel + visit, data=control)
prp(rpart)
summary(rpart)

rpart2 = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel + visit, data=treatment)
prp(rpart2)
summary(rpart2)

# Causal Tree for REVENUE! -------------------------------------------------------------

names(hllstrm)
tree1 <- causalTree(spend ~ recency + history + mens + womens + zip_code + newbie + channel, data = hllstrm, treatment = hllstrm$treatment,
                   split.Rule = "CT", cv.option = "CT", split.Honest = T, cv.Honest = T, split.Bucket = F, xval = 5, 
                   cp = 0, minsize = 20)





