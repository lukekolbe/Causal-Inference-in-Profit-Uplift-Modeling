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
library(tidyverse)a

# Laoding the Data set -----------------
set.seed(666)
getwd()
setwd("~/Desktop/apa_data")
f_a <- read.csv("FashionA.csv", sep=",")

# Decriptive Analysis ------------------
str(f_a)
table(f_a$controlGroup)

table(f_a$campaignMov, f_a$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
prop.table(table(f_a$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%

with(f_a, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
summary(aov(campaignValue  ~ controlGroup, data=f_a)) 
# there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)

#with(f_a, prop.table(table(converted,controlGroup, campaignValue), margin=1))

table(f_a$checkoutDiscount) #no checkout discounts in the data?!
prop.table(table(f_a$checkoutAmount>0, f_a$controlGroup)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)

# Drop columns with no information
f_a <- f_a[,-which(names(f_a) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TabSwitchPer.product.", "TimeToFirst.cart.","TabSwitchOnLastScreenCount","TotalTabSwitchCount"))] 
summary(f_a)
#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(f_a, anyNA)))
# Check % of NA Columns
colMeans(is.na(f_a))
# Drop the high NA percentage Columns
cols.dont.want=c("TimeSinceOn.search.","TimeSinceOn.sale.","TimeToFirst.search.","TimeToFirst.cart.",
                 "TimeToFirst.sale.","SecondsSinceFirst.search.","SecondsSinceFirst.cart.","SecondsSinceFirst.sale.",
                 "TimeToCartAdd","SecondsSinceTabSwitch") # get rid of these
f_a=f_a[,! names(f_a) %in% cols.dont.want, drop=F]

#Create Dummy Variables from NA Columns
#f_a$HasSessionBefore <- ifelse(f_a$TimeSinceLastVisit == c("NA"), "0", "1")

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0


#mean imputation for low NA percentage Columns
#colMeans(is.na(f_a))
#summary(f_a)
#str(f_a)

for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}

# Drop all Variables with Nulls
#for(i in 1:ncol(f_a)){
# f_a[is.na(f_a[,i]),i] = NULL
#}

# Feature Engineering -------------------------------------------
f_a$treatment = numeric(nrow(f_a))
f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)

library(lubridate)
# create 12-factor variable (months) for seasonality, maybe even seasons (4) out of epochSecond

f_a$month=as.factor(month(as_datetime(f_a$epochSecond)))
table(f_a$month)
str(f_a$month)

# Data set rearrangement 
#f_a <- f_a[,c(4:9,94,63,10:62,64:93,1,2,3)] # sorting new for better visibility of important columns

# Dropping further columns, we do not need anymore
f_a <- f_a[,-which(names(f_a) %in% c("controlGroup", "epochSecond"))] 

# Seperating the different treatments --> Later test with 2-Model-Approach if treatment effects are higher for different treatments
f_a_5 <- f_a[f_a$campaignValue==500,] 
f_a_0 <- f_a[f_a$campaignValue==0,]
f_a <- f_a[f_a$campaignValue==2000,]
# Remove highly correlated (>0.75) variables
f_a <- f_a[,-which(names(f_a) %in% c("HoursSinceFirstVisit","IsMobile","RecencyOfPreviousSessionInHrs",
                                     "SecondsFor.3.","SecondsSinceFirst.overview.","SecondsSinceFirst.product.",
                                     "SecondsSincePrevious","SessionTimeInSeconds","targetViewCount",
                                     "TimeSinceLastVisit","TimeSinceOn.overview.","TimeToFirst.product.",
                                     "TotalClickCount","ViewCount","ViewsOn.overview.",
                                     "ViewsOn.product.","targetViewCount"))]


## Average Treatment Effect (ATE)

experiment <- table(list("Treated" = f_a$treatment, "Converted" = f_a$converted))
experiment

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# which is plausible because of the random sampling
mean(f_a$converted[f_a$treatment==1]) - mean(f_a$converted[f_a$treatment==0]) # 0.005% conversion uplift
# or alternatively:
(experiment[2,2]/sum(experiment[2,]) ) - (experiment[1,2]/sum(experiment[1,]) )

mean(f_a$checkoutAmount[f_a$treatment==1]) - mean(f_a$checkoutAmount[f_a$treatment==0]) # 0.49 euro checkoutAmount uplift


summary(aov_a <- aov(checkoutAmount  ~ treatment, data=f_a)) # the differences in checkout amount are statistically significant!
ttest_a <- t.test(checkoutAmount ~ treatment, data=f_a) # the differences in checkout amount are statistically significant!
ttest_a

table(f_a$checkoutAmount>=105,f_a$treatment) # 105 --> Value to shop to be able to use discount code
#only 15k people qualify for the discountcode

# 1ST ROUND SAMPLE SPLITTING AND STRATIFICATION ---------------------------------------------------

train_indices_f_a <- list()

combinations <- expand.grid(list("converted"=c(0,1), "treatment"= c(0,1))) # treatment is ordered 1,0 compared to hillström data because the variable indicates control group membership
xtabs(~converted+treatment, f_a)
sample_size_f_a <- as.numeric(xtabs(~converted+treatment, f_a))


for(i in 1:4){
  train_indices_f_a[[i]] <- sample(which(f_a$converted == combinations$converted[i] &
                                           f_a$treatment == combinations$treatment[i])
                                   , size = round(0.75*sample_size_f_a[i])) 
} 
trainIndex_f_a <- c(train_indices_f_a, recursive=TRUE)

trainData <- f_a[trainIndex_f_a,] # temporarily the train data is only a small partition!
testData  <- f_a[-trainIndex_f_a,]

#  UPLIFT RF ------------------------------------------------------
# str(trainData_mens)
# str(trainData_womens)
# table(trainData$z_var2)
# table(testData$z_var2)

trainData2=trainData
cols.dont.want=c("campaignMov", "campaignValue","checkoutDiscount", "controlGroup",
                 "epochSecond","label","checkoutAmount","confirmed","aborted","ViewedBefore.cart.", 
                 "TabSwitchPer.product.", "TimeToFirst.cart.", "SecondsSinceFirst.cart.", "SecondsSinceTabSwitch","TabSwitchOnLastScreenCount") # get rid of these
trainData2=trainData2[,! names(trainData2) %in% cols.dont.want, drop=F]

upliftRF_f_a2 <- upliftRF(converted ~ trt(treatment) +. -treatment ,
                          data = trainData2,
                          mtry = 8,
                          ntree = 300,
                          split_method = "KL",
                          minsplit = 50,
                          verbose = TRUE)
summary(upliftRF_f_a2)
### ONLY WORKS WITH BINARY TARGET

# Note that the summary() includes the raw variable importance values. More options are available for function varImportance().
varImportance(upliftRF_f_a2, plotit = FALSE, normalize = TRUE)

# Predictions for fitted Uplift RF model
pred_uplift <- list()
pred_upliftRF_f_a2 <- predict(object = upliftRF_f_a2, newdata = testData)
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




head(pred_mens)



# Uplift Generalized Lai´s Approach ------------------------
# required packages
#install.packages("splitstackshape")
#install.packages("caret")
#install.packages("MLmetrics")
#install.packages("e1071")
library(e1071)
library(MLmetrics)
library(splitstackshape)
library(caret)

# Extra variables:
treatmentVariable = "treatment"
targetVariable = "converted"
#table(f_a[,targetVariable])
#table(f_a[,treatmentVariable])
#targetNegativeClass <-  f_a$converted[f_a$converted == 0]  # Needed later on for factoring the target variable (only needed for certain techniques)
#targetPositiveClass <- f_a$converted[f_a$converted == 1]   # Needed later on for factoring the target variable (only needed for certain techniques)
targetNegativeClass = "NO"
targetPositiveClass = "YES"
predictors <- names(f_a)[(names(f_a) != targetVariable) & (names(f_a) != treatmentVariable)]
str(predictors)
# We check the distribution of both treatment/control and purchase/non-purchase
prop.table(table(f_a[,targetVariable],f_a[,treatmentVariable]))

# Checking the rations are indeed more or less the same 


# Specifying new target variable
targetVariable.Multi = "converted.Multi"
#targetCR = f_a$converted[f_a$treatment == 0 & f_a$converted ==1]
#targetCN = f_a$converted[f_a$treatment == 0 & f_a$converted ==0]
#targetTR = f_a$converted[f_a$treatment == 1 & f_a$converted ==1]
#targetTN = f_a$converted[f_a$treatment == 1 & f_a$converted ==0]
targetCR = "CR"
targetCN = "CN"
targetTR = "TR"
targetTN = "TN"
## Factoring Target variable ##
trainData[,targetVariable] <- factor(trainData[,targetVariable])
levels(trainData[,targetVariable]) <- c(targetNegativeClass, targetPositiveClass)


## Factoring Target variable Multinominal ##
trainData[,targetVariable.Multi] <- NA
levels(trainData[,targetVariable.Multi]) <- c(targetTN, targetTR, targetCN, targetCR)
str(trainData)

trainData[,targetVariable.Multi][trainData[,targetVariable] == "YES"  & trainData[,treatmentVariable] == 1] <- targetTR # Treated responders
trainData[,targetVariable.Multi][trainData[,targetVariable] == "NO"  & trainData[,treatmentVariable] == 1] <- targetTN # Treated non-responders
trainData[,targetVariable.Multi][trainData[,targetVariable] == "YES"  & trainData[,treatmentVariable] == 0] <- targetCR # Control responders
trainData[,targetVariable.Multi][trainData[,targetVariable] == "NO"  & trainData[,treatmentVariable] == 0] <- targetCN # Control non-responders


prop.table(table(trainData[,targetVariable.Multi]))

table(trainData$converted.Multi)

gmbGrid = expand.grid(interaction.depth = c(1,5,9),
                      n.trees = (1:30)*50,
                      shrinkage =0.1,
                      n.minobsinnode = 20)

## setting up traiing schema ##
ctrl = trainControl(method= "cv",
                    number = 5,
                    classProbs = T,
                    summaryFunction=multiClassSummary)
## Training the model 
model = train(trainData[,predictors],
              trainData[,targetVariable.Multi],
              method="gbm",
              trControl = ctrl)

## Factoring Target variable ##
testData$targetVariableFactor <- factor(testData[,targetVariable])
levels(testData[,"targetVariableFactor"]) <- c(targetNegativeClass, targetPositiveClass)

## Factoring Target variable Multinominal ##
testData[,targetVariable.Multi] <- NA
levels(testData[,targetVariable.Multi]) <- c(targetTN, targetTR, targetCN, targetCR)

# We fill in the new targetVariable for the test set.
testData[,targetVariable.Multi][testData[,targetVariable] == "YES" & testData[,treatmentVariable] == 1] <- targetTR # Treated responders
testData[,targetVariable.Multi][testData[,targetVariable] == "NO"  & testData[,treatmentVariable] == 1] <- targetTN # Treated non-responders
testData[,targetVariable.Multi][testData[,targetVariable] == "YES" & testData[,treatmentVariable] == 0] <- targetCR # Control responders
testData[,targetVariable.Multi][testData[,targetVariable] == "NO"  & testData[,treatmentVariable] == 0] <- targetCN # Control non-responders

# We look at the percentages for each class.
print(prop.table(table(df.test[,targetVariable.Multi])))
numeric(0)

# We ust the model to get the probabilities of the obs. for the test set
modelProbs.GLAI <- extractProb(list(model),
                               testX = testData[,predictors],
                               testY = testData[,targetVariable.Multi])

modelProbs.GLAI.Results <- modelProbs.GLAI[modelProbs.GLAI$dataType == "Test",]


#We then group together the predictions in a dataframe. 
# The final uplift prediction is the probability of a person purchasing when receiving 
# a treatment minus the probability of a person purchasing when not receiving a treatment.
predictions.GLAI = data.frame(TR=numeric(nrow(modelProbs.GLAI.Results)),
                              TN=numeric(nrow(modelProbs.GLAI.Results)),
                              CR=numeric(nrow(modelProbs.GLAI.Results)),
                              CN=numeric(nrow(modelProbs.GLAI.Results)))

predictions.GLAI$TR <- modelProbs.GLAI.Results[,targetTR]
predictions.GLAI$TN <- modelProbs.GLAI.Results[,targetTN]
predictions.GLAI$CR <- modelProbs.GLAI.Results[,targetCR]
predictions.GLAI$CN <- modelProbs.GLAI.Results[,targetCN]


# To calculate the uplift we work as followed:
prob.C <- prop.table(table(testData[,treatmentVariable]))[1]
prob.T <- prop.table(table(testData[,treatmentVariable]))[2]

predictions.GLAI$Uplift <- ((predictions.GLAI$TR / prob.T) + (predictions.GLAI$CN / prob.C)) - 
  ((predictions.GLAI$TN / prob.T) + (predictions.GLAI$CR / prob.C))

# We can take a sneak peak at the predictions
head(precicitons.GLAI, n=10)   


#Evaluating the model-----------------------------

## Evaluation in uplift modeling is difficult as we cannot both test a person 
## how he would react when receving the campaign or treatment and how he would react 
## when not receiving the campaign or treatment. Therefore we have to look at it on a more aggregated basis.

## We rank the uplift scores from high to low and add this information to a dataframe.

set.seed(123) # As there is a randomness is involved we set a seed to be able to reproduce results while testing.


mm <- cbind(uplift = predictions.GLAI[,5],
            target = testData[,targetVariable], 
            treatment = testData[,treatmentVariable],
            uplift_rank = rank(-predictions.GLAI[,5], ties.method = "random"))

## Afterwards we divide the observation into 10 equal groups. 
## The first group will contain the highest uplift scores, 
## the second group the second highest-scores and so on.


## There is a possibility of having obs. with the same uplifct score and there is a chance 
## that these will not be part of the same group. If this is the case, the obs. are assigned randomly

groups <- 10
bk <- unique(quantile(mm[, 4], probs = seq(0, 1, 1 / groups)))
if ((length(bk)-1) != groups){
  warning("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly ", groups)
}
mm <- cbind(mm, decile = cut(mm[, 4], breaks = bk, labels = NULL, include.lowest = TRUE))

# Previewing the dataframe
head(mm)


## We have now ranked all the obs. in the test accord. to uplift socre and assignemd them into a group (accord. to their ranking).
## The next step is to test the actual values like per group:
# - How many belonged to the treatment group?
# - How many to the control group?
# - How many of those have purchased?

n.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], sum)  # Sum of people responding and not having received the treatment
n.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], sum)  # Sum of people responding and having received the treatment
r.y1_ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], mean) # Ratio of people responding and not having received the treatment
r.y1_ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], mean) # Ratio of people responding and having received the treatment
n.ct0 <- tapply(mm[mm[, 3] == 0, ][, 2], mm[mm[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
n.ct1 <- tapply(mm[mm[, 3] == 1, ][, 2], mm[mm[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment

# In rare situations the ratio of a group can be non-existing because there are nog people in the treatment or control group.
# We set these to 0.
r.y1_ct0 <- ifelse(is.na(r.y1_ct0), 0, r.y1_ct0)
r.y1_ct1 <- ifelse(is.na(r.y1_ct1), 0, r.y1_ct1)

## We group these statistics into a new dataframe and call it a performance-class

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


# Now that we have the new performance-class we can use it to produce some graphs

# Response Rate per Decile ----------

# The Reponse Rate Per Decile plot is a direct visualisation of the performance bock. 
# In theory the ideal plot is to have high gesponse rates for the treatment group 
# and low response rates in the control group in the first deciles and vice versa in the last deciles.

temp.df.treatment <- data.frame(Decile = seq(1:10), responseRate = perf[,6], Group = "treatment")
temp.df.control <- data.frame(Decile = seq(1:10), responseRate = perf[,7], Group = "control")
temp.df <- rbind(temp.df.treatment, temp.df.control)

require(ggplot2)
require(scales)
ggplot(temp.df, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=responseRate, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=percent, limits=c(0,0.7), name="Response Rate (%)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Response Rate Per Decile - Two Model Approach") + theme(plot.title = element_text(face="bold",hjust = 0.5))

# Uplift per Decile ------------------------

# By substracing the response rates of the treatment groups with the response rates of the control 
# groups we achieve the uplift per decile as seen in the next plot

temp.df.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf[,6] - perf[,7])
require(ggplot2)
require(scales)
ggplot(temp.df.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=percent, limits=c(-0.3,0.3), name="Uplift (Treatment - Control)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Two Model Approach") + theme(plot.title = element_text(face="bold", size=20)) +
  guides(fill=FALSE)

# QINI Plot -------------------------

# One way of reporesenting the performance of an uplift technique as a single number 
# is through the Qini Coefficient and the accompaning Qini Curve.

# The idea is to calculate the incremental gains:

# First the cumulitative sum of the treated and the control groups are calculated
# with respect to the total population in each group at the specified decile.

# Afterwars we calculate the percentage of the total amout of people (both treatment and control)
# present in each decile.

r.cumul.y1_ct1 <- cumsum(perf[,"n.y1_ct1"]) / cumsum(perf[,"n.ct1"])
r.cumul.y1_ct0 <- cumsum(perf[,"n.y1_ct0"]) / cumsum(perf[,"n.ct0"])
deciles <- seq(1 / groups, 1, 1 / groups)

r.cumul.y1_ct1[is.na(r.cumul.y1_ct1)] <- 0
r.cumul.y1_ct0[is.na(r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.

### Model Incremental gains 
inc.gains <- c(0.0, (r.cumul.y1_ct1 - r.cumul.y1_ct0) * deciles)

# The overall incremental gains is basically the overal uplift. The random
# incremental gains is then the overall incremental gains divided by the amout 
# of groups used.

### Overall incremental gains
overall.inc.gains <- sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) - sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"])

### Random incremental gains
random.inc.gains <- c(0, cumsum(rep(overall.inc.gains / groups, groups)))

# Next up we compute the area underneath the incremental curve.

### Compute area under the model incremental gains (uplift) curve 
x <- c(0.0, seq(1 / groups, 1, 1 / groups))
y <- inc.gains

auuc <- 0
auuc.rand <- 0

for (i in 2:length(x)) {
  auuc <- auuc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
}

# We do the same for the are underneath the random incremental curve.
### Compute area under the random incremental gains curve
y.rand <- random.inc.gains

for (i in 2:length(x)) {
  auuc.rand <- auuc.rand + 0.5 * (x[i] - x[i-1]) * (y.rand[i] + y.rand[i-1])
}

# We then compute the difference between those two areas.

### Compute the difference between the areas (Qini coefficient)
Qini <- auuc - auuc.rand
miny <- 100 * min(c(random.inc.gains, inc.gains))
maxy <- 100 * max(c(random.inc.gains, inc.gains))

# The last step is to plot the Qini-curve

plot(inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(miny, maxy))
lines(random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("topright", c("Model", "Random"), 
       col=c("blue", "red"), lty=c(2,1))

