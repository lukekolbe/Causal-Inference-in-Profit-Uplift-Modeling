source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

getwd()
b_t <- read.csv("working data/Datasets/BooksAndToys.csv", sep=",")

# Feature Engineering -------------------------------------------
b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)


b_t$eligibility <- numeric(nrow(b_t))
b_t$eligibility[b_t$campaignUnit=="CURRENCY"] <- ifelse((b_t$checkoutAmount[b_t$campaignUnit=="CURRENCY"]+b_t$campaignValue[b_t$campaignUnit=="CURRENCY"]/100 >= b_t$campaignMov[b_t$campaignUnit=="CURRENCY"]/100&b_t$treatment[b_t$campaignUnit=="CURRENCY"]==1),1,0)
b_t$eligibility[b_t$campaignUnit=="PERCENT"] <- ifelse((b_t$treatment[b_t$campaignUnit=="PERCENT"]==1),1,0)  ### no ifelse needed, could be just "1"

# STREAMLINE DATA ROWS----------------------------------------------------------------

# merging fragmented rows from same campaignValue category
b_t$campaignValue[b_t$campaignValue==15] <- 1500

# deleting rows that are unusual
b_t <- b_t[-which(b_t$campaignUnit=="CURRENCY"  &b_t$campaignValue==0),] # zero € discount does not make sense in our case
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==0),] # only 21 rows
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==1000),] # cases with 10 percent discount were the only ones with MOV>0 (3616 rows)
b_t <- b_t[-which(b_t$campaignUnit=="PERCENT"   &b_t$campaignValue==1600),] # only 1 row

# creating another variable for the cost matrix: Expected Discount
# this is based on the assumption that everyone who is eligible for a discount also uses said discount

b_t$ExpectedDiscount <- numeric(nrow(b_t))
b_t$ExpectedDiscount[b_t$campaignUnit=="CURRENCY"&b_t$eligibility==1] <- b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$eligibility==1]/100
b_t$ExpectedDiscount[b_t$campaignUnit=="PERCENT"&b_t$eligibility==1] <- (b_t$checkoutAmount[b_t$campaignUnit=="PERCENT"&b_t$eligibility==1]/(1-b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$eligibility==1]/10000))-b_t$checkoutAmount[b_t$campaignUnit=="PERCENT"&b_t$eligibility==1]


# Decriptive Analysis ------------------

# Average Treatment Effect (ATE)
# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==0])) - mean(as.numeric(trainData_b_t2$converted[trainData_b_t2$controlGroup==1]))
mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==0]) - mean(trainData_b_t2$checkoutAmount[trainData_b_t2$controlGroup==1])

# or alternatively:
experiment <- table(list("Control" = trainData_b_t2$controlGroup, "Converted" = trainData_b_t2$converted))
experiment

(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )


# str(b_t)
# table(b_t$controlGroup)
# 
# table(b_t$campaignMov, b_t$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# 
# table(b_t$campaignUnit, b_t$campaignValue)
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"],
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0], 
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$eligibility[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$eligibility[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0],
#       b_t$treatment[b_t$campaignUnit=="PERCENT"&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==1&b_t$checkoutAmount>0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="PERCENT"&b_t$treatment==0],
#       b_t$campaignValue[b_t$campaignUnit=="PERCENT"&b_t$treatment==0])
# 
# table(b_t$campaignMov[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1],
#       b_t$campaignValue[b_t$campaignUnit=="CURRENCY"&b_t$treatment==1])
# 
# prop.table(table(b_t$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%
# 
# with(b_t, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
# summary(aov(campaignValue  ~ controlGroup, data=b_t)) 
# # there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)
# 
# #with(b_t, prop.table(table(converted,controlGroup, campaignValue), margin=1))
# 
# table(b_t$checkoutDiscount) #no checkout discounts in the data?!
# prop.table(table(b_t$checkoutAmount>0, b_t$treatment)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)
# 
# 
# aggregate(checkoutAmount ~ treatment, data=b_t, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=b_t, mean)[1,2]
# treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
# summary(aov(checkoutAmount  ~ controlGroup, data=b_t)) # the differences in checkout amount are statistically significant!
# t.test(checkoutAmount ~ controlGroup, data=b_t) # the differences in checkout amount are statistically significant!
# 
# names(b_t)
# 
# table(b_t$campaignMov)
# 
# table(b_t$checkoutAmount>=105,b_t$controlGroup)#14.800 people qualified for actually using the discount of 20â¬ through achieving the minimum order value
# with(b_t, prop.table(table(checkoutAmount>=105,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do


# Drop columns with no information----------------------------------------------------------------
b_t <- b_t[,-which(names(b_t) %in% c("campaignTags","trackerKey","campaignId","checkoutDiscount", "controlGroup","label"))]

#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(b_t, anyNA)))

# Check % of NA Columns
colMeans(is.na(b_t))
#Drop the high NA percentage Columns

cols.dont.want=c("TimeSinceLastConversion","TimeSinceOn.sale.",
                 "TimeToFirst.cart.", "SecondsSinceFirst.sale.","TimeToCartAdd",
                 "TimeToFirst.sale.","SecondsSinceFirst.cart.","SecondsSinceTabSwitch", #only NA
                 "ViewedBefore.cart.","TabSwitchPer.product.","TabSwitchOnLastScreenCount","TotalTabSwitchCount") # these columns only carry 0s


b_t=b_t[,! names(b_t) %in% cols.dont.want, drop=F]
#Setting specific Column Null Values to 0, works for specificly defined columns
b_t$InitCartNonEmpty <- ifelse(b_t$InitCartNonEmpty == c("NA"), "0", b_t$InitCartNonEmpty)

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0

b_t.campaignUnit <- data.frame(b_t$campaignUnit)
names(b_t.campaignUnit) <- "campaignUnit"
b_t <- b_t[,-which(names(b_t)%in%"campaignUnit")]

for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}

b_t <- cbind(b_t, b_t.campaignUnit)
str(b_t)


# correlation test and removal of highly correlated variables ------------------
# find and reduce attributes that are highly corrected (ideally >0.75)
correlationMatrix <- cor(b_t[,-which(names(b_t) %in% c("converted", "treatment","checkoutAmount","eligibility", "aborted", "confirmed", "ExpectedDiscount","campaignMov", "campaignValue", "campaignUnit"))]) 
#build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

b_t <- b_t[,-which(names(b_t) %in% c(highlyCorrelated))]


# Uplift NIV --------------------------------------------------------------
exclude.vars <- c("converted","checkoutAmount","treatment",
                  "eligibility", "ExpectedDiscount", "aborted", 
                  "confirmed", "campaignMov", "campaignValue", "campaignUnit")
n <- names(b_t)
f_niv_bt <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% exclude.vars], collapse = " + ")))

bt_niv <- niv(f_niv_bt, b_t, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
              nbins = 10, continuous = 4, plotit = TRUE)

b_t_niv <- bt_niv$niv

# final selection after comparing importances -----------------------------
### this makes any code below obsolete when creating samples for actual model building
#picking top25 from NIV process

b_t <- b_t[,which(names(b_t) %in% c("checkoutAmount", "converted", "treatment",
                                    "ScreenWidth",
                                    "TimeSpentOn.product.",
                                    "epochSecond",
                                    "DayOfWeek",
                                    "TimeSinceOn.product.",
                                    "TimeToFirst.overview.",
                                    "HourOfDay",
                                    "SecondsSinceClick",
                                    "TimeSpentOn.search.",
                                    "SecondsFor.3.",
                                    "TimeToFirst.search.",
                                    "InitPageWas.home.",
                                    "ViewCountLastVisit",
                                    "TimeSinceOn.overview.",
                                    "VisitorKnown",
                                    "TimeToFirst.product.",
                                    "DurationLastVisitInSeconds",
                                    "RecencyOfPreviousSessionInHrs",
                                    "TimeSpentOn.overview.",
                                    "TotalClickCount",
                                    "TriggerEventsSinceLastOnThisPage",
                                    "ViewsOn.search.",
                                    "PreviousVisitCount",
                                    "TimeSinceOn.search.",
                                    "ClicksPer.product.",
                                    "eligibility",
                                    "ExpectedDiscount",
                                    "campaignMov",
                                    "campaignUnit",
                                    "campaignValue"
                                    ))]


#  Splitting into train & test set ----------------------------------------
set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

strat_split <- stratified(b_t, c("treatment", "converted"), 0.667, bothSets=TRUE)
b_t.train <- as.data.frame(strat_split[[1]])
b_t.test <- as.data.frame(strat_split[[2]])

#  ALTERNATIVE Splitting into train FOR WHEN ESTIMATION SET NEEDED ----------------------------------------

strat_split <- stratified(b_t, c("treatment", "converted"), 0.75, bothSets=TRUE)
b_t.train_alt <- as.data.frame(strat_split[[1]])
b_t.test_alt <- as.data.frame(strat_split[[2]])

strat_split <- stratified(b_t.train_alt, c("treatment", "converted"), 0.7, bothSets=TRUE)
b_t.train_alt <- as.data.frame(strat_split[[1]])
b_t.estimate <- as.data.frame(strat_split[[2]])

