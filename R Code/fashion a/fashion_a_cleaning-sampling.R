source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding") ## "Rounding is set as is reproduces the results gained with prior version of R
### CHECK FOR CORRECT SEED
sample(20)
#SHOULD BE [1]  4 11  2  3 20 18 14 15  8  5  6 13 10 17  7 12 16  1  9 19

getwd()
f_a <- read.csv("working data/Datasets/FashionA.csv", sep=",")

# Feature Engineering -------------------------------------------
f_a$treatment = numeric(nrow(f_a))
f_a$treatment = ifelse(f_a$controlGroup==0, 1, 0)

f_a$eligibility <- ifelse((f_a$checkoutAmount+f_a$campaignValue/100 >= f_a$campaignMov/100&f_a$treatment==1),1,0)

f_a$ExpectedDiscount <- numeric(nrow(f_a))
f_a$ExpectedDiscount[f_a$campaignUnit=="CURRENCY"&f_a$eligibility==1] <- f_a$campaignValue[f_a$campaignUnit=="CURRENCY"&f_a$eligibility==1]/100
      
          
# Decriptive Analysis ------------------

# The ATE is the outcome difference between the groups, assuming that individuals in each group are similar
# (((which is plausible because of the random sampling)))
mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==0])) - mean(as.numeric(trainData_f_a2$converted[trainData_f_a2$controlGroup==1]))
mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==0]) - mean(trainData_f_a2$checkoutAmount[trainData_f_a2$controlGroup==1])

# or alternatively:
experiment <- table(list("Control" = trainData_f_a2$controlGroup, "Converted" = trainData_f_a2$converted))
experiment

(experiment[1,2]/sum(experiment[1,]) ) - (experiment[2,2]/sum(experiment[2,]) )
# str(f_a)
# table(f_a$treatment)
# 
# table(f_a$campaignMov, f_a$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# # Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
# prop.table(table(f_a$campaignValue)) #campaign value mostly 2000 CURRENCY UNITS, except for ~66700 or 6%
# 
# with(f_a, prop.table(table(campaignValue,treatment), margin=1)) # proportions of control/treatment groups seem consistent across treatments
# summary(aov(campaignValue  ~ treatment, data=f_a)) 
# # there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)
# 
# #with(f_a, prop.table(table(converted,treatment, campaignValue), margin=1))
# 
# table(f_a$checkoutDiscount) #no checkout discounts in the data?!
# prop.table(table(f_a$checkoutAmount>0, f_a$treatment)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)
# 
# 
# treatment_uplift_a <- aggregate(checkoutAmount ~ treatment, data=f_a, mean)[1,2] - aggregate(checkoutAmount ~ treatment, data=f_a, mean)[2,2]
# treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
# summary(aov(checkoutAmount  ~ treatment, data=f_a)) # the differences in checkout amount are statistically significant!
# t.test(checkoutAmount ~ treatment, data=f_a) # the differences in checkout amount are statistically significant!
# 
# table(f_a$campaignMov)
# 
# table(f_a$checkoutAmount>=105,f_a$treatment)#14.800 people qualified for actually using the discount of 20€ through achieving the minimum order value
# with(f_a, prop.table(table(checkoutAmount>=105,treatment), margin=2)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do

# Drop columns with no information----------------------------------------------------------------

f_a <- f_a[f_a$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!
f_a <- f_a[,-which(names(f_a) %in% c("campaignTags","trackerKey","campaignId","checkoutDiscount", "controlGroup","label"))]
          
          
#NA Columns ---------------------------------------------------
# Identify NA Columns
names(which(sapply(f_a, anyNA)))

# Check % of NA Columns
colMeans(is.na(f_a))

#results in https://docs.google.com/document/d/1EWbMd2HmSLIcBgpGPeH02hArmZ1uKFRsuiTFGLJNdcs/edit?usp=drive_web&ouid=105611495550068087134

# # Drop the high NA percentage Columns (SPECIFIC FOR FASHION A & B)
cols.dont.want=c("TimeSinceLastConversion","TimeSinceOn.search.","TimeToFirst.search.","TimeToFirst.cart.",
                 "SecondsSinceFirst.search.","SecondsSinceFirst.cart.",# get rid of these because the NA ratio in f_a & f_b is >0.8
                 "TotalTabSwitchCount", "TabSwitchOnLastScreenCount", "TabSwitchPer.product.", #only 0
                 "ViewedBefore.cart.", #only 1 with value higher 0
                 "SecondsSinceTabSwitch" #only NA
                 ) # these columns only carry 0s

f_a=f_a[,! names(f_a) %in% cols.dont.want, drop=F]

# Setting specific Column Null Values to 0:
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_a[, varlist][is.na(f_a[,varlist])] = 0

f_a.campaignUnit <- data.frame(f_a$campaignUnit)
names(f_a.campaignUnit) <- "campaignUnit"
f_a <- f_a[,-which(names(f_a)%in%"campaignUnit")]
for(i in 1:ncol(f_a)){
  f_a[is.na(f_a[,i]), i] <- median(f_a[,i], na.rm = TRUE)
}

f_a <- cbind(f_a, f_a.campaignUnit)
colMeans(is.na(f_a))

# correlation test and removal of highly correlated variables ------------------
#identifying pairs of highly correlated variables and removing one of each pair
correlationMatrix <- cor(f_a[,-which(names(f_a) %in% c("converted", "treatment","checkoutAmount", "eligibility", "ExpectedDiscount", "aborted", "confirmed", "campaignMov", "campaignValue", "campaignUnit"))]) #build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE, exact=TRUE)

f_a <- f_a[,-which(names(f_a) %in% c(highlyCorrelated))]

# Uplift NIV --------------------------------------------------------------

exclude.vars <- c("converted","checkoutAmount","treatment",
                  "eligibility", "ExpectedDiscount", "aborted", 
                  "confirmed", "campaignMov", "campaignValue", "campaignUnit")

n <- names(f_a)
f_niv_fa <- as.formula(paste("converted ~", paste("trt(treatment) +"),
                             paste(n[!n %in% exclude.vars], collapse = " + ")))

fa_niv <- niv(f_niv_fa, f_a, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
              nbins = 10, continuous = 4, plotit = TRUE)

f_a_niv <- fa_niv$niv

# final selection after comparing importances -----------------------------
#picking top25 from NIV process
# leaving out aborted and confirmed for suspicion of target leakage

f_a <- f_a[,which(names(f_a) %in% c("checkoutAmount", "converted", "treatment",
                                    "TimeToFirst.overview.",
                                    "TimeSpentOn.overview.",
                                    "SecondsSinceFirst.sale.",
                                    "PageIs.overview.",
                                    "TotalClickCount",
                                    "VisitorKnown",
                                    "DayOfWeek",
                                    "TimeSinceLastVisit",
                                    "SecondsSincePrevious",
                                    "ViewCountLastVisit",
                                    "DurationLastVisitInSeconds",
                                    "PreviousVisitCount",
                                    "HasConfirmedBefore",
                                    "TimeToFirst.sale.",
                                    "WasConvertedBefore",
                                    "TimeSinceOn.sale.",
                                    "SecondsSinceClick",
                                    "HourOfDay",
                                    "FrequencyOfPreviousSessions",
                                    "ViewCount",
                                    "epochSecond",
                                    "TimeSpentOn.product.",
                                    "ScreenWidth",
                                    "TimeSinceOn.product.",
                                    "NumberOfDifferent.overview.",
                                    "eligibility", "ExpectedDiscount", 
                                    "campaignMov",
                                    "campaignUnit",
                                    "campaignValue"
                                    ))]


#  Splitting into train & test set ----------------------------------------
set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

strat_split <- stratified(f_a, c("treatment", "converted"), 0.667, bothSets=TRUE)
f_a.train <- as.data.frame(strat_split[[1]])
f_a.validate <- as.data.frame(strat_split[[2]])

strat_validate_split <- stratified(f_a.validate, c("treatment", "converted"), 0.7, bothSets=TRUE)
f_a.test <- as.data.frame(strat_validate_split[[2]])

# second part of stratification (TRAINING DATA) -------------------------------------------
strat_trainsplit_small <- stratified(f_a.train, c("treatment", "converted"), 0.85, bothSets=TRUE)
#f_a.train_discard <- as.data.frame(strat_trainsplit_small[[1]]) # we cannot use this data, too many rows, to expensive to compute.
f_a.train_small <- as.data.frame(strat_trainsplit_small[[2]])

 # third part of stratification (ESTIMATION DATA) ONLY NEEDED FOR HONEST.CAUSALTREE-------------------------------------------
f_a.train_large <- as.data.frame(strat_trainsplit_small[[1]])
strat_estimationsplit <- stratified(f_a.train_large, c("treatment", "converted"), 0.85, bothSets=TRUE)

f_a.estimation <- as.data.frame(strat_estimationsplit[[2]])

