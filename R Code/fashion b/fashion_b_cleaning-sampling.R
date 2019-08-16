source("R Code/misc code/install-packages.R")
source("R Code/misc code/load-packages.R")

set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

getwd()
f_b <- read.csv("working data/Datasets/FashionB.csv", sep=",")

# Feature Engineering -------------------------------------------
f_b$treatment = numeric(nrow(f_b))
f_b$treatment = ifelse(f_b$controlGroup==0, 1, 0)

f_b$eligibility <- ifelse(((f_b$checkoutAmount+f_b$campaignValue/100 >= f_b$campaignMov/100)&f_b$treatment==1),1,0)

f_b$ExpectedDiscount <- numeric(nrow(f_b))
f_b$ExpectedDiscount[f_b$campaignUnit=="CURRENCY"&f_b$eligibility==1] <- f_b$campaignValue[f_b$campaignUnit=="CURRENCY"&f_b$eligibility==1]/100

# Decriptive Analysis ------------------
# Average Treatment Effect (ATE) 
#aggregate(checkoutAmount ~ treatment, data=f_b, mean)[2,2] - aggregate(checkoutAmount ~ treatment, data=f_b, mean)[1,2] 

# str(f_b)
# table(f_b$controlGroup)
# 
# table(f_b$campaignMov, f_b$campaignValue) # minimum order value is different depending on campaignValue (but consistent within value-segments)
# # Idee: uplift (5 Euro Gutschein vs 20 Euro Gutschein ?)
# prop.table(table(f_b$campaignValue)) #campaign value mostly 500 CURRENCY UNITS, except for ~66700 or 6%
# prop.table(table(f_b$campaignMov)) #mov mostly 5000 currency units
# prop.table(table(f_b$campaignUnit)) #only currency units
# 
# with(f_b, prop.table(table(campaignValue,controlGroup), margin=1)) # proportions of control/treatment groups seem consistent across treatments
# summary(aov(campaignValue  ~ controlGroup, data=f_b)) 
# # there are three campaignValues with very different segment size, but they show no difference in treatment/control population >> deleting some data (campaignValue other than "2000" does not shift the distribution)
# 
# #with(f_b, prop.table(table(converted,controlGroup, campaignValue), margin=1))
# 
# table(f_b$checkoutDiscount) #no checkout discounts in the data?!
# prop.table(table(f_b$checkoutAmount>0, f_b$controlGroup)) #~5% have a positive checkout amount (3.8% treatment, 1.2% control)
# 
# 
# treatment_uplift_a <- aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[1,2] - aggregate(checkoutAmount ~ controlGroup, data=f_b, mean)[2,2]
# treatment_uplift_a #the treatment gives an average uplift across the whole population of 0.4892036
# summary(aov(checkoutAmount  ~ controlGroup, data=f_b)) # the differences in checkout amount are statistically significant!
# t.test(checkoutAmount ~ controlGroup, data=f_b) # the differences in checkout amount are statistically significant!
# 
# names(f_b)
# 
# table(f_b$campaignMov)
# 
# table(f_b$checkoutAmount>=105,f_b$controlGroup)#14.800 people qualified for actually using the discount of 20â¬ through achieving the minimum order value
# with(f_b, prop.table(table(checkoutAmount>=105,controlGroup), margin=1)) # 25% of the treatment group have a checkout amount >=105 and 22.8% of the control group do

# Drop columns with no information----------------------------------------------------------------
f_b <- f_b[,-which(names(f_b) %in% c("campaignTags","trackerKey","campaignId","checkoutDiscount", "controlGroup","label"))] #"campaignUnit" "campaignMov", "campaignValue"


#NA Columns ---------------------------------------------------
# # Identify NA Columns
names(which(sapply(f_b, anyNA)))

# # Check % of NA Columns
colMeans(is.na(f_b))

#results in https://docs.google.com/document/d/1EWbMd2HmSLIcBgpGPeH02hArmZ1uKFRsuiTFGLJNdcs/edit?usp=drive_web&ouid=105611495550068087134

# # Drop the high NA percentage Columns (SPECIFIC FOR FASHION A & B)
cols.dont.want=c("TimeSinceLastConversion","TimeSinceOn.search.","TimeToFirst.search.","TimeToFirst.cart.",
                 "SecondsSinceFirst.search.","SecondsSinceFirst.cart.",# get rid of these because the NA ratio in f_a & f_b is >0.8
                 "TotalTabSwitchCount", "TabSwitchOnLastScreenCount", "TabSwitchPer.product.", #only 0
                 "ViewedBefore.cart.", #only 1 with value higher 0
                 "SecondsSinceTabSwitch" #only NA
) 

f_b=f_b[,! names(f_b) %in% cols.dont.want, drop=F]

# Setting specific Column Null Values to 0 :
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
f_b[,varlist][is.na(f_b[,varlist])] = 0

f_b.campaignUnit <- data.frame(f_b$campaignUnit)
names(f_b.campaignUnit) <- "campaignUnit"
f_b <- f_b[,-which(names(f_b)%in%"campaignUnit")]
for(i in 1:ncol(f_b)){
  f_b[is.na(f_b[,i]), i] <- median(f_b[,i], na.rm = TRUE)
}
f_b <- cbind(f_b, f_b.campaignUnit)

# correlation test and removal of highly correlated variables ------------------

# find and reduce attributes that are highly corrected (ideally >0.75)
correlationMatrix <- cor(f_b[,-which(names(f_b) %in% c("converted", "treatment","aborted", "confirmed","checkoutAmount","eligibility", "ExpectedDiscount", "campaignMov", "campaignValue", "campaignUnit"))]) 
#build a correlation matrix without necessary variables (otherwise the method will kick "treatment)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

f_b <- f_b[,-which(names(f_b) %in% c(highlyCorrelated))]

# Uplift NIV --------------------------------------------------------------

n <- names(f_b)
f_niv_fb <- as.formula(paste("converted ~", paste("trt(treatment) +"),paste(n[!n %in% c("converted","checkoutAmount","treatment","eligibility", "ExpectedDiscount")], collapse = " + ")))

fb_niv <- niv(f_niv_fb, f_b, subset=NULL, na.action = na.pass, B = 10, direction = 1, 
              nbins = 10, continuous = 4, plotit = TRUE)

f_b_niv <- fb_niv$niv

# final selection after comparing importances -----------------------------
### this makes any code below obsolete when creating samples for actual model building
#picking top25 from NIV process

f_b <- f_b[,which(names(f_b) %in% c("checkoutAmount", "converted", "treatment",
                                    "DurationLastVisitInSeconds",
                                    "VisitorKnown",
                                    "SecondsSincePrevious",
                                    "PreviousVisitCount",
                                    "ViewCountLastVisit",
                                    "TimeToFirst.product.",
                                    "HoursSinceFirstVisit",
                                    "RecencyOfPreviousSessionInHrs",
                                    "FrequencyOfPreviousSessions",
                                    "epochSecond",
                                    "InitPageWas.home.",
                                    "SecondsSinceClick",
                                    "ScreenWidth",
                                    "TimeSpentOn.product.",
                                    "TimeSpentOn.overview.",
                                    "TimeToFirst.overview.",
                                    "DayOfWeek",
                                    "ViewsOn.overview.",
                                    "TimeSinceOn.product.",
                                    "ClicksPer.product.",
                                    "HourOfDay",
                                    "WasConvertedBefore",
                                    "SameAsPreviousPrevious",
                                    "NumberOfDifferent.overview.",
                                    "NumberOfDifferent.product.",
                                    "eligibility", "ExpectedDiscount", # keeping these for evaluation
                                    "campaignMov",
                                    "campaignUnit",
                                    "campaignValue"
                                    ))]


#  Splitting into train & test set ----------------------------------------
set.seed(101010, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rounding")

strat_split <- stratified(f_b, c("treatment", "converted"), 0.667, bothSets=TRUE)
f_b.train <- as.data.frame(strat_split[[1]])
f_b.validate <- as.data.frame(strat_split[[2]])

strat_validate_split <- stratified(f_b.validate, c("treatment", "converted"), 0.5, bothSets=TRUE)
f_b.test <- as.data.frame(strat_validate_split[[2]])

# second part of stratification -------------------------------------------
strat_trainsplit_small <- stratified(f_b.train, c("treatment", "converted"), 0.23, bothSets=TRUE)
f_b.train_small <- as.data.frame(strat_trainsplit_small[[1]])

# third part of stratification (ESTIMATION DATA) ONLY NEEDED FOR HONEST.CAUSALTREE-------------------------------------------
f_b.train_large <- as.data.frame(strat_trainsplit_small[[2]])
strat_estimationsplit <- stratified(f_b.train_large, c("treatment", "converted"), 0.73, bothSets=TRUE)

f_b.estimation <- as.data.frame(strat_estimationsplit[[2]])
