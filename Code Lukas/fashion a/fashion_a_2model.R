
# TWO MODEL APPROACH (REGRESSION) ---------------------------------------------------------------


train_index <- createDataPartition(f_a$treatment)

train_treat_index <- sample(which(trainData$treatment==1), size = 150000, replace=FALSE) 
train_control_index <- sample(which(trainData$treatment==0), size = 150000, replace=FALSE) 


#create data without NA
glm_train_noNA <- trainData[,apply(trainData, 2, anyNA)==FALSE] #draw all columns from train data that have no NAs
glm_test_noNA <- testData[,apply(testData, 2, anyNA)==FALSE] #draw all columns from train data that have no NAs


train_noNA_treat <- glm_train_noNA[train_treat_index,]
train_noNA_control <- glm_train_noNA[train_control_index,]

table(train_noNA_treat$treatment)
table(train_noNA_control$treatment)

n2 <- names(train_noNA_treat)
f2 <- as.formula(paste("checkoutAmount ~", paste(n2[!n2 %in% c("campaignMov", "campaignValue",
                                                               "checkoutDiscount","controlGroup",
                                                               "treatment","converted",
                                                               "checkoutAmount","epochSecond",
                                                               "label", # all above: target variables, no information
                                                               "ViewedBefore.cart.",  #no data
                                                               "TabSwitchPer.product.", #no data
                                                               #"TimeToFirst.cart.", 
                                                               #"SecondsSinceFirst.cart.", 
                                                               #"SecondsSinceTabSwitch",
                                                               "TabSwitchOnLastScreenCount", #no data
                                                               
                                                               "ViewCount",
                                                               "VisitorKnown",
                                                               "TotalTabSwitchCount", #no data
                                                               "aborted")], #last four features were dropped for the glm model because they produce NA coefficients (collinearity)
                                                 collapse = " + ")))


f2



glm_f_a_treat <- glm(f2,family = gaussian, data=train_noNA_treat)
glm_f_a_contr <- glm(f2, family = gaussian, data=train_noNA_control)




summary(glm_f_a_treat)
summary(glm_f_a_contr)

length(glm_f_a_treat$coefficients) > glm_f_a_treat$rank
length(glm_f_a_contr$coefficients) > glm_f_a_contr$rank

prediction_2model_treat <- predict(glm_f_a_treat, glm_test_noNA)
prediction_2model_contr <- predict(glm_f_a_contr, glm_test_noNA)

aggregate(checkoutAmount ~ treatment, data=glm_test_noNA, mean)
aggregate(checkoutAmount ~ treatment, data=glm_test_noNA, mean)


summary(prediction_2model_treat)
summary(prediction_2model_contr)


# TWO MODEL APPROACH (DECISION TREES) ---------------------------------------------------------------

library(rpart)
rpart_contr = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=control, cp=0.0017, xval=10, model=TRUE)
prp(rpart)
summary(rpart)

rpart_men = rpart(spend~recency + history +history_segment + mens + womens + zip_code + newbie + channel, data=trainData_mens[trainData_mens$treatment==1,], cp=0.0017, xval=10, model=TRUE)
prp(rpart_men)
summary(rpart_men)

