
set.seed(101010)

getwd()
hllstrm <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillström Data/hillstrm.csv", sep=",")
hllstrm <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/Hillström Data/hillstrm.csv", sep=",")

hllstrm <- read.csv("H:\\Applied Predictive Analytics\\Data\\hillstrm.csv", sep=",")
setwd("~/Desktop/apa_data")
hllstrm = read.csv("Hillstrom.csv", sep=",")

#for use with SMOTE DATA
h_s.train <- read.csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/SMOTE/h_s.train_SMOTE.csv")
h_s.train <- read.csv2("H:\\Applied Predictive Analytics\\Data\\SMOTE\\h_s.train_SMOTE.csv")
h_s.train[,1]<-NULL


str(hllstrm)
str(h_s.train)

summary(hllstrm$segment)
summary(hllstrm$spend)
table(hllstrm$spend>0)
table(hllstrm$segment)

hllstrm$treatment <- integer(length=nrow(hllstrm))
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


# REMOVING UNNECESSARY COLUMNS --------------------------------------------

hllstrm <- hllstrm[,-c(2,6,8,9)] #channel & zip_code not needed after dummification; history_segment redundant; segment replaced by "treatment"

### ONLY NECESSARY BEFORE SMOTE
# hllstrm[,c(3,4,5,6,7,9:15)] <- apply(hllstrm[,c(3,4,5,6,7,9:15)],2, as.integer)
# str(hllstrm)
# 
# for(i in c(3,4,5,6,7,9:15)){
#   hllstrm[,i] <- as.factor(hllstrm[,i])
# }
# 


# stratification ----------------------------------------------------------

train_indices_all <- list()

combinations <- expand.grid(list("Conversion"=c(0,1), "Treatment"= c(0,1)))
sample_size_all <- as.numeric(xtabs(~conversion+treatment, hllstrm))

for(i in 1:4){
  train_indices_all[[i]] <- sample(which(hllstrm$conversion == combinations$Conversion[i] &
                                           hllstrm$treatment == combinations$Treatment[i])
                                   , size = round(0.25*sample_size_all[i]), replace=FALSE) 
} 


trainIndex_all <- c(train_indices_all, recursive=TRUE)

h_s.train <- hllstrm[-trainIndex_all,]
h_s.test  <- hllstrm[trainIndex_all,]

table(h_s.train$spend>0, h_s.train$segment)

str(h_s.train)
str(h_s.test)

summary(hllstrm[,c("conversion","treatment")])
summary(h_s.test[,c("conversion","treatment")])
summary(h_s.train[,c("conversion","treatment")])




# EVALUATION ------------------------------------------------------------

# Evaluation CAUSAL FOREST HILLSTROM -----------------------------------
# cf_hillstrom_preds <- predict(object = cf_hillstrom,
#                               newdata=testData_all[, -c(2,6,8,9,11,12,13)],
#                               estimate.variance = TRUE)
# cf_hillstrom_preds = cf_hillstrom_preds[,1]
# cf_hillstrom_preds

cf_hillstrom_preds = read.csv2("/Users/asmir/hu_apa/APA_predictions/cf_preds/cf_h_s_preds_new.csv")
head(cf_hillstrom_preds)
cf_hillstrom_preds = cf_hillstrom_preds[,2]
cf_hillstrom_preds
#table(cf_hillstrom_preds != 0)
# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_cf = cbind(uplift = cf_hillstrom_preds,
              target = h_s.test$spend,
              treatment = h_s.test$treatment,
              uplift_rank = length(cf_hillstrom_preds) +1 - rank(cf_hillstrom_preds, ties.method = "random"))

head(mm_cf)
#View(mm_cf)
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
View(mm_cf)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

cf_n.y1_ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#cf_n.y1_ct0
cf_n.y1_ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#cf_n.y1_ct1
cf_r.y1_ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#cf_r.y1_ct0
cf_r.y1_ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#cf_r.y1_ct1
cf_n.ct0 <- tapply(mm_cf[mm_cf[, 3] == 0, ][, 2], mm_cf[mm_cf[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#cf_n.ct0
cf_n.ct1 <- tapply(mm_cf[mm_cf[, 3] == 1, ][, 2], mm_cf[mm_cf[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#cf_n.ct1
cf_n.y1_ct0
#cf_n.y1_ct1
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
temp.df_cf.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf_cf[,6], Group = "treatment")
temp.df_cf.control <- data.frame(Decile = seq(1:10), averageRevenue = perf_cf[,7], Group = "control")
temp.df_cf <- rbind(temp.df_cf.treatment, temp.df_cf.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df_cf, aes(x=Decile)) +
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
  ggtitle("Average Revenue Per Decile - Causal Honest Forest") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.df_cf.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf_cf[,6] - perf_cf[,7])
require(ggplot2)
require(scales)
ggplot(temp.df_cf.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(0,3), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Causal Honest Forest") + theme(plot.title = element_text(face="bold", size=20)) +
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
cumsum(perf_cf[,"n.y1_ct1"])

cf_r.cumul.y1_ct1
cf_r.cumul.y1_ct0 <- cumsum(perf_cf[,"n.y1_ct0"]) / cumsum(perf_cf[,"n.ct0"])
cf_r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

cf_r.cumul.y1_ct1[is.na(cf_r.cumul.y1_ct1)] <- 0
cf_r.cumul.y1_ct0[is.na(cf_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
cf_inc.gains = c(0.0,(cf_r.cumul.y1_ct1 - cf_r.cumul.y1_ct0) * deciles)
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

#cf_y[2] + cf_y[1]

for (i in 2:length(cf_x)) {
  cf_auuc <-  cf_auuc + 0.5 * (cf_x[i] - cf_x[i-1]) * (cf_y[i] + cf_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
cf_y.rand <- cf_random.inc.gains

for (i in 2:length(cf_x)) {
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
legend("bottomright", c("Causal Honest Forest", "Random"), 
       col=c("blue", "red"), lty=c(2,1))




# Evaluation CAUSAL BOOSTING HILLSTROM -----------------------------------------
cb_hillstrom_preds = read.csv("/Users/asmir/hu_apa/APA_predictions/cb_h_s_preds.csv")
head(cb_hillstrom_preds)
cb_hillstrom_preds = cb_hillstrom_preds[,2]
cb_hillstrom_preds
# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_cb = cbind(uplift = cb_hillstrom_preds,
              target = h_s.test$spend,
              treatment = h_s.test$treatment,
              uplift_rank = length(cb_hillstrom_preds) +1 - rank(cb_hillstrom_preds, ties.method = "random"))

head(mm_cb)
View(mm_cb)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bk_cb = unique(quantile(mm_cb[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bk_cb)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mm_cb = cbind(mm_cb, decile = cut(mm_cb[,4], breaks = bk_cb, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mm_cb)
View(mm_cb)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

cb_n.y1_ct0 <- tapply(mm_cb[mm_cb[, 3] == 0, ][, 2], mm_cb[mm_cb[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#cb_n.y1_ct0
cb_n.y1_ct1 <- tapply(mm_cb[mm_cb[, 3] == 1, ][, 2], mm_cb[mm_cb[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#cb_n.y1_ct1
cb_r.y1_ct0 <- tapply(mm_cb[mm_cb[, 3] == 0, ][, 2], mm_cb[mm_cb[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#cb_r.y1_ct0
cb_r.y1_ct1 <- tapply(mm_cb[mm_cb[, 3] == 1, ][, 2], mm_cb[mm_cb[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#cb_r.y1_ct1
cb_n.ct0 <- tapply(mm_cb[mm_cb[, 3] == 0, ][, 2], mm_cb[mm_cb[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#cb_n.ct0
cb_n.ct1 <- tapply(mm_cb[mm_cb[, 3] == 1, ][, 2], mm_cb[mm_cb[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#cb_n.ct1
#cb_n.y1_ct0
#cb_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
cb_r.y1_ct0 <- ifelse(is.na(cb_r.y1_ct0), 0, cb_r.y1_ct0)
cb_r.y1_ct1 <- ifelse(is.na(cb_r.y1_ct1), 0, cb_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
df_cb <- merge(cbind(cb_n.y1_ct0, cb_r.y1_ct0, cb_n.ct0), cbind(cb_n.y1_ct1, cb_r.y1_ct1, cb_n.ct1), by= "row.names", all = TRUE)             

df_cb$Row.names <- as.numeric(df_cb$Row.names)
df_cb[, c(2, 4, 5, 7)][is.na(df_cb[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

df_cb$uplift = df_cb$cb_r.y1_ct1 - df_cb$cb_r.y1_ct0

df_cb <- df_cb[order(df_cb$Row.names), ] # Ordering according to row-names.

perf_cb <- cbind(group   = df_cb$Row.names,
                 n.ct1    = df_cb$cb_n.ct1,
                 n.ct0    = df_cb$cb_n.ct0, 
                 n.y1_ct1 = df_cb$cb_n.y1_ct1,
                 n.y1_ct0 = df_cb$cb_n.y1_ct0,
                 r.y1_ct1 = df_cb$cb_r.y1_ct1, 
                 r.y1_ct0 = df_cb$cb_r.y1_ct0,
                 uplift   = df_cb$uplift)

class(perf_cb) <- "performance"

perf_cb

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.df_cb.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf_cb[,6], Group = "treatment")
temp.df_cb.control <- data.frame(Decile = seq(1:10), averageRevenue = perf_cb[,7], Group = "control")
temp.df_cb <- rbind(temp.df_cb.treatment, temp.df_cb.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df_cb, aes(x=Decile)) +
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
  ggtitle("Average Revenue Per Decile - CV Causal Boosting") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.df_cb.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf_cb[,6] - perf_cb[,7])
require(ggplot2)
require(scales)
ggplot(temp.df_cb.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-2,7), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - CV Causal Boosting ") + theme(plot.title = element_text(face="bold", size=20)) +
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
cb_r.cumul.y1_ct1 <- cumsum(perf_cb[,"n.y1_ct1"]) / cumsum(perf_cb[,"n.ct1"])
#r.cumul.y1_ct1
cb_r.cumul.y1_ct0 <- cumsum(perf_cb[,"n.y1_ct0"]) / cumsum(perf_cb[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

cb_r.cumul.y1_ct1[is.na(cb_r.cumul.y1_ct1)] <- 0
cb_r.cumul.y1_ct0[is.na(cb_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
cb_inc.gains = c(0.0,(cb_r.cumul.y1_ct1 - cb_r.cumul.y1_ct0) * deciles)
cb_inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
cb_overall.inc.gains <- sum(perf_cb[, "n.y1_ct1"]) / sum(perf_cb[, "n.ct1"]) - sum(perf_cb[, "n.y1_ct0"]) / sum(perf_cb[, "n.ct0"])
cb_overall.inc.gains

### Random incremental gains
cb_random.inc.gains <- c(0, cumsum(rep(cb_overall.inc.gains / groups, groups)))
cb_random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
cb_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
cb_x
cb_y <- cb_inc.gains
cb_y

cb_auuc <- 0
cb_auuc.rand <- 0

for (i in 2:length(cb_x)) {
  cb_auuc <- cb_auuc + 0.5 * (cb_x[i] - cb_x[i-1]) * (cb_y[i] + cb_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
cb_y.rand <- cb_random.inc.gains

for (i in 2:length(cb_x)) {
  cb_auuc.rand <- cb_auuc.rand + 0.5 * (cb_x[i] - cb_x[i-1]) * (cb_y.rand[i] + cb_y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
cb_Qini <- cb_auuc - cb_auuc.rand
cb_Qini
cb_miny <- 100 * min(c(cb_random.inc.gains, cb_inc.gains))
cb_maxy <- 100 * max(c(cb_random.inc.gains, cb_inc.gains))

# The last step is to plot the QINI-curve
plot(cb_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(cb_miny, cb_maxy))
lines(cb_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("CV Causal Boosting", "Random"), 
       col=c("blue", "red"), lty=c(2,1))

# Evaluation BART  HILLSTROM ----------------------------------------
bart_hillstrom_preds = read.csv("/Users/asmir/hu_apa/APA_predictions/bart_h_s_preds.csv")
head(bart_hillstrom_preds)
bart_hillstrom_preds = bart_hillstrom_preds[,2]
bart_hillstrom_preds

# We rank the uplift scores from high to low and add this infromation to a dataframe 
mmbart_ = cbind(uplift = bart_hillstrom_preds,
                target = h_s.test$spend,
                treatment = h_s.test$treatment,
                uplift_rank = length(bart_hillstrom_preds) +1 - rank(bart_hillstrom_preds, ties.method = "random"))

head(mmbart_)
#View(mmbart_)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bkbart_ = unique(quantile(mmbart_[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bkbart_)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mmbart_ = cbind(mmbart_, decile = cut(mmbart_[,4], breaks = bkbart_, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mmbart_)
View(mmbart_)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

bart_n.y1_ct0 <- tapply(mmbart_[mmbart_[, 3] == 0, ][, 2], mmbart_[mmbart_[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#bart_n.y1_ct0
bart_n.y1_ct1 <- tapply(mmbart_[mmbart_[, 3] == 1, ][, 2], mmbart_[mmbart_[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#bart_n.y1_ct1
bart_r.y1_ct0 <- tapply(mmbart_[mmbart_[, 3] == 0, ][, 2], mmbart_[mmbart_[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#bart_r.y1_ct0
bart_r.y1_ct1 <- tapply(mmbart_[mmbart_[, 3] == 1, ][, 2], mmbart_[mmbart_[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#bart_r.y1_ct1
bart_n.ct0 <- tapply(mmbart_[mmbart_[, 3] == 0, ][, 2], mmbart_[mmbart_[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#bart_n.ct0
bart_n.ct1 <- tapply(mmbart_[mmbart_[, 3] == 1, ][, 2], mmbart_[mmbart_[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#bart_n.ct1
#bart_n.y1_ct0
#bart_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
bart_r.y1_ct0 <- ifelse(is.na(bart_r.y1_ct0), 0, bart_r.y1_ct0)
bart_r.y1_ct1 <- ifelse(is.na(bart_r.y1_ct1), 0, bart_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
dfbart_ <- merge(cbind(bart_n.y1_ct0, bart_r.y1_ct0, bart_n.ct0), cbind(bart_n.y1_ct1, bart_r.y1_ct1, bart_n.ct1), by= "row.names", all = TRUE)             

dfbart_$Row.names <- as.numeric(dfbart_$Row.names)
dfbart_[, c(2, 4, 5, 7)][is.na(dfbart_[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

dfbart_$uplift = dfbart_$bart_r.y1_ct1 - dfbart_$bart_r.y1_ct0

dfbart_ <- dfbart_[order(dfbart_$Row.names), ] # Ordering according to row-names.

perfbart_ <- cbind(group   = dfbart_$Row.names,
                   n.ct1    = dfbart_$bart_n.ct1,
                   n.ct0    = dfbart_$bart_n.ct0, 
                   n.y1_ct1 = dfbart_$bart_n.y1_ct1,
                   n.y1_ct0 = dfbart_$bart_n.y1_ct0,
                   r.y1_ct1 = dfbart_$bart_r.y1_ct1, 
                   r.y1_ct0 = dfbart_$bart_r.y1_ct0,
                   uplift   = dfbart_$uplift)

class(perfbart_) <- "performance"

perfbart_

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.dfbart_.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perfbart_[,6], Group = "treatment")
temp.dfbart_.control <- data.frame(Decile = seq(1:10), averageRevenue = perfbart_[,7], Group = "control")
temp.dfbart_ <- rbind(temp.dfbart_.treatment, temp.dfbart_.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.dfbart_, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,9), name="Average Revenue (Euro)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Average Revenue Per Decile - BART") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.dfbart_.uplift <- data.frame(Deciles = seq(1:10), Uplift = perfbart_[,6] - perfbart_[,7])
require(ggplot2)
require(scales)
ggplot(temp.dfbart_.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-1,3), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - BART") + theme(plot.title = element_text(face="bold", size=20)) +
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
bart_r.cumul.y1_ct1 <- cumsum(perfbart_[,"n.y1_ct1"]) / cumsum(perfbart_[,"n.ct1"])
#r.cumul.y1_ct1
bart_r.cumul.y1_ct0 <- cumsum(perfbart_[,"n.y1_ct0"]) / cumsum(perfbart_[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

bart_r.cumul.y1_ct1[is.na(bart_r.cumul.y1_ct1)] <- 0
bart_r.cumul.y1_ct0[is.na(bart_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
bart_inc.gains = c(0.0,(bart_r.cumul.y1_ct1 - bart_r.cumul.y1_ct0) * deciles)
bart_inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
bart_overall.inc.gains <- sum(perfbart_[, "n.y1_ct1"]) / sum(perfbart_[, "n.ct1"]) - sum(perfbart_[, "n.y1_ct0"]) / sum(perfbart_[, "n.ct0"])
bart_overall.inc.gains

### Random incremental gains
bart_random.inc.gains <- c(0, cumsum(rep(bart_overall.inc.gains / groups, groups)))
bart_random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
bart_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
bart_x
bart_y <- bart_inc.gains
bart_y

bart_auuc <- 0
bart_auuc.rand <- 0

for (i in 2:length(bart_x)) {
  bart_auuc <- bart_auuc + 0.5 * (bart_x[i] - bart_x[i-1]) * (bart_y[i] + bart_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
bart_y.rand <- bart_random.inc.gains

for (i in 2:length(bart_x)) {
  bart_auuc.rand <- bart_auuc.rand + 0.5 * (bart_x[i] - bart_x[i-1]) * (bart_y.rand[i] + bart_y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
bart_Qini <- bart_auuc - bart_auuc.rand
bart_Qini
bart_miny <- 100 * min(c(bart_random.inc.gains, bart_inc.gains))
bart_maxy <- 100 * max(c(bart_random.inc.gains, bart_inc.gains))

# The last step is to plot the QINI-curve
plot(bart_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(bart_miny, bart_maxy))
lines(bart_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("BART", "Random"), 
       col=c("blue", "red"), lty=c(2,1))

# Evaluation Causal Honest Tree 

ct_hillstrom_preds = read.csv("/Users/asmir/hu_apa/APA_predictions/ct_h_s_preds.csv")
head(ct_hillstrom_preds)
ct_hillstrom_preds = ct_hillstrom_preds[,2]
ct_hillstrom_preds

# We rank the uplift scores from high to low and add this infromation to a dataframe 
mmct_ = cbind(uplift = ct_hillstrom_preds,
              target = h_s.test$spend,
              treatment = h_s.test$treatment,
              uplift_rank = length(ct_hillstrom_preds) +1 - rank(ct_hillstrom_preds, ties.method = "random"))

head(mmct_)
#View(mmct_)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bkct_ = unique(quantile(mmct_[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bkct_)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mmct_ = cbind(mmct_, decile = cut(mmct_[,4], breaks = bkct_, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mmct_)
View(mmct_)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

ct_n.y1_ct0 <- tapply(mmct_[mmct_[, 3] == 0, ][, 2], mmct_[mmct_[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#ct_n.y1_ct0
ct_n.y1_ct1 <- tapply(mmct_[mmct_[, 3] == 1, ][, 2], mmct_[mmct_[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#ct_n.y1_ct1
ct_r.y1_ct0 <- tapply(mmct_[mmct_[, 3] == 0, ][, 2], mmct_[mmct_[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#ct_r.y1_ct0
ct_r.y1_ct1 <- tapply(mmct_[mmct_[, 3] == 1, ][, 2], mmct_[mmct_[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#ct_r.y1_ct1
ct_n.ct0 <- tapply(mmct_[mmct_[, 3] == 0, ][, 2], mmct_[mmct_[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#ct_n.ct0
ct_n.ct1 <- tapply(mmct_[mmct_[, 3] == 1, ][, 2], mmct_[mmct_[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#ct_n.ct1
#ct_n.y1_ct0
#ct_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
ct_r.y1_ct0 <- ifelse(is.na(ct_r.y1_ct0), 0, ct_r.y1_ct0)
ct_r.y1_ct1 <- ifelse(is.na(ct_r.y1_ct1), 0, ct_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
dfct_ <- merge(cbind(ct_n.y1_ct0, ct_r.y1_ct0, ct_n.ct0), cbind(ct_n.y1_ct1, ct_r.y1_ct1, ct_n.ct1), by= "row.names", all = TRUE)             

dfct_$Row.names <- as.numeric(dfct_$Row.names)
dfct_[, c(2, 4, 5, 7)][is.na(dfct_[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

dfct_$uplift = dfct_$ct_r.y1_ct1 - dfct_$ct_r.y1_ct0

dfct_ <- dfct_[order(dfct_$Row.names), ] # Ordering according to row-names.

perfct_ <- cbind(group   = dfct_$Row.names,
                 n.ct1    = dfct_$ct_n.ct1,
                 n.ct0    = dfct_$ct_n.ct0, 
                 n.y1_ct1 = dfct_$ct_n.y1_ct1,
                 n.y1_ct0 = dfct_$ct_n.y1_ct0,
                 r.y1_ct1 = dfct_$ct_r.y1_ct1, 
                 r.y1_ct0 = dfct_$ct_r.y1_ct0,
                 uplift   = dfct_$uplift)

class(perfct_) <- "performance"

perfct_

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.dfct_.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perfct_[,6], Group = "treatment")
temp.dfct_.control <- data.frame(Decile = seq(1:10), averageRevenue = perfct_[,7], Group = "control")
temp.dfct_ <- rbind(temp.dfct_.treatment, temp.dfct_.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.dfct_, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,10), name="Average Revenue (Euro)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Average Revenue Per Decile - Causal Honest Tree") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.dfct_.uplift <- data.frame(Deciles = seq(1:10), Uplift = perfct_[,6] - perfct_[,7])
require(ggplot2)
require(scales)
ggplot(temp.dfct_.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-1,4), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Causal Honest Tree") + theme(plot.title = element_text(face="bold", size=20)) +
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
ct_r.cumul.y1_ct1 <- cumsum(perfct_[,"n.y1_ct1"]) / cumsum(perfct_[,"n.ct1"])
#r.cumul.y1_ct1
ct_r.cumul.y1_ct0 <- cumsum(perfct_[,"n.y1_ct0"]) / cumsum(perfct_[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

ct_r.cumul.y1_ct1[is.na(ct_r.cumul.y1_ct1)] <- 0
ct_r.cumul.y1_ct0[is.na(ct_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
ct_inc.gains = c(0.0,(ct_r.cumul.y1_ct1 - ct_r.cumul.y1_ct0) * deciles)
ct_inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
ct_overall.inc.gains <- sum(perfct_[, "n.y1_ct1"]) / sum(perfct_[, "n.ct1"]) - sum(perfct_[, "n.y1_ct0"]) / sum(perfct_[, "n.ct0"])
ct_overall.inc.gains

### Random incremental gains
ct_random.inc.gains <- c(0, cumsum(rep(ct_overall.inc.gains / groups, groups)))
ct_random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
ct_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
ct_x
ct_y <- ct_inc.gains
ct_y

ct_auuc <- 0
ct_auuc.rand <- 0

for (i in 2:length(ct_x)) {
  ct_auuc <- ct_auuc + 0.5 * (ct_x[i] - ct_x[i-1]) * (ct_y[i] + ct_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
ct_y.rand <- ct_random.inc.gains

for (i in 2:length(ct_x)) {
  ct_auuc.rand <- ct_auuc.rand + 0.5 * (ct_x[i] - ct_x[i-1]) * (ct_y.rand[i] + ct_y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
ct_Qini <- ct_auuc - ct_auuc.rand
ct_Qini
ct_miny <- 100 * min(c(ct_random.inc.gains, ct_inc.gains))
ct_maxy <- 100 * max(c(ct_random.inc.gains, ct_inc.gains))

# The last step is to plot the QINI-curve
plot(ct_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(ct_miny, ct_maxy))
lines(ct_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("Causal Honest Tree", "Random"),
       col=c("blue", "red"), lty=c(2,1))

# Plot übereinander

plot(ct_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(ct_miny, ct_maxy),
     cb_inc.gains * 100 ~ c(c, seq(100/ groups, 100, 100 / groups)), type = "b",
     col = "green", lty = 2,
     cf_inc.gains * 100 ~ c(c, seq(100/ groups, 100, 100 / groups)), type = "b",
     col = "green", lty = 2,
     bart_inc.gains * 100 ~ c(c, seq(100/ groups, 100, 100 / groups)), type = "b",
     col = "green", lty = 2)
lines(ct_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("Causal Honest Tree", "Random"),
       col=c("blue", "red"), lty=c(2,1))

# Evaluation CTWO MODELL HILLSTROM -----------------------------------------
tm_hillstrom_preds = read.csv("/Users/asmir/hu_apa/APA_predictions/tm_preds/tm_h_s_preds.csv")
head(tm_hillstrom_preds)
tm_hillstrom_preds = tm_hillstrom_preds[,2]
tm_hillstrom_preds
# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_tm = cbind(uplift = tm_hillstrom_preds,
              target = h_s.test$spend,
              treatment = h_s.test$treatment,
              uplift_rank = length(tm_hillstrom_preds) +1 - rank(tm_hillstrom_preds, ties.method = "random"))

head(mm_tm)
#View(mm_tm)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bk_tm = unique(quantile(mm_tm[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bk_tm)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mm_tm = cbind(mm_tm, decile = cut(mm_tm[,4], breaks = bk_tm, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mm_tm)
View(mm_tm)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

tm_n.y1_ct0 <- tapply(mm_tm[mm_tm[, 3] == 0, ][, 2], mm_tm[mm_tm[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#tm_n.y1_ct0
tm_n.y1_ct1 <- tapply(mm_tm[mm_tm[, 3] == 1, ][, 2], mm_tm[mm_tm[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#tm_n.y1_ct1
tm_r.y1_ct0 <- tapply(mm_tm[mm_tm[, 3] == 0, ][, 2], mm_tm[mm_tm[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#tm_r.y1_ct0
tm_r.y1_ct1 <- tapply(mm_tm[mm_tm[, 3] == 1, ][, 2], mm_tm[mm_tm[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#tm_r.y1_ct1
tm_n.ct0 <- tapply(mm_tm[mm_tm[, 3] == 0, ][, 2], mm_tm[mm_tm[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#tm_n.ct0
tm_n.ct1 <- tapply(mm_tm[mm_tm[, 3] == 1, ][, 2], mm_tm[mm_tm[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#tm_n.ct1
#tm_n.y1_ct0
#tm_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
tm_r.y1_ct0 <- ifelse(is.na(tm_r.y1_ct0), 0, tm_r.y1_ct0)
tm_r.y1_ct1 <- ifelse(is.na(tm_r.y1_ct1), 0, tm_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
df_tm <- merge(cbind(tm_n.y1_ct0, tm_r.y1_ct0, tm_n.ct0), cbind(tm_n.y1_ct1, tm_r.y1_ct1, tm_n.ct1), by= "row.names", all = TRUE)             

df_tm$Row.names <- as.numeric(df_tm$Row.names)
df_tm[, c(2, 4, 5, 7)][is.na(df_tm[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

df_tm$uplift = df_tm$tm_r.y1_ct1 - df_tm$tm_r.y1_ct0

df_tm <- df_tm[order(df_tm$Row.names), ] # Ordering according to row-names.

perf_tm <- cbind(group   = df_tm$Row.names,
                 n.ct1    = df_tm$tm_n.ct1,
                 n.ct0    = df_tm$tm_n.ct0, 
                 n.y1_ct1 = df_tm$tm_n.y1_ct1,
                 n.y1_ct0 = df_tm$tm_n.y1_ct0,
                 r.y1_ct1 = df_tm$tm_r.y1_ct1, 
                 r.y1_ct0 = df_tm$tm_r.y1_ct0,
                 uplift   = df_tm$uplift)

class(perf_tm) <- "performance"

perf_tm

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.df_tm.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf_tm[,6], Group = "treatment")
temp.df_tm.control <- data.frame(Decile = seq(1:10), averageRevenue = perf_tm[,7], Group = "control")
temp.df_tm <- rbind(temp.df_tm.treatment, temp.df_tm.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df_tm, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,10), name="Average Revenue (Euro)") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        # axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=15)) +
  ggtitle("Average Revenue Per Decile - Two Model - Ridge Regression") + theme(plot.title = element_text(face="bold",hjust = 0.5))
## UPLIFT PER DECILE
# By substracting the response rates of the treatment groups with the reponse rates of the control groups
# we achieve the uplift per decile as seen in the next plot.

temp.df_tm.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf_tm[,6] - perf_tm[,7])
require(ggplot2)
require(scales)
ggplot(temp.df_tm.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(0,3), name="Uplift (Treatment - Control) in Average Spend") +
  scale_x_discrete(name ="Deciles", limits=rep(1:10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()) +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20)) +
  ggtitle("Uplift Per Decile - Two Model - Ridge Regression ") + theme(plot.title = element_text(face="bold", size=20)) +
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
tm_r.cumul.y1_ct1 <- cumsum(perf_tm[,"n.y1_ct1"]) / cumsum(perf_tm[,"n.ct1"])
#r.cumul.y1_ct1
tm_r.cumul.y1_ct0 <- cumsum(perf_tm[,"n.y1_ct0"]) / cumsum(perf_tm[,"n.ct0"])
#r.cumul.y1_ct0
deciles <- seq(1 / groups, 1, 1 / groups)
deciles

tm_r.cumul.y1_ct1[is.na(tm_r.cumul.y1_ct1)] <- 0
tm_r.cumul.y1_ct0[is.na(tm_r.cumul.y1_ct0)] <- 0

# Per decile we can calculate the incremental gains for the model performance.
tm_inc.gains = c(0.0,(tm_r.cumul.y1_ct1 - tm_r.cumul.y1_ct0) * deciles)
tm_inc.gains
# The overall incremental gains is basically the overal uplift. The random incremental gains
# is then the overall incremental gains divided by the amount of groups used.

### Overall incremental gains
tm_overall.inc.gains <- sum(perf_tm[, "n.y1_ct1"]) / sum(perf_tm[, "n.ct1"]) - sum(perf_tm[, "n.y1_ct0"]) / sum(perf_tm[, "n.ct0"])
tm_overall.inc.gains

### Random incremental gains
tm_random.inc.gains <- c(0, cumsum(rep(tm_overall.inc.gains / groups, groups)))
tm_random.inc.gains

# Next up we compute the are underneath the incremental curve.
### Compute area under the model incremental gains (uplift) curve 
tm_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
tm_x
tm_y <- tm_inc.gains
tm_y

tm_auuc <- 0
tm_auuc.rand <- 0

for (i in 2:length(tm_x)) {
  tm_auuc <- tm_auuc + 0.5 * (tm_x[i] - tm_x[i-1]) * (tm_y[i] + tm_y[i-1])
}

# We do the same for the area underneath the random incremental curve.
### Compute area under the random incremental gains curve
tm_y.rand <- tm_random.inc.gains

for (i in 2:length(tm_x)) {
  tm_auuc.rand <- tm_auuc.rand + 0.5 * (tm_x[i] - tm_x[i-1]) * (tm_y.rand[i] + tm_y.rand[i-1])
}
# We then compute the difference between those two areas.
### Compute the difference between the areas (Qini coefficient)
tm_Qini <- tm_auuc - tm_auuc.rand
tm_Qini
tm_miny <- 100 * min(c(tm_random.inc.gains, tm_inc.gains))
tm_maxy <- 100 * max(c(tm_random.inc.gains, tm_inc.gains))

# The last step is to plot the QINI-curve
plot(tm_inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
     col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
     ylab = "Cumulative incremental gains (pc pt)", ylim = c(tm_miny, tm_maxy))
lines(tm_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
legend("bottomright", c("Two Model - Ridge Regression", "Random"), 
       col=c("blue", "red"), lty=c(2,1))
