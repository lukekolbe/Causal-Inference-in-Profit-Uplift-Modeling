
library(splitstackshape)

set.seed(101010)

getwd()
b_t <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
#b_t <- read.csv("/Users/Lukas/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")
b_t <- read.csv("H:\\Applied Predictive Analytics\\Data\\BooksAndToys.csv", sep=",")
b_t <- read.csv("/Users/asmir/Desktop/apa_data/BooksAndToys.csv", sep=",")
# Feature Engineering & variable transformation -------------------------------------------------

b_t$z_var <- 0
b_t$z_var <- ifelse(b_t$label>0, 1, 0)
summary(b_t$z_var)

b_t$treatment = numeric(nrow(b_t))
b_t$treatment = ifelse(b_t$controlGroup==0, 1, 0)

# Drop columns with no information
b_t <- b_t[,-which(names(b_t) %in% c("campaignUnit","campaignTags","trackerKey","campaignId","checkoutDiscount","ViewedBefore.cart.",
                                     "TimeToFirst.cart."))]

# Setting specific Column Null Values to 0 (all at once):
varlist=c("InitCartNonEmpty","FrequencyOfPreviousSessions")
b_t[, varlist][is.na(b_t[,varlist])] = 0

for(i in 1:ncol(b_t)){
  b_t[is.na(b_t[,i]), i] <- median(b_t[,i], na.rm = TRUE)
}


#b_t <- b_t[b_t$campaignValue==2000,] # only work with those with campaign-value of "2000" as they are the largest uniform group!


b_t <- b_t[,which(names(b_t) %in% c("checkoutAmount","converted","treatment","label","z_var","TimeSpentOn.overview.","epochSecond",
                                    "TimeToFirst.overview.","TimeSpentOn.product.",
                                    "DurationLastVisitInSeconds","PreviousVisitCount",
                                    "TimeSinceOn.product.","ViewCountLastVisit","SecondsSinceClick",
                                    "FrequencyOfPreviousSessions","NumberOfDifferent.product.",
                                    "ScreenWidth","ClicksPer.product.","NumberOfDifferent.overview.",
                                    "PageIs.product.","DayOfWeek","RepeatCount","PageIs.overview.",
                                    "HourOfDay","InitPageWas.home."))]


#  Splitting into train & test set ----------------------------------------

set.seed(101010)#12

strat_split <- stratified(b_t, c("treatment", "converted"), 0.667, bothSets=TRUE)
b_t.train <- as.data.frame(strat_split[[1]])
b_t.validate <- as.data.frame(strat_split[[2]])

# CAUSAL FOREST BOOKS AND TOYS EVALUATION  --------------------------

cf_b_t_preds = read.csv2("/Users/asmir/hu_apa/APA_predictions/cf_preds/cf_b_t_preds.csv")
head(cf_b_t_preds)
cf_b_t_preds = cf_b_t_preds[,2]
cf_b_t_preds

#table(cf_hillstrom_preds != 0)
# We rank the uplift scores from high to low and add this infromation to a dataframe 
# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_cf = cbind(uplift = cf_b_t_preds,
              target = b_t.validate$checkoutAmount,
              treatment = b_t.validate$treatment,
              uplift_rank = length(cf_b_t_preds) +1 - rank(cf_b_t_preds, ties.method = "random"))

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
  scale_y_continuous(labels=, limits=c(0,22), name="Average Revenue (Euro)") +
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
  scale_y_continuous(labels=, limits=c(-4,4), name="Uplift (Treatment - Control) in Average Spend") +
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
#r.cumul.y1_ct1
cf_r.cumul.y1_ct0 <- cumsum(perf_cf[,"n.y1_ct0"]) / cumsum(perf_cf[,"n.ct0"])
#r.cumul.y1_ct0
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

for (i in 2:length(cf_x)) {
  cf_auuc <- cf_auuc + 0.5 * (cf_x[i] - cf_x[i-1]) * (cf_y[i] + cf_y[i-1])
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

# BART BOOKS AND TOYS EVALUATION ------------
bart_b_t_preds = read.csv("/Users/asmir/hu_apa/APA_predictions/bart_preds/bart_b_t_preds.csv")
head(bart_b_t_preds)
bart_b_t_preds = bart_b_t_preds[,2]
bart_b_t_preds

#table(bart_hillstrom_preds != 0)
# We rank the uplift scores from high to low and add this infromation to a dataframe 
# We rank the uplift scores from high to low and add this infromation to a dataframe 
mm_bart = cbind(uplift = bart_b_t_preds,
                target = b_t.validate$checkoutAmount,
                treatment = b_t.validate$treatment,
                uplift_rank = length(bart_b_t_preds) +1 - rank(bart_b_t_preds, ties.method = "random"))

head(mm_bart)
#View(mm_bart)
# Adterwards we devide the observation into 10 equal groups. The first group will contain the highest uplit scores,
# the second group the second highest-scores and so on.
# There is a possibilty of having obersevations with the same uplift score and there is a chance that these will
# not be part of the same group. If this is the case, the observations are assigned randomly.

groups = 10
bk_bart = unique(quantile(mm_bart[,4], probs = seq(0,1,1 / groups)))
#View(bk)
#head(mm) # model matrix
#mm[,4] # uplift column of model matrix 
if ((length(bk_bart)-1) != groups){
  warning ("uplift: due to many ties in uplift predictions, the ties will be dealt with randomly", groups)
}
mm_bart = cbind(mm_bart, decile = cut(mm_bart[,4], breaks = bk_bart, labels = NULL, include.lowest = T))

# Previewing the dataframe
head(mm_bart)
#View(mm_bart)

# We have now ranked all the observations in the test according to uplift socre and assigned them into a group
# (according to their ranking). The next step is to test the actual values like per group:
# - How many beloned to the treatment group?
# - How many to the control group?
# - How many of those have converted?

bart_n.y1_ct0 <- tapply(mm_bart[mm_bart[, 3] == 0, ][, 2], mm_bart[mm_bart[, 3] == 0, ][, 5], sum)  # Sum of revenue of people not having received the treatment
#bart_n.y1_ct0
bart_n.y1_ct1 <- tapply(mm_bart[mm_bart[, 3] == 1, ][, 2], mm_bart[mm_bart[, 3] == 1, ][, 5], sum)  # Sum of revenue of people having received the treatment
#bart_n.y1_ct1
bart_r.y1_ct0 <- tapply(mm_bart[mm_bart[, 3] == 0, ][, 2], mm_bart[mm_bart[, 3] == 0, ][, 5], mean) # Average revenue of people not having received the treatment
#bart_r.y1_ct0
bart_r.y1_ct1 <- tapply(mm_bart[mm_bart[, 3] == 1, ][, 2], mm_bart[mm_bart[, 3] == 1, ][, 5], mean) # Average revenue of people having received the treatment
#bart_r.y1_ct1
bart_n.ct0 <- tapply(mm_bart[mm_bart[, 3] == 0, ][, 2], mm_bart[mm_bart[, 3] == 0, ][, 5], length)  # Sum of people not having received the treatment
#bart_n.ct0
bart_n.ct1 <- tapply(mm_bart[mm_bart[, 3] == 1, ][, 2], mm_bart[mm_bart[, 3] == 1, ][, 5], length)  # Sum of people having received the treatment
#bart_n.ct1
bart_n.y1_ct0
#bart_n.y1_ct1
# In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
# We set these to 0.
bart_r.y1_ct0 <- ifelse(is.na(bart_r.y1_ct0), 0, bart_r.y1_ct0)
bart_r.y1_ct1 <- ifelse(is.na(bart_r.y1_ct1), 0, bart_r.y1_ct1)

# We grop these statistic into a new dataframe and call it a performanee class.
df_bart <- merge(cbind(bart_n.y1_ct0, bart_r.y1_ct0, bart_n.ct0), cbind(bart_n.y1_ct1, bart_r.y1_ct1, bart_n.ct1), by= "row.names", all = TRUE)             

df_bart$Row.names <- as.numeric(df_bart$Row.names)
df_bart[, c(2, 4, 5, 7)][is.na(df_bart[, c(2, 4, 5, 7)])] <- 0 # missing implies 0 counts

df_bart$uplift = df_bart$bart_r.y1_ct1 - df_bart$bart_r.y1_ct0

df_bart <- df_bart[order(df_bart$Row.names), ] # Ordering according to row-names.

perf_bart <- cbind(group   = df_bart$Row.names,
                   n.ct1    = df_bart$bart_n.ct1,
                   n.ct0    = df_bart$bart_n.ct0, 
                   n.y1_ct1 = df_bart$bart_n.y1_ct1,
                   n.y1_ct0 = df_bart$bart_n.y1_ct0,
                   r.y1_ct1 = df_bart$bart_r.y1_ct1, 
                   r.y1_ct0 = df_bart$bart_r.y1_ct0,
                   uplift   = df_bart$uplift)

class(perf_bart) <- "performance"

perf_bart

# Now that we have the new performance-class we can use it to produce some graphs.

## RESPONSE RATE PER DECILE
temp.df_bart.treatment <- data.frame(Decile = seq(1:10), averageRevenue = perf_bart[,6], Group = "treatment")
temp.df_bart.control <- data.frame(Decile = seq(1:10), averageRevenue = perf_bart[,7], Group = "control")
temp.df_bart <- rbind(temp.df_bart.treatment, temp.df_bart.control)
#View(temp.df)

require(ggplot2)
require(scales)
ggplot(temp.df_bart, aes(x=Decile)) +
  geom_bar(stat="identity", aes(y=averageRevenue, fill = Group), position = "dodge") + 
  scale_y_continuous(labels=, limits=c(0,18), name="Average Revenue (Euro)") +
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

temp.df_bart.uplift <- data.frame(Deciles = seq(1:10), Uplift = perf_bart[,6] - perf_bart[,7])
require(ggplot2)
require(scales)
ggplot(temp.df_bart.uplift, aes(x=Deciles)) +
  geom_bar(stat="identity", aes(y =Uplift, fill="red")) + 
  scale_y_continuous(labels=, limits=c(-2,3), name="Uplift (Treatment - Control) in Average Spend") +
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
bart_r.cumul.y1_ct1 <- cumsum(perf_bart[,"n.y1_ct1"]) / cumsum(perf_bart[,"n.ct1"])
#r.cumul.y1_ct1
bart_r.cumul.y1_ct0 <- cumsum(perf_bart[,"n.y1_ct0"]) / cumsum(perf_bart[,"n.ct0"])
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
bart_overall.inc.gains <- sum(perf_bart[, "n.y1_ct1"]) / sum(perf_bart[, "n.ct1"]) - sum(perf_bart[, "n.y1_ct0"]) / sum(perf_bart[, "n.ct0"])
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
lines(bart_random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)legend("bottomright", c("BART", "Random"), 
                                                                                                                        col=c("blue", "red"), lty=c(2,1))

