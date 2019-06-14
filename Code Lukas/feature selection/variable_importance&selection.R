

library(caret)

bt_rfe_label <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_b_t.results_label.rds")
fa_rfe_label <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_f_a.results_label.rds")
fb_rfe_label <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_f_b.results_label.rds")

bt_rfe_amount <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_b_t.results_amount.rds")
fa_rfe_amount <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_f_a.results_amount.rds")
fb_rfe_amount <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/rfe good/rfe_f_b.results_amount.rds")





#fb_urf <- readRDS("/Volumes/kolbeluk/upliftRF_f_b2_new.rds")
#fa_urf <- readRDS("/Volumes/kolbeluk/upliftRF_f_a2_new.rds")
#fa_ctree <- readRDS("/Volumes/kolbeluk/tree_f_a1.new.rds")
# summary(fa_ctree)
# fa_ctree_importance <- data.frame(fa_ctree$variable.importance)
# fa_ctree_importance
# 
# summary(bt_rfe)


# summarize the results
print(bt_rfe_label)
print(fa_rfe_label)
print(fb_rfe_label)

print(bt_rfe_amount)
print(fa_rfe_amount)
print(fb_rfe_amount)

# list the chosen features
predictors(bt_rfe_label)
predictors(fa_rfe_label)
predictors(fb_rfe_label)

predictors(bt_rfe_amount)
predictors(fa_rfe_amount)
predictors(fb_rfe_amount)


# plot the results
plot(bt_rfe_label, type=c("g", "o"))
plot(fa_rfe_label, type=c("g", "o"))
plot(fb_rfe_label, type=c("g", "o"))

plot(bt_rfe_amount, type=c("g", "o"))
plot(fa_rfe_amount, type=c("g", "o"))
plot(fb_rfe_amount, type=c("g", "o"))


# rfe_var <- fb_rfe$variables
# rfe_var[rfe_var$Variables==15,]



# BooksToys RFE Results  -------------------------------------------


#https://github.com/topepo/caret/issues/487

bt_imps_label <- bt_rfe_label$variables
bt_imps_label <- bt_imps_label[order(-bt_imps_label$Variables, bt_imps_label$Resample, bt_imps_label$var),]

bt_imps_amount <- bt_rfe_amount$variables
bt_imps_amount <- bt_imps_amount[order(-bt_imps_amount$Variables, bt_imps_amount$Resample, bt_imps_amount$var),]

library(plyr)

###LABEL AVERAGE
averaged_bt_label <- ddply(subset(bt_imps_label, Variables == 51),
                     .(var), 
                     function(x) c(Mean = mean(x$Overall),
                                   sd = sd(x$Overall)))

averaged_bt_label$resamp_rank <- rank(averaged_bt_label$Mean)

sort(averaged_bt_label$var[averaged_bt_label$resamp_rank <= 51])


###AMOUNT AVERAGE
averaged_bt_amount <- ddply(subset(bt_imps_amount, Variables == 51),
                     .(var), 
                     function(x) c(Mean = mean(x$Overall),
                                         sd = sd(x$Overall)))

averaged_bt_amount$resamp_rank <- rank(averaged_bt_amount$Mean)

sort(averaged_bt_amount$var[averaged_bt_amount$resamp_rank <= 51])



# Fashion A RFE Results ---------------------------------------------------



fa_imps_label <- fa_rfe_label$variables
fa_imps_label <- fa_imps_label[order(-fa_imps_label$Variables, fa_imps_label$Resample, fa_imps_label$var),]

fa_imps_amount <- fa_rfe_amount$variables
fa_imps_amount <- fa_imps_amount[order(-fa_imps_amount$Variables, fa_imps_amount$Resample, fa_imps_amount$var),]

library(plyr)

###LABEL AVERAGE
averaged_fa_label <- ddply(subset(fa_imps_label, Variables == 51),
                           .(var), 
                           function(x) c(Mean = mean(x$Overall),
                                         sd = sd(x$Overall)))

averaged_fa_label$resamp_rank <- rank(averaged_fa_label$Mean)

sort(averaged_fa_label$var[averaged_fa_label$resamp_rank <= 51])


###AMOUNT AVERAGE
averaged_fa_amount <- ddply(subset(fa_imps_amount, Variables == 51),
                            .(var), 
                            function(x) c(Mean = mean(x$Overall),
                                          sd = sd(x$Overall)))

averaged_fa_amount$resamp_rank <- rank(averaged_fa_amount$Mean)

sort(averaged_fa_amount$var[averaged_fa_amount$resamp_rank <= 51])


# Fashion B RFE Results ---------------------------------------------------

fb_imps_label <- fb_rfe_label$variables
fb_imps_label <- fb_imps_label[order(-fb_imps_label$Variables, fb_imps_label$Resample, fb_imps_label$var),]

fb_imps_amount <- fb_rfe_amount$variables
fb_imps_amount <- fb_imps_amount[order(-fb_imps_amount$Variables, fb_imps_amount$Resample, fb_imps_amount$var),]

library(plyr)

###LABEL AVERAGE
averaged_fb_label <- ddply(subset(fb_imps_label, Variables == 51),
                           .(var), 
                           function(x) c(Mean = mean(x$Overall),
                                         sd = sd(x$Overall)))

averaged_fb_label$resamp_rank <- rank(averaged_fb_label$Mean)

sort(averaged_fb_label$var[averaged_fb_label$resamp_rank <= 51])


###AMOUNT AVERAGE
averaged_fb_amount <- ddply(subset(fb_imps_amount, Variables == 51),
                            .(var), 
                            function(x) c(Mean = mean(x$Overall),
                                          sd = sd(x$Overall)))

averaged_fb_amount$resamp_rank <- rank(averaged_fb_amount$Mean)

sort(averaged_fb_amount$var[averaged_fb_amount$resamp_rank <= 51])




# trying a complete set across all datasets -------------------------------

####LABEL
all_imps_label <- rbind(fa_rfe_label$variables, fb_rfe_label$variables, bt_rfe_label$variables)
all_imps_label <- all_imps_label[order(-all_imps_label$Variables, all_imps_label$Resample, all_imps_label$var),]

library(plyr)

averaged_all_label <- ddply(subset(all_imps_label, Variables == 51),
                      .(var), 
                      function(x) c(Mean = mean(x$Overall),
                                    sd = sd(x$Overall)))

averaged_all_label$resamp_rank <- rank(averaged_all_label$Mean)

sort(averaged_all_label$var[averaged_all_label$resamp_rank <= 51])



####AMOUNT
all_imps_amount <- rbind(fa_rfe_amount$variables, fb_rfe_amount$variables, bt_rfe_amount$variables)
all_imps_amount <- all_imps_amount[order(-all_imps_amount$Variables, all_imps_amount$Resample, all_imps_amount$var),]

library(plyr)

averaged_all_amount <- ddply(subset(all_imps_amount, Variables == 51),
                            .(var), 
                            function(x) c(Mean = mean(x$Overall),
                                          sd = sd(x$Overall)))

averaged_all_amount$resamp_rank <- rank(averaged_all_amount$Mean)

sort(averaged_all_amount$var[averaged_all_amount$resamp_rank <= 51])

###

averaged_all_amount$absolutes <- abs(averaged_all_amount$Mean)
averaged_all_label$absolutes <- abs(averaged_all_label$Mean)

sort(averaged_all_amount$absolutes)
sort(averaged_all_label$absolutes)

write_csv2(averaged_all_amount, "rfe_averaged_amount.csv")
write_csv2(averaged_all_label, "rfe_averaged_label.csv")






# correlation importances (after cleaning!) -------------------------------

upliftRF_bt <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/UpliftRF results/upliftRF_b_t.rds")
upliftRF_fa <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/UpliftRF results/upliftRF_f_a2_new.rds")
upliftRF_fb <- readRDS("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/UpliftRF results/upliftRF_f_b2_new.rds")

varImportance(upliftRF_bt)
varImportance(upliftRF_fa)
varImportance(upliftRF_fb)


f_a <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionA.csv", sep=",")
f_b  <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/fashion/FashionB.csv", sep=",")
b_t <- read.csv("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/books and toys/BooksAndToys.csv", sep=",")

str(f_a)
str(f_b)
str(b_t)

summary(b_t$ChannelIs.EMAIL.)
summary(f_a$ChannelIs.EMAIL.)



correlationMatrix_fa <- cor(f_a)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

correlationMatrix_fb <- cor(f_b)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)

correlationMatrix_bt <- cor(b_t)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=TRUE, verbose=TRUE)


checkout_cor <- as.data.frame(cbind(fa=correlationMatrix_fa[,2],fb=correlationMatrix_fb[,2], bt=correlationMatrix_bt[,2]))
write_csv2(checkout_cor, "checkout_cor.csv")

