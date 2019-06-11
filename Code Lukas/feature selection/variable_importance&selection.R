

library(caret)

bt_rfe <- readRDS("/Volumes/kolbeluk/rfe_b_t.results.rds")
fa_rfe <- readRDS("/Volumes/kolbeluk/rfe_f_a.results.rds")
fb_rfe <- readRDS("/Volumes/kolbeluk/rfe_f_b.results.rds")

fb_urf <- readRDS("/Volumes/kolbeluk/upliftRF_f_b2_new.rds")
fa_urf <- readRDS("/Volumes/kolbeluk/upliftRF_f_a2_new.rds")

fa_ctree <- readRDS("/Volumes/kolbeluk/tree_f_a1.new.rds")
summary(fa_ctree)
fa_ctree_importance <- data.frame(fa_ctree$variable.importance)
fa_ctree_importance

summary(bt_rfe)


# summarize the results
print(bt_rfe)
print(fa_rfe)
print(fb_rfe)

# list the chosen features
predictors(bt_rfe)
predictors(fa_rfe)
predictors(fb_rfe)




# plot the results
plot(bt_rfe, type=c("g", "o"))
plot(fa_rfe, type=c("g", "o"))
plot(fb_rfe, type=c("g", "o"))


rfe_var <- fb_rfe$variables
rfe_var[rfe_var$Variables==15,]



# BooksToys RFE Results  -------------------------------------------


#https://github.com/topepo/caret/issues/487

bt_imps <- bt_rfe$variables
bt_imps <- bt_imps[order(-bt_imps$Variables, bt_imps$Resample, bt_imps$var),]

library(plyr)

averaged_bt <- ddply(subset(bt_imps, Variables == 8),
                     .(var), 
                     function(x) c(Mean = mean(x$Overall),
                                   sd = sd(x$Overall)))

averaged_bt$resamp_rank <- rank(averaged_bt$Mean)

sort(averaged_bt$var[averaged_bt$resamp_rank <= 8])



# Fashion A RFE Results ---------------------------------------------------



fa_imps <- fa_rfe$variables
fa_imps <- fa_imps[order(-fa_imps$Variables, fa_imps$Resample, fa_imps$var),]

library(plyr)

averaged_fa <- ddply(subset(fa_imps, Variables == 8),
                     .(var), 
                     function(x) c(Mean = mean(x$Overall),
                                   sd = sd(x$Overall)))

averaged_fa$resamp_rank <- rank(averaged_fa$Mean)

sort(averaged_fa$var[averaged_fa$resamp_rank <= 8])



# Fashion B RFE Results ---------------------------------------------------

fb_imps <- fb_rfe$variables
fb_imps <- fb_imps[order(-fb_imps$Variables, fb_imps$Resample, fb_imps$var),]

library(plyr)

averaged_fb <- ddply(subset(fb_imps, Variables == 12),
                     .(var), 
                     function(x) c(Mean = mean(x$Overall),
                                   sd = sd(x$Overall)))

averaged_fb$resamp_rank <- rank(averaged_fb$Mean)

sort(averaged_fb$var[averaged_fb$resamp_rank <= 8])




# trying a complete set across all datasets -------------------------------

all_imps <- rbind(fa_rfe$variables, fb_rfe$variables, bt_rfe$variables)
all_imps <- all_imps[order(-all_imps$Variables, all_imps$Resample, all_imps$var),]

library(plyr)

averaged_all <- ddply(subset(all_imps, Variables == 8),
                      .(var), 
                      function(x) c(Mean = mean(x$Overall),
                                    sd = sd(x$Overall)))

averaged_all$resamp_rank <- rank(averaged_all$Mean)

sort(averaged_all$var[averaged_all$resamp_rank <= 8])


