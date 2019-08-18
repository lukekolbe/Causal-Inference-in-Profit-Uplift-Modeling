# Function Evaluation setup ----------------------------------------------------------------
# This function uses our test dataset and model predictions to compute a model performance matrix 
predEval <- function(t.d, p){
  
  # We rank the uplift scores from high to low and add this infromation to a dataframe
  mm_pred = cbind(
    uplift = p,
    target = t.d$checkoutAmount,
    treatment = t.d$treatment,
    uplift_rank = length(p) + 1 - rank(p, ties.method = "random")
  )
  # We define deciles (groups), which get used in the literature to evaluate our model performance across 10 equally sized sub populations
  groups = 10
  bk_pred = unique(quantile(mm_pred[, 4], probs = seq(0, 1, 1 / groups)))
  
  if ((length(bk_pred) - 1) != groups) {
    warning (
      "uplift: due to many ties in uplift predictions, the ties will be dealt with randomly",
      groups
    )
  }
  # We add the deciles to our model matrix 
  mm_pred = cbind(mm_pred,
                  decile = cut(
                    mm_pred[, 4],
                    breaks = bk_pred,
                    labels = NULL,
                    include.lowest = T
                  ))
  # We compute the following variables from our model matrix
  pred_n.y1_ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], sum)  # Sum of revenue of people not having received the treatment
  pred_n.y1_ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], sum)  # Sum of revenue of people having received the treatment
  pred_r.y1_ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], mean) # Average revenue of people not having received the treatment
  pred_r.y1_ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], mean) # Average revenue of people having received the treatment
  pred_n.ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], length)  # Sum of people not having received the treatment
  pred_n.ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], length)  # Sum of people having received the treatment

  # In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
  # We set these to 0.
  pred_r.y1_ct0 <- ifelse(is.na(pred_r.y1_ct0), 0, pred_r.y1_ct0)
  pred_r.y1_ct1 <- ifelse(is.na(pred_r.y1_ct1), 0, pred_r.y1_ct1)
  # The uplift is the difference of the sum of reveues between our treatment and control group
  uplift = pred_r.y1_ct1 - pred_r.y1_ct0
  # We bind our previously computed variables together to a performance matrix 
  perf_pred <- cbind(group   = c(1:10),
                     n.ct1    = pred_n.ct1,
                     n.ct0    = pred_n.ct0, 
                     n.y1_ct1 = pred_n.y1_ct1,
                     n.y1_ct0 = pred_n.y1_ct0,
                     r.y1_ct1 = pred_r.y1_ct1, 
                     r.y1_ct0 = pred_r.y1_ct0,
                     uplift   = uplift)
  
  return(list(perf_pred, mm_pred))
}

# ARPD & UPD --------------------------------------------------------------
predARPD <- function(n, t) {
  
  perf_pred <- model.performance[[1]]
  
  temp.df_pred.treatment <-
    data.frame(
      Decile = seq(1:10),
      averageRevenue = perf_pred[, 6],
      Group = "treatment"
    )
  temp.df_pred.control <-
    data.frame(
      Decile = seq(1:10),
      averageRevenue = perf_pred[, 7],
      Group = "control"
    )
  temp.df_pred <- rbind(temp.df_pred.treatment, temp.df_pred.control)
  
  arpd.title <- "arpd"
  #arpd.plot <-
  arpd.plot <- ggplot(temp.df_pred, aes(x = Decile)) +
    geom_bar(stat = "identity",
             aes(y = averageRevenue, fill = Group),
             position = "dodge") +
    scale_y_continuous(labels = ,
                       limits = c(0, max(temp.df_pred$averageRevenue)),
                       name = "Average Revenue (Euro)") +
    scale_x_discrete(name = "Deciles", limits = rep(1:10)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    theme(
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14), 
      axis.title=element_text(size=15,face="bold")
    ) +
    labs(
      title = paste("Average Revenue Per Decile -", t),
      subtitle = paste("Data:", n)
    ) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(filename=paste(n, t, arpd.title, ".png", sep="_"), 
         plot = arpd.plot, 
         device = png, 
         path = "final predictions/final plots/",
         dpi=320,
         scale = 50, 
         width = 15, 
         height = 10,
         units= c("in"),
         limitsize=FALSE)
  
  return(arpd.plot)
}

## UPLIFT PER DECILE
predUPD <- function(n, t) {
  
  perf_pred <- model.performance[[1]]

  temp.df_pred.uplift <-
    data.frame(Deciles = seq(1:10), Uplift = perf_pred[, 6] - perf_pred[, 7])
  
  upd.title <- "upd"
  upd.plot <- ggplot(temp.df_pred.uplift, aes(x = Deciles)) +
    geom_bar(stat = "identity", aes(y = Uplift, fill = "red")) +
    scale_y_continuous(limits = c(ifelse(
      min(temp.df_pred.uplift$Uplift) <= 0,
      min(temp.df_pred.uplift$Uplift),
      0
    ), max(temp.df_pred.uplift$Uplift)), name = "Uplift (Treatment - Control) in Average Spend") +
    scale_x_discrete(name = "Deciles", limits = rep(1:10)) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14), 
      axis.title=element_text(size=15,face="bold")
    ) +
    labs(title = paste("Uplift Per Decile -", t),
         subtitle = paste("Data:", n)) +
    theme(plot.title = element_text(face = "bold")) +
    guides(fill = FALSE)
  
  
  # +  
  #   xlab("Dataset") + ylab("Share of spend > 0")+
  #   theme(
  #     axis.line.x = element_blank(),
  #     axis.ticks.x = element_blank(),
  #     axis.text.x = element_text(size = 14),
  #     axis.text.y = element_text(size = 14), 
  #     axis.title=element_text(size=15,face="bold")
  #   )+
  #   geom_text(size = 4, position = position_stack(vjust = 0.5))
  
  ggsave(filename=paste(n, t, upd.title, ".png", sep="_"), 
         plot = upd.plot, 
         device = png, 
         path = "final predictions/final plots/",
         dpi=320,
         scale = 50, 
         width = 15, 
         height = 10,
         units= c("in"),
         limitsize=FALSE)
  
  return(upd.plot)
}

# QINI Score ---------------------------------------------------------------
# This function calculates the Qini score for our model 
predQini_score <- function(n, t) {
  
  perf_pred <- model.performance[[1]]
  
  groups = 10
  pred_r.cumul.y1_ct1 <- cumsum(perf_pred[, "n.y1_ct1"]) / cumsum(perf_pred[, "n.ct1"]) # Cumulative revenue per treated person  
  pred_r.cumul.y1_ct0 <- cumsum(perf_pred[, "n.y1_ct0"]) / cumsum(perf_pred[, "n.ct0"]) # Cumulative revene per controlled person
  deciles <- seq(1 / groups, 1, 1 / groups)
  pred_r.cumul.y1_ct1[is.na(pred_r.cumul.y1_ct1)] <- 0 # If they are missing set them to zero
  pred_r.cumul.y1_ct0[is.na(pred_r.cumul.y1_ct0)] <- 0
  pred_inc.gains = c(0.0, (pred_r.cumul.y1_ct1 - pred_r.cumul.y1_ct0) * deciles) # The incremental gains 
  pred_overall.inc.gains <- sum(perf_pred[, "n.y1_ct1"]) / sum(perf_pred[, "n.ct1"]) - sum(perf_pred[, "n.y1_ct0"]) / sum(perf_pred[, "n.ct0"]) # ATE
  pred_random.inc.gains <- c(0, cumsum(rep(pred_overall.inc.gains / groups, groups))) # A cumulation of equal random gains which end up at the ATE
  pred_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
  pred_y <- pred_inc.gains 
  pred_auuc <- 0
  pred_auuc.rand <- 0
  # calculating the Qini curve
  for (i in 2:length(pred_x)) {
    pred_auuc <-
      pred_auuc + 0.5 * (pred_x[i] - pred_x[i - 1]) * (pred_y[i] + pred_y[i-1])
  }
  # calculating the random curve
  pred_y.rand <- pred_random.inc.gains
  for (i in 2:length(pred_x)) {
    pred_auuc.rand <-
      pred_auuc.rand + 0.5 * (pred_x[i] - pred_x[i - 1]) * (pred_y.rand[i] + pred_y.rand[i -
                                                                                           1])
  }
  # calculating the Qini
  qini.matrix[t, which(names(qini.matrix) %in% paste(n))] <-
    pred_auuc - pred_auuc.rand
  return(qini.matrix)
}

# QINI Plot ---------------------------------------------------------------
# Function to plot the Qini curve 
predQini_plot <- function(n,t){

  perf_pred <- model.performance[[1]]
  
  groups = 10
  pred_r.cumul.y1_ct1 <-
    cumsum(perf_pred[, "n.y1_ct1"]) / cumsum(perf_pred[, "n.ct1"])
  pred_r.cumul.y1_ct0 <-
    cumsum(perf_pred[, "n.y1_ct0"]) / cumsum(perf_pred[, "n.ct0"])
  deciles <- seq(1 / groups, 1, 1 / groups)
  pred_r.cumul.y1_ct1[is.na(pred_r.cumul.y1_ct1)] <- 0
  pred_r.cumul.y1_ct0[is.na(pred_r.cumul.y1_ct0)] <- 0
  pred_inc.gains = c(0.0, (pred_r.cumul.y1_ct1 - pred_r.cumul.y1_ct0) * deciles)
  pred_overall.inc.gains <-
    sum(perf_pred[, "n.y1_ct1"]) / sum(perf_pred[, "n.ct1"]) - sum(perf_pred[, "n.y1_ct0"]) / sum(perf_pred[, "n.ct0"])
  pred_random.inc.gains <-
    c(0, cumsum(rep(pred_overall.inc.gains / groups, groups)))
  qini.title <- "qini"
  pred_miny <- 100 * min(c(pred_random.inc.gains, pred_inc.gains))
  pred_maxy <- 100 * max(c(pred_random.inc.gains, pred_inc.gains))
  
  qini.perc <-
    data.frame(
      "model inc gains" = pred_inc.gains * 100,
      "decile" = c(0, seq(100 / groups, 100, 100 / groups)),
      "random inc gains" = pred_random.inc.gains * 100
    )
  qini.perc <- reshape2::melt(qini.perc, id.var = "decile")
  
 qini.plot <-  ggplot(qini.perc, aes(decile, value, color = variable)) +
    geom_line(aes(linetype = variable)) +
    geom_point(data = qini.perc[which(qini.perc$variable %in% c("model.inc.gains")), ], aes(decile, value, color =
                                                                                              variable)) +
    scale_color_manual(values = c("blue", "red")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    xlab("Proportion of population targeted (%)") +
    ylab("Cumulative incremental gains (pc pt)") +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    scale_y_continuous(breaks = seq(0, max(qini.perc$value), 10)) +
    theme(
      legend.position = c(0.95, 0.05),
      legend.justification = c("right", "bottom")
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank()
    ) +
    theme(
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14), 
      axis.title=element_text(size=15,face="bold")
    ) +
    labs(title = paste("Qini Curve - ", t),
         subtitle = paste("Data:", n)) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(filename=paste(n, t, qini.title, ".png", sep="_"), 
         plot = qini.plot, 
         device = png, 
         path = "final predictions/final plots/",
         dpi=320,
         scale = 50, 
         width = 15, 
         height = 10,
         units= c("in"),
         limitsize=FALSE)
  
  return(qini.plot)
}

# Function Profit ----------------------------------------------------------------
# Function to calculate the campaign profits
predProfit <- function(n,t,t.d,p) {

  mm_pred <- model.performance[[2]]
  
  t.d$uplift <- p
  #the eligibility rule for those who are newly eligible as per our model
  t.d$m.eligibility[t.d$eligibility==0] <- ifelse(((t.d$uplift[t.d$eligibility==0] + 
                                                      t.d$checkoutAmount[t.d$eligibility==0]) >= t.d$campaignMov[t.d$eligibility==0] / 100),1,0)
  
  t.d$m.eligibility[t.d$eligibility==1] <- ifelse(((t.d$uplift[t.d$eligibility==1] + 
                                                      t.d$checkoutAmount[t.d$eligibility==1] + 
                                                      t.d$ExpectedDiscount[t.d$eligibility==1]) >= t.d$campaignMov[t.d$eligibility==1] / 100),1,0)
  
  #the eligibility rule for those who are eligible as per our model but were also eligible before
  t.d$m.ExpectedDiscount <- numeric(nrow(t.d))
  t.d$m.ExpectedDiscount[t.d$campaignUnit=="CURRENCY"&t.d$m.eligibility==1] <-
    t.d$campaignValue[t.d$campaignUnit=="CURRENCY"&t.d$m.eligibility==1]/100
  
  # for those newly eligible we take the checkoutAmount (which is revenue), 
  # add the treatment effect, and compute the discount they receive as percentage of their revenue
  t.d$m.ExpectedDiscount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==0] <-
    ((t.d$checkoutAmount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==0] + # (checkoutAmount + uplift (predicted)) * campaignValue
        t.d$uplift[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==0])*
       (t.d$campaignValue[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==0]/10000))
  
  # for those already eligible we set the expected Discount as per our Model differently:
  # since the checkoutAmount for those sessions has already internalized treatment cost, 
  # we add the estimated ExpectedDiscount to the checkoutAmount as well as the Uplift as predicted by the model
  # This is not entirely correct, but otherwise we would compute a discount on top of a discount, 
  # since people who were eligible in the original campaign have cost internalized.
  t.d$m.ExpectedDiscount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==1] <- 
    ((t.d$checkoutAmount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==1] 
      + t.d$ExpectedDiscount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==1]
      + t.d$uplift[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==1])
     * (t.d$campaignValue[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1&t.d$eligibility==1]/10000))
  
  # t.d$m.ExpectedDiscount[t.d$campaignUnit=="CURRENCY"&t.d$m.eligibility==1] <- # for those newly eligible
  # t.d$campaignValue[t.d$campaignUnit=="CURRENCY"&t.d$m.eligibility==1]/100
  # 
  # t.d$m.ExpectedDiscount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1] <- 
  #   ((t.d$checkoutAmount[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1] + 
  #       t.d$uplift[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1])*(t.d$campaignValue[t.d$campaignUnit=="PERCENT"&t.d$m.eligibility==1]/10000))
  # 
  
  mm_pred = cbind(
    mm_pred,
    eligibility = t.d$eligibility,
    expectedDiscount = t.d$ExpectedDiscount,
    m.eligible = t.d$m.eligibility,
    m.expectedDiscount <- t.d$m.ExpectedDiscount)# model eligible
  
  
  pred_n.y1_ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], sum)  # Sum of revenue of people not having received the treatment
  pred_n.y1_ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], sum)  # Sum of revenue of people having received the treatment
  pred_r.y1_ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], mean) # Average revenue of people not having received the treatment
  pred_r.y1_ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], mean) # Average revenue of people having received the treatment
  pred_n.ct0 <- tapply(mm_pred[mm_pred[, 3] == 0,][, 2], mm_pred[mm_pred[, 3] == 0,][, 5], length)  # Sum of people not having received the treatment
  pred_n.ct1 <- tapply(mm_pred[mm_pred[, 3] == 1,][, 2], mm_pred[mm_pred[, 3] == 1,][, 5], length)  # Sum of people having received the treatment
  
  # In rare situations the ratio of a group can be non-existing because there are no people in the treatment or control group.
  # We set these to 0.
  pred_r.y1_ct0 <- ifelse(is.na(pred_r.y1_ct0), 0, pred_r.y1_ct0)
  pred_r.y1_ct1 <- ifelse(is.na(pred_r.y1_ct1), 0, pred_r.y1_ct1)
  
  uplift = pred_r.y1_ct1 - pred_r.y1_ct0 #uplift per decile in the old campaign
  
  pred_n.ct1.eligible <- tapply(mm_pred[mm_pred[, 6] == 1,][, 6],
                                mm_pred[mm_pred[, 6] == 1,][, 5], length)  # no of eligible customers per decile in the initial campaign
  
  # model features
  pred_n.m.eligible = tapply(mm_pred[mm_pred[, 8] == 1,][, 8],
                             mm_pred[mm_pred[, 8] == 1,][, 5], length) # number of eligible customers in the model campaign (model campaign := campaign according to our model)

  #helper function that adds columns in case last deciles are left empty
  if(length(pred_n.m.eligible)<10){
    length <- (length(pred_n.m.eligible)+1)
    for(i in c(length:10)){
      pred_n.m.eligible[i] <- 0
    }
  }
  
  delta.eligible <- pred_n.m.eligible - pred_n.ct1.eligible
  
  #length(pred_n.m.eligible)
  
  m.cost = tapply(mm_pred[mm_pred[, 8] == 1,][, 9], #give me all rows of column 7 (expectedDiscount) where model.eligible == 1
                  mm_pred[mm_pred[, 8] == 1,][, 5], sum) #group them by decile and compute the sum for each decile
  
  #helper function that adds columns in case last deciles are left empty
  if(length(m.cost)<10){
    length <- (length(m.cost)+1)
    for(i in c(length:10)){
      m.cost[i] <- 0
    }
  }
  
  campaign.cost <- tapply(mm_pred[mm_pred[, 6] == 1,][, 7], #give me all rows of column 7 (expectedDiscount) where model.eligible == 1
                                   mm_pred[mm_pred[, 6] == 1,][, 5], sum)

  if(length(campaign.cost)<10){
    length <- (length(campaign.cost)+1)
    for(i in c(length:10)){
      campaign.cost[i] <- 0
    }
  }
  delta.cost <- m.cost - campaign.cost # additional cost 
  delta.revenue <- pred_n.ct0 * uplift #additional money earned by treating everyone in each decile
  delta.profit = delta.revenue - delta.cost # profit generated by our model camapign if we treated overyone in each decile
  
  # function to assess profit according to the cumulated no. of deciles treated from our model
  profit_decile  = function(x) {
    if (x <= 9) {
      sum(delta.profit[1:x]) + sum(delta.profit[(x + 1):10] * (-1))
    } else{
      sum(delta.profit[1:x])
    }
  }
  profit_per_deciles_treated = numeric(10)
  
  for (i in seq_along(1:10)) {
    profit_per_deciles_treated[i] <-
      profit_decile(i) # saving the cumulated profits for deciless treated
  }
  profit_per_deciles_treated <- cbind(c(1:10), profit_per_deciles_treated)
  
  
  max.profit.model[t, which(names(max.profit.model) %in% paste(n))] = as.character(paste(
    "Decile:",profit_per_deciles_treated[,1][which.max(profit_per_deciles_treated[,2])],
    "|","Profit:", format(round(max(
      profit_per_deciles_treated[,2]),2), 
      nsmall = 2),sep = " "))
  
  #ROI per deciles treated
  ROI_per_deciles_treated = cbind(c(1:10), profit_per_deciles_treated[,2]/cumsum(m.cost))
  ROI_per_deciles_treated
  
  #decile to treat
  treatment.decision[t, which(names(treatment.decision) %in% paste(n))] <- as.character(paste(
    "Decile:",ROI_per_deciles_treated[,1][which.max(ROI_per_deciles_treated[,2])],
    "|","ROI:", format(round(max(
      ROI_per_deciles_treated[,2]),2), 
      nsmall = 2),sep = " "))
  
  
  profitability <- cbind(
    group   =        1:10,
    n.ct1    =       pred_n.ct1,
    n.ct0    =       pred_n.ct0,
    uplift   =       uplift,
    eligible =       pred_n.ct1.eligible,
    #real.Cost =      pred_campaign_cost,
    #real.Rev =       pred_campaign_revenue,
    #real.Profit  =   pred_campaign_profit,
    delta.eligible = delta.eligible,
    delta.Cost =     delta.cost,
    delta.Rev =      delta.revenue,
    delta.Profit =   delta.profit,
    delta.cum_Profit_tr = profit_per_deciles_treated[,2],
    delta.cum_ROI_tr = ROI_per_deciles_treated[,2]
  )
  
  return(list(max.profit.model,treatment.decision))
}

