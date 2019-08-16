# Function Evaluation setup ----------------------------------------------------------------
predEval <- function(t.d, p){
  
  # We rank the uplift scores from high to low and add this infromation to a dataframe
  mm_pred = cbind(
    uplift = p,
    target = t.d$checkoutAmount,
    treatment = t.d$treatment,
    uplift_rank = length(p) + 1 - rank(p, ties.method = "random")
  )
  
  groups = 10
  bk_pred = unique(quantile(mm_pred[, 4], probs = seq(0, 1, 1 / groups)))
  
  if ((length(bk_pred) - 1) != groups) {
    warning (
      "uplift: due to many ties in uplift predictions, the ties will be dealt with randomly",
      groups
    )
  }
  mm_pred = cbind(mm_pred,
                  decile = cut(
                    mm_pred[, 4],
                    breaks = bk_pred,
                    labels = NULL,
                    include.lowest = T
                  ))
  
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
  
  uplift = pred_r.y1_ct1 - pred_r.y1_ct0
  
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
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
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
         dpi=300,
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
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    labs(title = paste("Uplift Per Decile -", t),
         subtitle = paste("Data:", n)) +
    theme(plot.title = element_text(face = "bold")) +
    guides(fill = FALSE)
  
  ggsave(filename=paste(n, t, upd.title, ".png", sep="_"), 
         plot = upd.plot, 
         device = png, 
         path = "final predictions/final plots/",
         dpi=300,
         scale = 50, 
         width = 15, 
         height = 10,
         units= c("in"),
         limitsize=FALSE)
  
  return(upd.plot)
}

# QINI Score ---------------------------------------------------------------
predQini_score <- function(n, t) {
  
  perf_pred <- model.performance[[1]]
  
  groups = 10
  pred_r.cumul.y1_ct1 <- cumsum(perf_pred[, "n.y1_ct1"]) / cumsum(perf_pred[, "n.ct1"])
  pred_r.cumul.y1_ct0 <- cumsum(perf_pred[, "n.y1_ct0"]) / cumsum(perf_pred[, "n.ct0"])
  deciles <- seq(1 / groups, 1, 1 / groups)
  pred_r.cumul.y1_ct1[is.na(pred_r.cumul.y1_ct1)] <- 0
  pred_r.cumul.y1_ct0[is.na(pred_r.cumul.y1_ct0)] <- 0
  pred_inc.gains = c(0.0, (pred_r.cumul.y1_ct1 - pred_r.cumul.y1_ct0) * deciles)
  pred_overall.inc.gains <- sum(perf_pred[, "n.y1_ct1"]) / sum(perf_pred[, "n.ct1"]) - sum(perf_pred[, "n.y1_ct0"]) / sum(perf_pred[, "n.ct0"])
  pred_random.inc.gains <- c(0, cumsum(rep(pred_overall.inc.gains / groups, groups)))
  pred_x <- c(0.0, seq(1 / groups, 1, 1 / groups))
  pred_y <- pred_inc.gains
  pred_auuc <- 0
  pred_auuc.rand <- 0
  for (i in 2:length(pred_x)) {
    pred_auuc <-
      pred_auuc + 0.5 * (pred_x[i] - pred_x[i - 1]) * (pred_y[i] + pred_y[i-1])
  }
  pred_y.rand <- pred_random.inc.gains
  for (i in 2:length(pred_x)) {
    pred_auuc.rand <-
      pred_auuc.rand + 0.5 * (pred_x[i] - pred_x[i - 1]) * (pred_y.rand[i] + pred_y.rand[i -
                                                                                           1])
  }
  
  qini.matrix_28[n, which(names(qini.matrix_28) %in% paste(t))] <-
    pred_auuc - pred_auuc.rand
  return(qini.matrix_28)
}

# QINI Plot ---------------------------------------------------------------
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
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    labs(title = paste("Qini Curve - ", t),
         subtitle = paste("Data:", n)) +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(filename=paste(n, t, qini.title, ".png", sep="_"), 
         plot = qini.plot, 
         device = png, 
         path = "final predictions/final plots/",
         dpi=300,
         scale = 50, 
         width = 15, 
         height = 10,
         units= c("in"),
         limitsize=FALSE)
  
  return(qini.plot)
}

# Function Profit ----------------------------------------------------------------
predProfit <- function(n,t,t.d,p) {

  mm_pred <- model.performance[[2]]
  
  mm_pred = cbind(
    mm_pred,
    eligibility = t.d$eligibility,
    expectedDiscount = t.d$ExpectedDiscount,
    m.eligible = ifelse((
      p + t.d$checkoutAmount + t.d$ExpectedDiscount) >= t.d$campaignMov / 100,1,0))# model eligible
  
  
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
  
  uplift = pred_r.y1_ct1 - pred_r.y1_ct0
  
  pred_n.ct1.eligible <- tapply(mm_pred[mm_pred[, 6] == 1,][, 6],
                                mm_pred[mm_pred[, 6] == 1,][, 5], length)  # Sum of people having received the treatment
  
  # additional costs by treating customers, here handing out 20€ vouchers
  pred_campaign_cost <-
    tapply(mm_pred[mm_pred[, 6] == 1,][, 7], #give me all rows of column 7 (expectedDiscount) where eligible == 1
           mm_pred[mm_pred[, 6] == 1,][, 5], sum) #group them by decile and compute the sum for each decile
  #pred_campaign_cost = pred_n.ct1.eligible[,1] * test.data$ExpectedDiscount
  
  # ADDITIONAL revenue generated by the campaign as per our model
  ### CAREFUL!!! shouldn't the revenue be the checkoutAmount + expectedDiscount of each person? >> NO! because we are looking at additional revenue, NOT total revenue
  # for cases where a discount was used, the cost is internalized: these rows have PROFIT.
  # Rows where no discount was used, checkoutAmount is REVENUE.
  # pred_campaign_revenue <- tapply(mm_pred[mm_pred[, 3]== 1, ][, 1], #give me all rows of column 1 (uplift) where treatment == 1
  #                              mm_pred[mm_pred[, 3] == 1, ][, 5], sum) #group them by decile and compute the sum for each decile
  
  pred_campaign_revenue = pred_n.ct1 * pred_r.y1_ct1 - pred_r.y1_ct0
  #pred_campaign_revenue = pred_n.ct1 * df_pred$uplift
  
  pred_campaign_profit = pred_campaign_revenue - pred_campaign_cost # difference between additional campaign reveue and additional camapign costs
  
  # model features
  pred_n.m.eligible = tapply(mm_pred[mm_pred[, 8] == 1,][, 8],
                             mm_pred[mm_pred[, 8] == 1,][, 5], length) # number of eligible customers in the model campaign (model campaign := campaign according to our model)
  
  #helper function that adds columns in case last deciles are left empty
  if(length(pred_n.m.eligible)<10){
    for(i in seq_along(1:(10-length(pred_n.m.eligible)))){
      pred_n.m.eligible[length(pred_n.m.eligible)+i] <- 0
     }
  }
  
  #length(pred_n.m.eligible)
  
  m.cost = tapply(mm_pred[mm_pred[, 8] == 1,][, 7], #give me all rows of column 7 (expectedDiscount) where model.eligible == 1
                  mm_pred[mm_pred[, 8] == 1,][, 5], sum) #group them by decile and compute the sum for each decile
  
  #helper function that adds columns in case last deciles are left empty
  if(length(m.cost)<10){ 
    for(i in seq_along(1:(10-length(m.cost)))){
      m.cost[length(m.cost)+i] <- 0
    }
  }
  
  length(m.cost)
  #m.cost = pred_n.m.eligible * f_a.test$expectedDiscount # costs generated by our model campaign if we treated everyone in each decile
  
  ##CAREFUL! SHOULDN'T M.REVENUE BE ALL PEOPLE * THEIR INDIVIDUAL REVENUE + UPLIFT - "M.EXPECTED.DISCOUNT"???
  # OR SHOULD THIS INDEED BE "EXTRA REVENUE GENERATED BY OUR MODEL CAMPAIGN IF WE TREATED EVERYONE?
  #m.revenue = (pred_n.ct1+pred_n.ct0) * mm_pred$uplift # revenue generated by our model campaign if we treated everyone in each decile
  #m.revenue = tapply(mm_pred[mm_pred[, 8]== 1, ][, 1], #give me all rows of column 7 (expectedDiscount) where model.eligible == 1
  # mm_pred[mm_pred[, 8] == 1, ][, 5], sum)
  
  m.revenue <- (pred_n.ct1 + pred_n.ct0) * (pred_r.y1_ct1 - pred_r.y1_ct0)
  m.profit = m.revenue - m.cost # profit generated by our model camapign if we treated overyone in each decile
  
  # function to assess profit according to the cumulated no. of deciles treated from our model
  profit_decile  = function(n) {
    if (n <= 9) {
      sum(m.profit[1:n]) + sum(m.profit[(n + 1):10] * (-1))
    } else{
      sum(m.profit[1:n])
    }
  }
  profit_per_deciles_treated = numeric(10)
  
  for (i in seq_along(1:10)) {
    profit_per_deciles_treated[i] <-
      profit_decile(i) # saving the cumulated profits for deciless treated
  }
  profit_per_deciles_treated
  
  max.profit.model[n, which(names(treatment.decision) %in% paste(t))] = max(profit_per_deciles_treated) # saving the max profit and its index in a vector
  
  
  #ROI per deciles treated
  ROI_per_deciles_treated = cbind(c(1:10), profit_per_deciles_treated/cumsum(m.cost))
  ROI_per_deciles_treated
  
  #decile to treat
  #which(names(qini.matrix_28) %in% paste(t))
  treatment.decision[n, which(names(treatment.decision) %in% paste(t))] <- as.character(paste(
    "Decile:",ROI_per_deciles_treated[,1][which.max(ROI_per_deciles_treated[,2])],
    "|","ROI:", format(round(max(
      ROI_per_deciles_treated[,2]),2), 
      nsmall = 2),sep = " "))
  
  profitability <- cbind(
    group   =  1:10,
    n.ct1    =       pred_n.ct1,
    n.ct0    =       pred_n.ct0,
    uplift   =       uplift,
    eligible =       pred_n.ct1.eligible,
    real.Cost =      pred_campaign_cost,
    real.Rev =       pred_campaign_revenue,
    real.Profit  =   pred_campaign_profit,
    model.eligible = pred_n.m.eligible,
    model.Cost =     m.cost,
    model.Rev =      m.revenue,
    model.Profit =   m.profit,
    model.cum_Profit_tr = profit_per_deciles_treated,
    model.cum_ROI_tr = ROI_per_deciles_treated
  )
  
  return(list(max.profit.model,treatment.decision))
}

