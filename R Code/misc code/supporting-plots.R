source("R Code/misc code/load-packages.R")


# CLASS IMBALANCE ---------------------------------------------------------

gg <- read.csv2("conversion_shares.csv")
str(gg)

gg$Percent <- paste0(round(gg$Share*100, digits = 1), '%')

# Stacked Percent

spend.prctg <- ggplot(gg, aes(fill=Spending, y=Share, x=Dataset, label=Percent)) + 
  geom_bar(stat="identity", position="fill") + 
  labs(title = "Share of cases with positive revenue",
       subtitle = "Percentage of people who spend money") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.text=element_text(size=10))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )+  
  xlab("Dataset") + ylab("Share of spend > 0")+
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14), 
    axis.title=element_text(size=15,face="bold")
  )+
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("grey","red"))

# png(filename = "spend_percentage3.png",
#     width = 750, height = 500, units = "px", pointsize = 12,
#     bg = "white", res = NA, family = "", restoreConsole = TRUE,
#     type = "Xlib",)
# dev.off()

ggsave(filename="spend_percentage.png", 
       plot = spend.prctg, 
       device = png, 
       path = "final predictions/final plots/",
       dpi=320,
       scale = 50, 
       width = 15, 
       height = 10,
       units= "in",
       limitsize=FALSE)

# AVERAGE TREATMENT EFFECTS -----------------------------------------------

f_b <- read.csv("working data/Datasets/FashionB.csv", sep=",")
f_a <- read.csv("working data/Datasets/FashionA.csv", sep=",")
b_t <- read.csv("working data/Datasets/BooksAndToys.csv", sep=",")
hllstrm <- read.csv("working data/Datasets/hillstrm.csv", sep=",")


ate <- function(x){
  cA.tr <- mean(x$checkoutAmount[x$controlGroup==0])
  cA.ct <- mean(x$checkoutAmount[x$controlGroup==1])
  return(list(cA.tr, cA.ct))
}

f_a.ate <- ate(f_a)
f_b.ate <- ate(f_b)
b_t.ate <- ate(b_t)


hllstrm$checkoutAmount <- hllstrm$spend
hllstrm$controlGroup <- ifelse(hllstrm$segment=="No E-Mail", 1, 0)
h_s.ate <- ate(hllstrm)


data.zoo <- c("FashionA", "FashionB", "BooksToys", "Hillstrom")
group.spend <- c("Dataset","Treatment Spend","Control Spend")

imbalance <- setNames(data.frame(matrix(ncol = 3, nrow = 4)), group.spend)
rownames(imbalance) <- data.zoo

imbalance$Dataset <- data.zoo
imbalance$`Treatment Spend` <- c(f_a.ate[[1]],f_b.ate[[1]],b_t.ate[[1]], h_s.ate[[1]])
imbalance$`Control Spend` <- c(f_a.ate[[2]],f_b.ate[[2]],b_t.ate[[2]], h_s.ate[[2]])


data.m <- melt(imbalance, id.vars='Dataset')

ate.plot <- ggplot(data.m, aes(Dataset, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")+
  labs(title = "Average Treatment Effects (ATE) across datasets",
       subtitle = "Mean spending in treatment and target groups") +
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.text=element_text(size=10))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )+  
  xlab("Dataset") + 
  ylab("Mean Spend per Treatment Group") +
  theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14), 
    axis.title=element_text(size=14,face="bold"))

ggsave(filename="ate-plot.png", 
       plot = ate.plot, 
       device = png, 
       path = "final predictions/final plots/",
       dpi=320,
       scale = 50, 
       width = 15, 
       height = 10,
       units= "in",
       limitsize=FALSE)



table(b_t$campaignValue, b_t$campaignUnit)
table(f_a$campaignValue)
table(f_a$campaignMov[f_a$campaignValue==2000])

mean(f_a$checkoutAmount[f_a$controlGroup==0&f_a$campaignValue==500]) - mean(f_a$checkoutAmount[f_a$controlGroup==1&f_a$campaignValue==500])
mean(f_a$checkoutAmount[f_a$controlGroup==0&f_a$campaignValue==2000]) - mean(f_a$checkoutAmount[f_a$controlGroup==1&f_a$campaignValue==2000])
