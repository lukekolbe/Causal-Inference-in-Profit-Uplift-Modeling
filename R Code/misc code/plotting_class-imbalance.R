library(ggplot2)

# create a dataset
specie=c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition=rep(c("normal" , "stress" , "Nitrogen") , 4)
value=abs(rnorm(12 , 0 , 15))
data=data.frame(specie,condition,value)

plot(f_a$checkoutAmount!=0)
prop.table(table(f_a$checkoutAmount!=0))
prop.table(table(f_b$checkoutAmount!=0))
prop.table(table(b_t$checkoutAmount!=0))
prop.table(table(hllstrm$spend!=0))


library(scales)
gg <- read.csv2("/Users/lukaskolbe/Library/Mobile Documents/com~apple~CloudDocs/UNI/Master/Applied Predictive Analytics/Data/conversion_shares.csv")
str(gg)

gg$Percent <- paste0(round(gg$Share*100, digits = 1), '%')

# Stacked Percent
spend.prctg <- ggplot(gg, aes(fill=Spending, y=Share, x=Dataset, label=Percent)) + 
  geom_bar(stat="identity", position="fill") + 
  labs(title = "Share of cases with positive revenue",
       subtitle = "percentage of people who spend money") +
  theme(plot.title = element_text(face = "bold")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )+theme(
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15)
  )+
  geom_text(size = 4, position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("grey","red"))

ggsave(filename="spend_percentage2.png", 
       plot = spend.prctg, 
       device = png, 
       path = "final predictions/final plots/",
       dpi=300,
       scale = 50, 
       width = 15, 
       height = 10,
       units= c("in"),
       limitsize=FALSE)
