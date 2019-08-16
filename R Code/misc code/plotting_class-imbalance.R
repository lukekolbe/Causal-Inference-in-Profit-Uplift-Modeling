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
ggplot(gg, aes(fill=Spending, y=Share, x=Dataset, , label=Percent)) + 
  geom_bar(stat="identity", position="fill") + ggtitle("Share of positive revenue cases") + geom_text(size = 5, position = position_stack(vjust = 0.5)) +scale_fill_manual(values = c("grey","red"))

ggplot(ts, aes(z, x, fill=factor(y, levels=c("blue","white" )))) + geom_bar(stat = "identity")
ggplot(ts, aes(z, x, fill=factor(y, levels=c("white", "blue")))) + geom_bar(stat = "identity")




