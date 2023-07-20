setwd("c:/Rdata")
getwd()
sales1=read.csv("sales1.csv")
head(sales1)
colnames(sales1)
class(sales1)
sales1$매출=sales1$QTY*sales1$PRICE


test12=lm(log(QTY)~PRICE,data=sales1)
summary(test12) #Multiple R-squared:  0.4306

dev.off()
par(mfrow=c(2,2))
plot(lm(log(QTY)~PRICE,data=sales1))

plot(test12$resid)

