setwd("c:/Rdata")
getwd()
sales1=read.csv("sales1.csv")
head(sales1)
colnames(sales1)
class(sales1)
sales1$매출=sales1$QTY*sales1$PRICE
# model <- lm(y ~ x1 + x2 + x3, data = mydata)
# both=step(out,direction="both",trace=FALSE) stepwise selection
# out=lm(rating~.,data=attitude) backward selection

#판매량에 따른 다중선형회귀모델
model=lm(log(QTY)~MAXTEMP+RAIN_DAY+SALEDAY,data=sales1)
summary(model)
#회귀식 log(QTY) = 6.989 + 0.02047 * MAXTEMP - 0.000001666 * RAIN_DAY + 0.00000595 * SALEDAY

#가격에 따른 다중선형회귀모델
model2=lm(PRICE~Year+SALEDAY,data=sales1)
summary(model2)
#회귀식 PRICE = -55680 + 28.14 * Year + 0.00216 * SALEDAY

#매출에 따른 다중선형회귀모델



