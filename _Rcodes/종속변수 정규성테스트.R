#종속변수 정규성확인
setwd("c:/Rdata")
getwd()
sales1=read.csv("sales1.csv")
sales1
##QTY 판매량
shapiro.test(sales1$QTY) # 기각 p-value = 0.01071
shapiro.test(log(sales1$QTY)) #채택 p-value = 0.6638
shapiro.test(sqrt(sales1$QTY)) #기각 p-value = 0.2822


##PRICE 가격
shapiro.test(sales1$PRICE) #p-value = 5.329e-05
shapiro.test(log(sales1$PRICE))#p-value = 0.0001204
shapiro.test(sqrt(sales1$PRICE))#p-value = 8.12e-05
#PRICE는 정규성을 갖지않음

##매출
sales1$매출=sales1$QTY*sales1$PRICE
shapiro.test(sales1$매출) # 기각 p-value = 0.001464
shapiro.test(log(sales1$매출)) #채택  p-value = 0.4561
shapiro.test(sqrt(sales1$매출)) # 기각 p-value = 0.08263

##람다식(QTY)
result <- shapiro.test(sales1$QTY)
statistic <- result$statistic
p_value <- result$p.value

print(paste("Statistic:", statistic))
print(paste("p-value:", p_value))
##람다식(PRICE)
result <- shapiro.test(sales1$PRICE)
statistic <- result$statistic
p_value <- result$p.value

print(paste("Statistic:", statistic))
print(paste("p-value:", p_value))

##람다식(매출)
result <- shapiro.test(sales1$매출)
statistic <- result$statistic
p_value <- result$p.value

print(paste("Statistic:", statistic))
print(paste("p-value:", p_value))

