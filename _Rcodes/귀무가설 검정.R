setwd("c:/Rdata")
getwd()
sales1=read.csv("sales1.csv")
head(sales1)
colnames(sales1)
class(sales1)
sales1$매출=sales1$QTY*sales1$PRICE

#############################################################################
# 칼럼이름
# YM,CATEGORY,ITEM_CNT,QTY,PRICE,MAXTEMP,SALEDAY,RAIN_DAY,HOLIDAY,Year,매출 
# 년월,품목,아이템수,판매량,가격,최대기온,판매일수,강우일수,휴일수,매출   
# 
# 귀무가설(판매량) (종속변수 : 판매량) 
# 1.최대기온이 높을수록 음료 판매량이 많을 것이다. (여름철)
# 2.비가 온날엔 판매량이 낮을것이다.
# 3.휴일수가 많은 달엔 음료 판매량이 많을 것이다.
# 4.영업일수가 많은 달엔 판매량이 많을 것이다.
# 
# 귀무가설(가격) (종속변수 : 가격)
# 1.최대기온이 높을수록 음료 가격은 높아진다.
# 2.년,월,분기별로 음료 가격은 높아진다
# 3.판매일수, 강우일수, 휴일이 많은 달엔 음료 가격에 영향을 미친다.
# 
# 귀무가설(매출) (종속변수 : 판매량*가격)
# 
# 귀무가설VS대립가설
# 판매량과 가격은 비례한다 VS 비례하지않는다
###############################################################################

#1.최대기온이 높을수록 음료 판매량이 많을 것이다.
test1=lm(log(sales1$QTY)~MAXTEMP,data=sales1)
summary(test1) #Multiple R-squared:  0.3433

# 2.비가 온날엔 판매량이 낮을것이다.
test2=lm(log(sales1$QTY)~RAIN_DAY,data=sales1)
summary(test2) #Multiple R-squared:  0.267

# 3.휴일수가 많은 달엔 음료 판매량이 많을 것이다.
test3=lm(log(sales1$QTY)~HOLIDAY,data=sales1)
summary(test3) #Multiple R-squared:  0.01951
# 관련없음(기각)

# 4.영업일수가 많은 달엔 판매량이 많을 것이다.
test4=lm(log(QTY)~SALEDAY,data=sales1)
summary(test4) #Multiple R-squared:  0.5877

# 1.최대기온이 높을수록 음료 가격은 높아진다. 
test5=lm(PRICE~MAXTEMP,data=sales1)
summary(test5) #Multiple R-squared:  0.002038
# 기온과 음료의 가격은 영향없음

# 2.년,월,분기별로 음료 가격은 높아진다(월, 분기 칼럼 없음)
test6=lm(PRICE~Year,data=sales1)
summary(test6) #Multiple R-squared:  0.766
# 년도와 음료가격은 관련있음

#test7=lm(sales1$PRICE~MONTH,data=sales1)
#test8=lm(sales1$PRICE~QUAT,data=sales1)

#3.판매일수, 강우일수, 휴일이 많은 달엔 음료 가격에 영향을 미친다.
test9=lm(PRICE~SALEDAY,data=sales1)
summary(test9) #Multiple R-squared:  0.7909
# 관련있음

test10=lm(PRICE~RAIN_DAY,data=sales1)
summary(test10)
# 관련없음

test11=lm(PRICE~HOLIDAY,data=sales1)
summary(test11)
# 관련없음

# 판매량과 가격은 비례한다 VS 반비례한다
test12=lm(log(QTY)~PRICE,data=sales1)
summary(test12) #Multiple R-squared:  0.4306

test13=lm(PRICE~log(QTY),data=sales1)
summary(test13) #Multiple R-squared:  0.4306

cor(log(sales1$QTY),sales1$PRICE)
plot(log(QTY)~PRICE,data=sales1)
abline(test12,col=2)

##########################################################
#검정결과
# 귀무가설(판매량)
# 1.최대기온과 판매량에 영향을 미친다.
# 2.비가 온날엔 판매량에 영향을 미친다.
# 3.영업일수가 많은 달엔 판매량에 영향을 미친다.

# 귀무가설(가격) (종속변수 : 가격) #종속변수의 P-value값이 낮기 때문에 신뢰하기 어려움
# 1.YEAR와 가격은 관계가 있다.
# 2.SALEDAY(영업일수) 많은 달엔 음료 가격에 관련이 있다.

# 판매량과 가격
# log(판매량)과 가격은 양의 상관관계를 갖는다.
# 판매량과 가격은 비례한다
##########################################################
