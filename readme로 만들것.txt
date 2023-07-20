setwd("c:/Rdata")
getwd()

sales1=read.csv("sales1.csv",fileEncoding="CP949")
head(sales1)
colnames(sales1)
class(sales1)

plot(sales1)
#종속변수 QTY(매출)과 모든 독립변수들을 포함한 모형
model=lm(log(QTY)~YM+ITEM_CNT+PRICE+MAXTEMP+SALEDAY+RAIN_DAY+HOLIDAY
         +Year+MONTH+Quart,data=sales1)
summary(model)

anova(model)

backward=step(model,direction="backward",trace=FALSE)
backward$anova

both=step(model,direction="both",trace=FALSE)
both$anova
#month,ym,holiday,rain_day를 빼야 최적의 모형이됨 
#month,ym,holiday,rain_day 포함된 경우 Adjusted R-squared:  0.9222
#month,ym,holiday,rain_day 미포함한 경우 Adjusted R-squared:  0.9226


model=lm(log(QTY)~PRICE+MAXTEMP+SALEDAY+
         +Year+Quart,data=sales1)
summary(model)

#다중 공선성확인
pairs(sales1[,c("PRICE","MAXTEMP","SALEDAY","Year","Quart")])
cor(sales1[,c("PRICE","MAXTEMP","SALEDAY","Year","Quart")])

model=lm(log(QTY)~PRICE+MAXTEMP+SALEDAY+Year+Quart,data=sales1)
summary(model)
#Adjusted R-squared:  0.9226 
model=lm(log(QTY)~PRICE+MAXTEMP+Year+Quart,data=sales1)
summary(model)
#Adjusted R-squared:  0.9169 

model3=lm(log(QTY)~Quart,data=sales1)
summary(model3)
#SALEDAY를 제거하는 이유 
#1. 독립변수 Quart 와 month와 높은 상관계수를 갖기때문에
#해당 모형을 설명하기 어려움
#2. 예측값이 너무 낮기 때문에 종속변수 QTY의 관계를 설명하기 어려움
#3. 낮은 상관성을 보임임
pairs(sales1[,c("PRICE","MAXTEMP","Year","Quart")])
cor(sales1[,c("PRICE","MAXTEMP","Year","Quart")])


model=lm(log(QTY)~PRICE+MAXTEMP+Year+Quart,data=sales1)
summary(model)

model=lm(log(QTY)~PRICE+MAXTEMP+Year+Quart,data=sales1)
summary(model)

#회귀식
#log(QTY)=-0.04085-0.0003805PRICE+0.01953MAXTEMP+0.2073Year+0.04804Quart
#QTY = exp(-0.04085 - 0.0003805*PRICE + 0.01953*MAXTEMP + 0.2073*Year + 0.04804*Quart)

par(mfrow=c(2,2))
plot(lm(log(QTY)~PRICE+MAXTEMP+Year+Quart,data=sales1))
# 잔차분석
# Residuals vs Fitted Plot: 예측값과 잔차가 무작위로 흩어져있음
# Normal Q-Q Plot: 잔차가 정규분포를 따름
# Scale-Location Plot: 일정한 분산값을 가짐
# Residuals vs Leverage Plot : 대부분의 관측치는 레버리지가 낮고 표준화 잔차가 작아야 합니다.
# 잔차에 이상이 없음을 확인할 수 있음

both=step()

#ALL Subsets Regression
library(leaps)
leaps=regsubsets(log(QTY)~PRICE+MAXTEMP
             +Year+Quart,data=sales1,nbset=10)



