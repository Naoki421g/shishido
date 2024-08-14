_?#packageをロード
library(tidyverse)
library(e1071)
library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(stringi)
library(caTools)
library(randomForest)
library(rpart)
library(kernlab)
library(nnet)
library(car)
library(Metrics)

#データのimport
df <- read_csv("data_yamagata.csv",locale = locale(encoding = "UTF-8"))
#標準化
df$squ_n <- scale(df$squ)
df$age_n <- scale(df$age)

#dfの型を確認
is.data.frame(df)

#データの数
ndf<-nrow(df)

#priceデータの確認
hist(df$price,main='price_hist',xlab='price')
boxplot(df$price,main='Price')
summary(df$price)
#対数変換
ln_price <- log(df$price)
hist(ln_price,main='ln_price_hist',xlab='ln_price')
boxplot(df$ln_price,main='ln_Price')

#age
plot(df$age,df$price,xlab='age',ylab='price')

#squere
plot(df$squ,df$price,xlab='squ',ylab='price')

#staion
ggplot(df,aes(x=factor(station),y=price)) + geom_boxplot() + theme_classic()

#region
ggplot(df,aes(x=factor(region_dammy),y=price)) + geom_boxplot() + theme_classic()

#region_有意差検定
price_yamagata <- df$price[df$region_dammy == 1]
price_other <- df$price[df$region_dammy == 0]
t_test_result <- t.test(price_yamagata, price_other)
print(t_test_result)

#renovation
ggplot(df,aes(x=factor(renov_dammy),y=price)) + geom_boxplot() + theme_classic()

#15年以上のものを抽出
df_over15 <- df[df$age>15,]
ggplot(df_over15,aes(x=factor(renov_dammy),y=price)) + geom_boxplot() + theme_classic()
#有意差検定
price_renov <- df$price[df_over15$renov_dammy == 1]
price_nonrenov <- df$price[df_over15$renov_dammy == 0]
t_test_result <- t.test(price_renov, price_nonrenov)
print(t_test_result)

#room
ggplot(df,aes(x=factor(room),y=price)) + geom_boxplot() + theme_classic()

#LandUse
ggplot(df,aes(x=factor(LandUse_dammy),y=price)) + geom_boxplot() + theme_classic()


#シードの固定
set.seed(120)

# データをトレーニングセットとテストセットに分割
split <- sample.split(df$price, SplitRatio = 0.8)
df.train <- subset(df, split == TRUE)
df.test <- subset(df, split == FALSE)

#線形モデル
liner_model <- lm(price ~ station + region_dammy + squ  + age + renov_dammy + room + factor(LandUse_dammy)
                  , data=df.train)
#結果を表示
summary(liner_model)
#AICを最小に
step(liner_model)
#線形モデル
liner_model_best <- lm(price ~ station + squ  + age + renov_dammy + region_dammy
                  , data=df.train)

#線形モデル
liner_model_best_n <- lm(price ~ station + squ_n  + age_n + renov_dummy + region_dummy
                       , data=df.train)
#結果を表示
summary(liner_model_best_n)
#多重共線性チェック
vif <- vif(liner_model_best)
vif
#予測
pred.liner<-predict(liner_model_best_n,newdata=df.test)
RMSE_liner <- (mean((pred.liner-df.test$price)^2))^0.5
RMSE_liner
#RMSE_liner_true <- rmse(df.test$price,pred.liner)
#RMSE_liner_true
plot(pred.liner,df.test$price,ylab='price')
abline(a = 0, b = 1, col = "red", lty = 2)

#ランダムフォレスト
classifier_Random_Forest <- randomForest(price ~ station + squ  + age + renov_dammy +region_dammy,
                                         data=df.train,
                                         ntree = 2300)
pred.forest<-predict(classifier_Random_Forest,newdata=df.test)
RMSE_forest <- (mean((pred.forest-df.test$price)^2))^0.5 
RMSE_forest
plot(pred.forest,df.test$price,ylab='price')
abline(a = 0, b = 1, col = "red", lty = 2)

#svm
svm<-ksvm(price ~ station + squ  + age + renov_dammy +region_dammy ,
          data=df.train)
pred.svm<-predict(svm,newdata=df.test)
RMSE_svm <- (mean((pred.svm-df.test$price)^2))^0.5 
RMSE_svm
plot(pred.svm,df.test$price,ylab='price')
abline(a = 0, b = 1, col = "red", lty = 2)

print(RMSE_liner)
print(RMSE_forest)
print(RMSE_svm)

#決定木
cart<-rpart(ln_price ~ station + region_dammy + squ + year1 + year2　+renov,
            data=df.train)
pred.cart<-predict(cart,newdata=df.test)
MSE_cart <- mean((pred.cart-df.test$ln_price)^2) 
MSE_cart
plot(pred.cart,df.test$ln_price)
abline(a = 0, b = 1, col = "red", lty = 2)
#nn
#nn<-nnet(price ~ station + region_dammy + squ + year1 + year2　+renov,data=df.train,size=10)
#pred.nn<-predict(nn,newdata=df.test)
#MSE = sum((pred.nn-df.test$price)^2) / (ndf*0.2)
#MSE
#plot(pred.nn,df.test$price,lty=2)
#abline(a = 0, b = 1, col = "red", lty = 2)

