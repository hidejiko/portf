## リアルオプション　サンプルコード
###　　広告費と売上高

###
### 練習問題 回帰係数の推定
###

install.packages("scatterplot3d") 
library(scatterplot3d)

### 広告費と売上データによる回帰分析

## データの読み込み
##adv_data <- read.csv('https://www.statlearning.com/s/Advertising.csv',
##                     row.names=1) # 1列目を行名として読み込む
adv_data <- read.csv("Advertising.csv")

plot(adv_data, col="orange") # 散布図

## TVの宣伝費で売上を説明
(adv_est1 <- lm(sales ~ TV, data=adv_data))
plot(sales ~ TV, data=adv_data, col="orange")
abline(adv_est1, col="brown", lwd=2)

## radioの宣伝費で売上を説明
(adv_est2 <- lm(sales ~ radio, data=adv_data))
plot(sales ~ radio, data=adv_data, col="orange")
abline(adv_est2, col="brown", lwd=2)

## 両者の宣伝費で売上を説明
(adv_est <- lm(sales ~ TV + radio, data=adv_data))
s3d <- scatterplot3d( 
    model.frame(adv_est)[c("TV","radio","sales")], # x,y,z の順
    type="p", # plotの種類: "p"点，"l"線，"h"足付き
    pch=16,# 点の種類 (?points 参照)
    angle=45, # xy平面の見る方向 (適宜調整)
    highlight.3d=TRUE # 高さ(z)ごとに色を変える
)
s3d$plane3d(adv_est, col="brown", # 回帰式の定める平面の追加
            draw_polygon=TRUE, # 平面の塗り潰しの設定
            polygon_args=list(col=rgb(1,0,0,0.1)))

### 
### 練習問題 最小二乗推定量の性質
### 


### 広告費と売上データ

## 回帰係数と正規方程式の解の一致
(beta <- coef(adv_est))        # 推定された回帰係数
X <- model.matrix(adv_est)     # デザイン行列
Y <- model.frame(adv_est)[[1]] # 目的変数
solve(crossprod(X)) %*% crossprod(X, Y) # 正規方程式の解

## あてはめ値と残差の直交性
yhat <- fitted(adv_est) # あてはめ値
ehat <- resid(adv_est)  # 残差
yhat %*% ehat           # 直交すれば内積はO

## 回帰式が標本平均を通ること
colMeans(X) %*% beta # 説明変数の標本平均のあてはめ値
mean(Y)              # 目的変数の標本平均

### 
### 練習問題 残差の分解
### 

### 広告費と売上データ

summary(adv_est)
Y <- model.frame(adv_est)[[1]]           # 目的変数の取得
(Sy <- sum((Y-mean(Y))^2))               # 目的変数のばらつき
(S <- sum(resid(adv_est)^2))             # 残差のばらつき
(Sr <- sum((fitted(adv_est)-mean(Y))^2)) # 回帰のばらつき
S+Sr # Sy と同じになっている

### 
### 練習問題 決定係数によるモデルの比較
###

### 広告費と売上データ

## モデルの比較
adv_model1 <- sales ~ TV
adv_model2 <- sales ~ radio
adv_model3 <- sales ~ TV + radio
adv_est1 <- lm(adv_model1, data=adv_data, y=TRUE)
adv_est2 <- lm(adv_model2, data=adv_data, y=TRUE)
adv_est3 <- lm(adv_model3, data=adv_data, y=TRUE)
summary(adv_est1)$adj.r.squared # 自由度調整済み決定係数
summary(adv_est2)$adj.r.squared # (model1より減少)
summary(adv_est3)$adj.r.squared # (model1より上昇)

## 予測値と実測値の比較
with(adv_est1,
     plot(y,fitted.values,col="orange",pch=17, # 三角
          xlab="sales",
          ylab="fitted values",ylim=range(y)))   
abline(0,1,col="red",lwd=2)
with(adv_est2,
     points(y,fitted.values,col="green",pch=15)) # 四角
with(adv_est3,
     points(y,fitted.values,col="blue",pch=16))  # 丸
legend("bottomright",inset=.05, # 凡例の作成
       col=c("orange","green","blue"), pch=c(17,15,16), 
       legend=c("model1","model2","model3"))

