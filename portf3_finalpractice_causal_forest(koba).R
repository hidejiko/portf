
## 小林秀二　


##### 疑似データを使ったCausal Forest #####

##Causal Forestのパッケージ"grf"の読み込み##
library(grf) 

##疑似データの作成
#n <- 2000
#p <- 10
#X <- matrix(rnorm(n * p), n, p) # 要素/row行数/col列数 ⇒　正規 2000行×p10個 
#W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))　# 二項分布0,1　rbinom(n, size, prob)
#Y <- pmax(X[, 1], 0) * W + X[, 2] + pmin(X[, 3], 0) + rnorm(n)　#n2000個

df <- read.csv("jtpa.csv",header=T)
head(df, n = 5)
dim(df)
names(df)

df <- as.data.frame(df)
Y<- as.numeric(df$earnings)
df$hsorged <- round(df$hsorged,digits =0)

class(Y)

df$x0<- as.factor(df$assignmt)
df$x1<- as.numeric(df$age)
df$x2<- as.numeric(df$prevearn)
df$x3<- as.numeric(df$hsorged)

X <- cbind(df$x1,df$x2,df$x3)  

n <- 11204
p <- 3
W <- rbinom(n, 1, 0.4 + 0.2 * (X[, 1] > 0))　# 二項分布0,1　rbinom(n, size, prob)


X.test <- matrix(0, 1001, p) #テストデータ用の空行列を用意　要素0、行101 p=10列を指定
X.test[, 1] <- seq(0,9000, length.out = 1001)  #等差数列seq(x, y, by = z)で, x から y まで zの値ごと length.out =  個数

# Causal Forestの学習
tau.forest <- causal_forest(X,Y,W)
## out-of-bagサンプルによるCATEの予測
tau.hat.oob <- predict(tau.forest)
hist(tau.hat.oob$predictions) #hist1 分布

## テストサンプルにCATEの予測
tau.hat <- predict(tau.forest, X.test)
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions, 0, 2), xlim=c(0,9000),xlab = "x", ylab = "CATE(x)", type = "l")
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 2)
#黒線はCATEの予測値、赤線は真のCATEを表す。plot2 

## Average Treatment Effectの推定
average_treatment_effect(tau.forest, target.sample = "all")

## Average Treatment Effect on the Treatedの推定
average_treatment_effect(tau.forest, target.sample = "treated")

## 95%信頼区間の計算
#3000本の木でcausal forestを学習する。
tau.forest <- causal_forest(X, Y, W, num.trees = 3000)

tau.hat <- predict(tau.forest, X.test, estimate.variance = TRUE)
sigma.hat <- sqrt(tau.hat$variance.estimates)
# plot3 
plot(X.test[, 1], tau.hat$predictions, ylim = range(tau.hat$predictions + 1.96 * sigma.hat, tau.hat$predictions - 1.96 * sigma.hat, 0, 2), xlab = "x", ylab = "CATE(x)", type = "l")
lines(X.test[, 1], tau.hat$predictions + 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], tau.hat$predictions - 1.96 * sigma.hat, col = 1, lty = 2)
lines(X.test[, 1], pmax(0, X.test[, 1]), col = 2, lty = 2)

