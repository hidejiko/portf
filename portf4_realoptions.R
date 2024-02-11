## リアルオプション2022-23

## コールオプション
##（１）モンテカルロ法　for文を使う場合

S=548
K=630
r=5/100
q=0.0
sigma=0.3
T=1


Call_MC<-function(n)
{
  Call<-0
  for(i in 1:n)
  {
    Call<-Call+max(S*exp((r-q-sigma^2/2)*T+sigma*sqrt(T)*rnorm(1))-K,0)
  }
  Call<-exp(-r*T)*Call/n
  return(Call)
}

Call_MC(100000)

## （２）モンテカルロ法　ベクトルを使う場合

Call_MC2<-function(n)
{
  x<-rnorm(n)
  y<-S*exp((r-q-sigma^2/2)*T+sigma*sqrt(T)*x)-K
  Call<-exp(-r*T)*sum(y[y>0])/n #y(Call価値)の正の部分のみを足す
  return(Call)
}

Call_MC2(100000)

## （３）シミュレーション時間の比較

system.time(Call_MC(100000))
system.time(Call_MC2(100000))

## （４）モンテカルロ法　幾何ブラウン運動に従うサンプルパスを行う場合との比較

install.packages("yuima") # インストール (初回のみ)
library(yuima) # ライブラリの読み込み

# Calculation of call-option prices by Black-Sholes eq.
BlackScholesCallPrice = function(S, K, r, sigma, T=1)
{
  d1 <- ( log(S/K) + (r + sigma^2/2) * T)/( sigma * sqrt(T))
  d2 <- ( log(S/K) + (r - sigma^2/2) * T)/( sigma * sqrt(T))
  C0 <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  return(C0)
}

# Calculation of call-option prices by Monte Carlo method
MonteCarloCallPrice = function(S, K, r, sigma, n, T=1)
{
  n_sample <- 1000
  c0_list <- list()
  c0 <- 0
  for (i in 1:n) {
    resultGBM <- GBM_sample(S, r, sigma, n_sample)
    sT <- resultGBM@data@original.data[n_sample]
    c0 <- (1/i) * exp(-1*r*T) * max(sT - K, 0) + ((i-1)/i) * c0
    c0_list <- append(c0_list, list(c0))
  }
  return(c0_list)
}

# A function which generates sample paths that follows a Geometric Brownian motion
GBM_sample = function(x0, alpha, beta, n_sample, T=1)
{
  # Step1: Define SDE
  # dS_t = alpha * S_t * dt + beta * S_t * dW_t
  mod <-setModel(drift="alpha*x", diffusion="beta*x")
  
  # Step2: Define samples
  samp <-setSampling(Initial=0, Terminal=T, n=n_sample)
  
  # Step3: define the statistical model
  smod <-setYuima(model=mod, sampling=samp)
  
  # Step4: Generate sample paths
  xinit <- x0
  param <- list(alpha=alpha, beta=beta)
  resultGBM <- simulate(smod, xinit=xinit, true.parameter=param)
  return(resultGBM)
}

# Params for call-option pricing
n <- 10000 # Num. of MonteCalro simulation
K <- 630 # Option exercise price (at t=T)
S <- 548 # Curent asset price (at t=0)
r <- 0.05 # Drift for Geometric Brownian motion
sigma <- 0.3 # Diffusion for Geometric Brownian motion
T <- 1 # Optional term

# Main procedure: Run simulation
bs_price <- BlackScholesCallPrice(S, K, r, sigma, T=1)
mc_price <- MonteCarloCallPrice(S, K, r, sigma, n, T=1)
#system.time(Call_MC(MonteCarloCallPrice(S, K, r, sigma, n, T=1)))

print(bs_price)
plot(1:n, mc_price, main="Monte Carlo Simulation:\nBlack-Scholes Option Pricing Model", xlab="Number of sample paths: # of ST", ylab="Option Price: C0",ylim = c(40,50), cex=0.5)
abline(h=bs_price, col='red', lwd=1, lty=2)

## （５）ブラック・ショールズ式による確認

S = 548
K = 630
T = 1
r = 5 / 100
q = 0 / 100
sigma = 30 / 100


BS_call = function(S, K, r, q, sigma, T)
{
  d1 = ( log(S/K) + (r - q + sigma^2/2)*T)/( sigma* sqrt(T))
  d2 = ( log(S/K) + (r - q - sigma^2/2)*T)/( sigma* sqrt(T))
  C0 = exp(-q*T)*S * pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return(C0)
}

BS_call(S, K, r, q, sigma, T)

## （６）バイノミアルモデルによる確認

S0   <- 548                  #現在の原資産価格
K    <- 630                  #権利行使価格
r    <- 0.05                 #無リスク金利
q    <- 0.00                 #配当率
sigma<- 0.3                  #ボラティリティ
T    <- 1.0                  #満期

binomialtree<-function(Step){
  Delta.t =T/Step                    #1Step当たりの期間
  u    <- exp(sigma*sqrt(Delta.t))       #上昇率
  d    <- exp(-sigma*sqrt(Delta.t))      #下落率
  p    <-(exp((r-q)*Delta.t)-d)/(u-d)    #リスク中立確率
  
  myMax=function(x)
  {
    myMax=max(x,0)
    return(myMax)
  }
  j=0:Step
  C0=sum(as.numeric(lapply(u^j*d^(Step-j)*S0-K,myMax))*
           choose(Step,j)*p^j*(1-p)^(Step-j))/(1+r)^T
  return( C0 )
}

binomialtree(1000)
system.time(binomialtree(1000))

## （７）　バイノミアルモデルのステップ数別のシミュレーション

BS_call <- function(S, K, r, q, sigma, T)
{
  d1 <- ( log(S/K) + (r - q + sigma^2/2)*T)/( sigma* sqrt(T))
  d2 <- ( log(S/K) + (r - q - sigma^2/2)*T)/( sigma* sqrt(T))
  C0 <- exp(-q*T)*S * pnorm(d1) - exp(-r*T)*K*pnorm(d2)
  return(C0)
}

(BS<-BS_call(S0,K,r,q,sigma,T))

#binomial = seq(1,100,1)
binomial_series =lapply(1:100,binomialtree)

plot(1:100,binomial_series,type="l",ylim=c(40,55),ann=F)
par(new=T)
plot(1:100,rep(BS,100),col="red",type="l",ylim=c(40,55),ann=F)

binomialtree(100)-BS  ## １００回の誤差

## （６）本源的価値と時間価値の図示

S_series = seq(0,1000,1)

IntrinsicValue=array(1:length(S_series))
for(i in 1:length(S_series)){
  IntrinsicValue[i]=max(S_series[i]-K,0)
}

BS=BS_call(S_series, K, r, q, sigma, T)

plot(BS,type="l",col="red",xlim=c(0,1000),ylim=c(0,300),ann=F)
par(new=T) 
plot(IntrinsicValue,type="l",xlim=c(0,1000),ylim=c(0,300),ann=F)

## 参考
## ファイナンスのためのRプログラミング ―証券投資理論の実践に向けて― 大崎 秀一,吉川 大介 共立出版 2013
## フィナンシャルエンジニアリング〔第9版〕 ―デリバティブ取引とリスク管理の総体系 ジョン ハル きんざい 2016





