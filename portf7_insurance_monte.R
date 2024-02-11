## リアルオプション2022 モンテカルロ法　

## integral 
R <- 100000
X <- runif(R, 0, 1)
4*mean( sqrt(1-X^2) )


## ruin probability　保険会社の倒産
u0 <- 10000     # initial assets
N <- 50000     # number of insured persons 
p <- 0.001    # accident rate 
lam <- 10    # average claims 
th <- 0.1     # safety margin
mu <- (1+th)*p*lam

set.seed(1)
t <- 60
R <- 10000     # number of Monte Carlo samples
S <- matrix(NA, t, R)
S[1,] <- u0
Ruin　<- rep(1, R)
for(k in 2:t){
  X <- rbinom(R, N, p)
  S[k,] <- S[k-1,] + N*mu - rexp(R, 1/(X*lam))
  Ruin <- Ruin*ifelse(S[k,]>0, 1, 0)
}

matplot(S, type="l", lty=1)
1 - mean(Ruin) ## 倒産確率


