### 秘書問題
## 秘書の採用の試行
myTrial <- function(n,r,verbose=FALSE){ # nとrを指定
    applicants <- sample(1:n,size=n)
    ref <- applicants[1:(r-1)]
    test <- applicants[r:n]
    idx <- which(test < min(ref))
    if(length(idx)==0) {
        employed <- applicants[n]
    } else {
        employed <- test[idx[1]]
    }
    if(verbose==TRUE){ # 全順位も返す
        return(list(applicants=applicants,
                    employed=employed))
    } else { # 採用した者の順位のみ返す
        return(employed)
    }
}

## 試行を行ってみる
n <- 10 # 候補者は10名
myTrial(n,2,verbose=TRUE) # 2人目から採用を考える
myTrial(n,3,verbose=TRUE) # 3人目から採用を考える
myTrial(n,4,verbose=TRUE) # 4人目から採用を考える
myTrial(n,5,verbose=TRUE) # 5人目から採用を考える
myTrial(n,6,verbose=TRUE) # 6人目から採用を考える

## Monte-Carlo simulation
## set.seed(8888) # 実験を再現したい場合はシードを指定
mc <- 5000
n <- 25 # 候補者数を変えて実験
myData <- data.frame(r=NULL,employed=NULL)
for (r in 2:(n-1)) {
    foo <- replicate(mc,myTrial(n,r))
    if(r %in% c(2,6,10,14,18,22)) { # いくつか表示
        cat("採用開始: ", r, "\n")
        print(table(foo))
    }
    myData <- rbind(myData,data.frame(r=rep(r,mc),
                                  employed=foo))
}
boxplot(employed ~ r, data=myData, # rごとの箱ひげ図
        col="lightblue", main=paste("n =", n)) 
(bar <- aggregate(employed ~ r, data=myData,
                      FUN=function(x){mean(x==1)}))
plot(employed ~ r, data=bar, # 1位を採用できる確率を表示
     type="s", col="blue", lwd=3,
     main=paste("n =", n), ylab="probability")

## 理論的に良いとされるrの値 (nが十分大きい場合)
n/exp(1) 
abline(v=n/exp(1), col="red", lwd=3)

