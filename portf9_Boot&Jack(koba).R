#ブートストラップ法――経験的密度関数・信頼区間・誤差評価
#bootstrap method & Jackknife method
#「ブートストラップ」とは計算統計学のひとつの手法で，データからの無作為再抽出を繰り返すことにより，
#推定量（たとえば標本平均のような）の誤差（バラツキ）を評価したり，密度関数を経験的に推定したりする．
#特定のパラメトリックな確率分布（たとえば正規分布のような）を前提とせず，むしろデータそのものから推定量の確率分布を導き，
#誤差評価をしたり，信頼区間をつくろうというノンパラメトリックな「精神」の発露である．

data <- c(0,1,2,3,4,5,6,7,8,9) # 数列｛0,1,2,...,9｝をテストデータとして data に格納
var(data) # data の分散
#ここで var(data) は分散の不偏推定値（平方和/自由度）であることに注意．下記に検算結果を記す．
deviation <- numeric(10) # deviation を長さ 10 の数値ベクトルとする
for (i in 1:10) deviation[i] <- data[i] - mean(data) # 偏差＝データ－平均を求め deviation に格納
ms <- (sum(deviation^2))/(10-1) # 分散＝平方和/自由度を ms に格納
ms # var(data)との一致を確認

var(data)/10 # data の標本平均の分散

sample(data, 10, replace=T) # data から重複を許して10標本を無作為再抽出（ブートストラップ）
# 以下，再抽出を繰り返してみる
sample(data, 10, replace=T)
sample(data, 10, replace=T)
sample(data, 10, replace=T)
sample(data, 10, replace=T)
sample(data, 10, replace=T)

mean(data) # 元データ（data）の標本平均
mean(sample(data, 10, replace=T)) # ブートストラップ１の平均
mean(sample(data, 10, replace=T)) # ブートストラップ２の平均
mean(sample(data, 10, replace=T)) # 以下，同じ操作．
mean(sample(data, 10, replace=T))

bootsrtap<- numeric(10) # bootstrap を長さ10の数値ベクトルとしてオブジェクト定義

for (i in 1:10){
  bootsrtap[i] <- mean(sample(data, 10, replace=T)) # 10回のブートストラップ反復による平均値を bootstrap に格納
}

bootsrtap # 計算結果の表示

mean(bootsrtap) # 平均
var(bootsrtap) # 分散（上で求めた分散推定値よりもかなり小さい）

bootstrap <-numeric(1000) # ブートストラップ反復の回数を1000回にしてみる
for (i in 1:1000){
  bootstrap[i] <- mean(sample(data, 10, replace=T))
}

bootstrap# 計算結果の表示
var(bootstrap) # 分散（10回反復のときよりも大きい）


#正規乱数をデータとする事例――標本平均のブートストラップ誤差評価

data <- c(rnorm(1000, mean=0, sd=1)) # 標準正規分布 N(0,1) からの1000無作為標本を data に格納
data # data の中身
mean(data)# data の標本平均

sample(data, 1000, replace=T) # data から重複を許して1000個の標本を無作為再抽出（ブートストラップ）

set.seed(101)# 擬似乱数の seed を「101」に設定
m <- 1000 # ブートストラップ反復回数を m=1000 に設定
replicate1000 <- numeric(m) # "replicate1000" を長さ1000の数値ベクトルとしてオブジェクト定義
for (i in 1:m){   # data からの m 回ブーツストラップを実行し，結果をreplicate1000に格納
  replicate1000[i] <- mean(sample(data, m, replace=T))
}

mean(replicate1000) # ブートストラップ平均
var(replicate1000)# ブートストラップ分散
sd(replicate1000) # ブートストラップ標準偏差

m <- 500 # ブートストラップ反復を m=500 として実行
replicate500 <- numeric(m)
for (i in 1:m){ 
  replicate500[i] <- mean(sample(data, m, replace=T))
}
mean(replicate500)
var(replicate500)
sd(replicate500)

m <- 100 # ブートストラップ反復を m=100 として実行
replicate100 <- numeric(m)
for (i in 1:m){ 
  replicate100[i] <- mean(sample(data, m, replace=T))
}

mean(replicate100)
var(replicate100)
sd(replicate100)

# m=100, 500, 1000 の結果をヒストグラム表示
par(mfrow = c(1, 1))

hist(replicate100, freq=F, ylim=c(0,15), col = "gray")

hist(replicate500, density=25, freq=F, add=T, col = "#FF00007F")

hist(replicate1000, density=25, angle=135, freq=F, add=T ,col = "#0000FF7F")

#####################################################################
#ヒストグラムからの確率密度関数の推定と信頼区間の計算

#ブートストラップによって求められた推定量（ここでは標本平均）のバラツキをヒストグラムとして描画できたならば，
#それに基づいて確率密度関数を推定することが可能である．得られた経験的密度関数を用いて，推定量の信頼区間を設定することもできる．
#この密度関数の経験的推定には，ある基底関数（kernel）を用いたヒストグラムの平滑化（smoothing）という手法が用いられている．
#天下り式の確率密度関数をデータに当てはめるのではなく，むしろデータから逆に密度関数そのものを推定しようというスタンスを取る．

library(MASS) # ライブラリー MASS をインストールする．
# これは "Modern Applied Statistics with S" のためのライブラリー

data <- c(rnorm(1000, mean=0, sd=1))
# 標準正規分布から1000乱数をデータとして data に格納
set.seed(101) # 擬似乱数シードを 101 に設定
m <- 1000 # ブートストラップ反復回数は m=1000回
replicate <- numeric(m) # replicate は長さ m のベクトル
for (i in 1:m) {  # data からのブートストラップ計算
  replicate[i] <- mean(sample(data, m, replace=T))
}

mean(replicate) # 平均
var(replicate) # 分散
sd(replicate) # 標準偏差

truehist(replicate, h=0.01)# 得られた1000個のブートストラップ値をヒストグラム表示
lines(density(replicate, width="SJ-dpi", n=256))

# このヒストグラムから経験的密度関数を推定する
quantile(replicate, p=c(0.025, 0.975)) # 推定された密度関数から５％信頼区間を求める．
# 下側2.5%点と上側2.5%点が表示される．

####################################################################
#コマンド「boot」の利用
#Ｒにはブートストラップに特化したコマンド「boot」がライブラリに用意されている．上述の操作は，この「boot」を用いることにより，格段に単純化される

library(boot) # ライブラリ「boot」をインストールする
data <- c(0,1,2,3,4,5,6,7,8,9) # 数列｛0,1,2,...,9｝を data に格納
set.seed(101) # 乱数シードを「101」に設定
# ブートストラップの設定１）「data」:再抽出対象となるデータ名．ここでは"data"．    
#                       ２）「function(x, i)mean(x[i])」：誤差評価を行なう統計量の関数指定．ここでは標本平均．
#                       ３）「R」：ブートストラップ反復回数．ここでは1000回．
boot(data, function(x, i)mean(x[i]), R=1000) 
#「ブートストラップ統計量」として出力されるのは，元データの平均（original）・ブートストラップ反復から得られた平均の
#original からのバイアス（bias）・ブートストラップから得られた標本平均の標準誤差（std. error）である．
#なお「boot」は他にも多くの機能をもっており，上述のような，ノンパラメトリック・ブートストラップだけでなく，パラメトリック・ブートストラップ（モンテ・カルロ法の一種）も実行できる．
#詳細は help(boot)を参照していただきたい．

####################################################################
#ジャックナイフ法による誤差評価
#ジャックナイフによる誤差評価は，データからの重複を許さない（replace=F）再抽出の反復に基づく．
#したがって，反復回数を m，再抽出数を k（＜m）で指定したとき，
# replicate <- numeric(m)
# for (i in 1:m) replicate[i] <- mean(sample(data, k, replace=F))
# var(replicate)
#によって，標本平均の誤差を求めることができる．たとえば，下記のように：

data <- c(0,1,2,3,4,5,6,7,8,9)# データ読みこみ
m <- 10 # 再抽出回数
k <- 9 # 除去数１の delete-one jackknife
replicate <- numeric(m)
for (i in 1:m) {
  replicate[i] <- mean(sample(data, k, replace=F))
}
var(replicate)
sd(replicate)

m <- 10 # 再抽出回数
k <- 5 # 半数除去の delete-half jackknife
replicate <- numeric(m)
for (i in 1:m){ 
  replicate[i] <- mean(sample(data, k, replace=F))
}
var(replicate)
sd(replicate)

## 参考
## http://leeswijzer.org/R/R-resampling.html












