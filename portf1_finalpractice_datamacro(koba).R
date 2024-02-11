
## 小林秀二　

##すべてのオブジェクトを消去する##
rm(list=ls())

##パッケ)ージの読み込み##
library(tree)
install.packages("tidyverse") 
library(tidyverse)
library(dplyr)

#############################データの読み込み　######################################
Macro <- read.csv("data_macro.csv",header=T)
head(Macro,n=5)
tail(Macro,n=5)
dim(Macro)
names(Macro)

###############################変数名の変更#####################################

Macro <- rename(Macro,Real=x183,date=月次)
names(Macro)
attach(Macro)
Real

##############################説明変数をずらす#######################################
# 3個ずらした変数を作成
Macro <- Macro %>%
  mutate(across(x1:x182,~lag(. , n = 3), .names = NULL))
Macro <- Macro %>%
  mutate(across(x184:x219,~lag(. , n = 3), .names = NULL))
# 頭の３つ削除
Macro <- Macro %>%
  slice(-1:-3)

head(Macro,n=5)
tail(Macro,n=5)
dim(Macro)

########################## （全データ）決定木　######################
tree.real=tree(Real~.-Real, Macro) #分類木
summary(tree.real)

##計算された分類木をプロットする##
plot(tree.real)
text(tree.real,pretty=1)
#pretty=1と指定することで質的予測変数のカテゴリー名を表示する。

##################### （a）訓練とテストに分割 ###############

##データを訓練データとテストデータに分割して、テスト誤分類率を計算する##
#set.seed(1234)
#train = sample(1:nrow(Macro), nrow(Macro)/2)  #sample乱数で２分割をトレーニング
#Macro.test = Macro[-train,] #残り２分割をテストデータ

splitmacro <- split(Macro, Macro$date<=2003.12)
train <- splitmacro$'TRUE'
test <- splitmacro$'FALSE'
dim(train)
dim(test)

########################## （b）訓練データ決定木グラフ描画　######################

#tree.real = tree(Real~.-Real,Macro,subset=train)#訓練データの回帰木
tree.real = tree(Real~.-Real,train)#訓練データの回帰木

##計算された分類木をプロットする##
plot(tree.real)
text(tree.real,pretty=0)
#pretty=1と指定することで質的予測変数のカテゴリー名を表示する。


########################## （b）テストデータ決定木　######################

##テストデータ-trainでの性能評価##
#Real.test = Macro[-train,"Real"]#テストデータの正解
Real.test = test$Real #テストデータの正解


#tree.pred = predict(tree.real,newdata=Macro[-train,])#テストデータによる予測値
tree.pred = predict(tree.real,newdata=test)#テストデータによる予測値

#テストデータでのMSE#
mean((tree.pred-Real.test)^2)

##################### （C）交差検証法 #####################################

##交差検証法により木の剪定を行う##
set.seed(1234)
#cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass) #訓練データの分類木による
#FUN=prune.misclassとすることにより、誤分類率を基準に木の剪定を行う。デフォルトでは、逸脱度を使用している
tree.real=tree(Real~.-Real, Macro) #分類木（全データ）の場合
#cv.real=cv.tree(tree.real,FUN=prune.tree)  #分類木による
cv.real=cv.tree(tree.real,FUN=prune.tree)  #分類木による
#FUN=prune.misclassとすることにより、誤分類率を基準に木の剪定を行う。デフォルトでは、逸脱度を使用している。
names(cv.real)
cv.real
#size: 終端ノードの数、k:チューニングパラメータalphaの値、dev:交差検証による誤分類数

#sizeとkの関数として誤分類数を描写する
par(mfrow=c(1,2))
plot(cv.real$size,cv.real$dev,type="b")
plot(cv.real$k,cv.real$dev,type="b")
#交差検証誤差を最小にするsizeになるように分類木を剪定する
best.size = cv.real$size[which(cv.real$dev==min(cv.real$dev))] 
prune.real=prune.tree(tree.real,best=best.size)
#剪定された分類木をプロットする
par(mfrow=c(1,1))
plot(prune.real)　#決定木グラフ
text(prune.real,pretty=0)

##テストデータ-trainでの性能評価##
#Real.test = Macro[-train,"Real"]#テストデータの正解
#tree.pred=predict(prune.real,Macro.test,type="vector")#テストデータによる予測値
#tree.pred=predict(tree.carseats,newdata=Carseats[-train,])#テストデータによる予測値（こっちでもよい）

Real.test = test$Real #テストデータの正解
tree.pred=predict(prune.real, newdata=test,type="vector")#テストデータによる予測値

#テストデータでのMSE#
mean((tree.pred-Real.test)^2)

########################  (d) バギング  #################################################
#### Bagging and Random Forests ####

##パッケージ"randomForest"のインストール##
install.packages("randomForest")
##パッケージの読み込み##
library(randomForest)
#library(MASS)
dim(Macro)

##乱数のシードの設定##
#set.seed(1)

##訓練データとテストデータへの分割##
#train = sample(1:nrow(Macro),nrow(Macro)/2)

splitmacro <- split(Macro, Macro$date<=2003.12)
train <- splitmacro$'TRUE'
test <- splitmacro$'FALSE'
dim(train)
dim(test)

##バギングによる学習##ブートストラップ・アグリゲーティングバギングはモデル平均化手法の一種 決定木に使われる
#bag.real=randomForest(Real~.,data=Macro,subset=train,mtry=10,importance=TRUE)
bag.real=randomForest(Real~.,data=train,mtry=10,importance=TRUE)

#mtry=13は、13個のすべての予測変数を木の各分割いおいて考えることを示しており、すなわちバギングを意味する。
#importance=TRUEとすることで、variable importanceを評価する。
#木の数はデフォルトで500になっている。変更する場合は引数ntree=xと入力する。
bag.real　#バギングの結果出力

##テストデータ-trainでの性能評価##
#yhat.bag = predict(bag.real,newdata=Macro[-train,])#テストデータによる予測値
yhat.bag = predict(bag.real,newdata=test)#テストデータによる予測値
#Real.test = Macro[-train,"Real"]#テストデータの正解
Real.test = test$Real #テストデータの正解

par(mfrow=c(1,1))
plot(yhat.bag, Real.test,ylim = c(-20,20))# 予測と正解のグラフ
abline(0,1)
#テストデータでのMSE#
mean((yhat.bag-Real.test)^2)

#予測変数の重要度を計算する#
importance(bag.real)
#"%IncMSE"は、与えれた変数がモデルから取り除かれたときのOOBのサンプルにおける予測精度の平均変化率である。
#"IncNodePurity"は、その変数によって得られるノードの不純度の総減少量をすべての木について平均したものである。
# 詳しくはコマンド"help("importance")"を入力して参照されたい。

#予測変数の重要度をプロットする
varImpPlot(bag.real)


################## (e) ランダムフォレスト　##########################
set.seed(1)
# チューニングtuneRF()関数でOOB error（誤判別率）が一番低くなるmtryを探す 説明変数、目的変数
#Macro.res <- tuneRF(Macro[train,-1],Macro[train,1],doBest=T)
Macro.res <- tuneRF(train,train$Real,doBest=T)

##mtry=p^{1/2}としてランダムフォレストにより予測を行う##mtry=x*(1/2)=3
#rf.Macro=randomForest(Real~.,data=Macro,subset=train,mtry=10,importance=TRUE)
rf.Macro=randomForest(Real~.,data=train,mtry=10,importance=TRUE)
#yhat.rf = predict(rf.Macro,newdata=Macro[-train,])
yhat.rf = predict(rf.Macro,newdata=test)

mean((yhat.rf-Real.test)^2)#MSE 平均二乗誤差

#予測変数の重要度を計算する#
importance(rf.Macro)
#"%IncMSE"は、与えれた変数がモデルから取り除かれたときのOOBのサンプルにおける予測精度の平均変化率である。
#"IncNodePurity"は、その変数によって得られるノードの不純度の総減少量をすべての木について平均したものである。
# 詳しくはコマンド"help("importance")"を入力して参照されたい。

#予測変数の重要度をプロットする
varImpPlot(rf.Macro)

##########（f）ブースティング　##################################################

#### Boosting ####

##パッケージ"gbm"のインストール##
install.packages("gbm")
##パッケージの読み込み##
library(gbm)
library(MASS)

#乱数のシードの設定#
#set.seed(1)
#訓練データとテストデータへの分割#
#train = sample(1:nrow(Macro),nrow(Macro)/2)  #sample乱数で２分割をトレーニング
#Carseats.test = Carseats[-train,] #残り２分割をテストデータ


splitmacro <- split(Macro, Macro$date<=2003.12)
train <- splitmacro$'TRUE'
test <- splitmacro$'FALSE'
dim(train)
dim(test)



##ブースティングによる予測モデルの計算##
#boost.real <- gbm(Real~ ., data = Macro[train,],
#                    distribution = "gaussian", n.trees = 5000,
#                    interaction.depth = 4)

boost.real <- gbm(Real~ ., data = train,
                  distribution = "gaussian", n.trees = 5000,
                  interaction.depth = 4)

# distribution = "gaussian"と設定することでRSS（残差平方和）最小化を基準とする回帰問題を設定する。
# 2値分類問題の場合は、distribution = "bernoulli"を指定する。
# 引数n.trees = 5000は5,000個の木を用いることを示す。
# interaction.depth = 4により各々の木の深さを最大4に限定する。
# チューニングパラメータlambdaの値はデフォルトで0.001となっている。

##summary()関数により変数の重要度を出力する##
summary(boost.real)
#出力結果の解釈についてはコマンド"help("summary.gbm")"で確認されたい。

Real.test = test$Real #テストデータの正解

##部分従属プロットを作成する##
par(mfrow=c(1,2))
plot(boost.real, i = "x105")
plot(boost.real, i = "x128")
plot(boost.real, i = "x211")

#詳細についてはコマンド"help("plot.gbm")"で確認されたい。

##テストデータMSEを計算する##
#yhat.boost <- predict(boost.real,
#                      newdata = Macro[-train, ], n.trees = 5000)
yhat.boost <- predict(boost.real,
                      newdata = test, n.trees = 5000)

mean((yhat.boost - Real.test)^2)

## チューニングパラメータlambdaの値を変更して学習してみる##
#boost.real <- gbm(Real ~ ., data = Macro[-train,],
#                    distribution = "gaussian", n.trees = 5000,
#                    interaction.depth = 4, shrinkage = 0.2)
#yhat.boost <- predict(boost.real,
#                      newdata = Macro[-train, ], n.trees = 5000)
boost.real <- gbm(Real ~ ., data = test,
                  distribution = "gaussian", n.trees = 5000,
                  interaction.depth = 4, shrinkage = 0.2)
yhat.boost <- predict(boost.real,
                      newdata = test, n.trees = 5000)

mean((yhat.boost - Real.test)^2)



