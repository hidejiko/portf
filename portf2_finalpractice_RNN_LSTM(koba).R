
## 小林秀二　

##### RNNとLSTMによる金融データ予測 #####

###必要なパッケージの読み込み###############################################
install.packages("tensorflow")
library(tensorflow)
install.packages("keras")
library(keras)
install.packages("tidyverse") 
library(tidyverse)
library(dplyr)

########################  データ読み込み ###################################
Macro <- read.csv("data_macro.csv",header=T)
head(Macro, n = 5)
dim(Macro)
Macro$x183

names(Macro)
Macro <- rename(Macro,Real=x183,Topix=x158,Yields=x191,CPI_Housing=x207,date=月次)
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

##################### 訓練とテストに分割 #################################

#2004年までTRUE、そうでなければFALSEをとるカテゴリ変数trainを作成する。
train = factor(ifelse(Macro$date<=2003.12,"TRUE","FALSE"))
test  = factor(ifelse(Macro$date>2003.12,"TRUE","FALSE"))
train
test
Smarket = data.frame(Macro,train,test)　

###変数名の変更　データの選択###
selectlist <- c("Real","Topix","Yields", "CPI_Housing") 
xdata <- Smarket[, selectlist] #access data


dim(Smarket)
dim(xdata)
xdata <- scale(xdata) #データを標準化(平均0、標準偏差1)する。

istrain <- Smarket[, "train"] #TRUE FALSEのみ
istest <- Smarket[, "test"] #TRUE FALSEのみ

################################ データの記述統計量 #############################################

#log_volumeの時系列プロットを作る
plot(Smarket$Real, type = "l") #type = "l"とすることで線グラフを指定する。
#x軸をyearにする。
Real = ts(Smarket$Real,start=c(1974.01), frequency=12) 
#取引日は1年に約225日あるので、frequency=225とここでは設定する。
plot(Real,type = "l", xlab="year", main="")

#log_volumeの時系列プロットを作る
plot(Smarket$Topix, type = "l") #type = "l"とすることで線グラフを指定する。
#x軸をyearにする。
Topix = ts(Smarket$Topix,start=c(1974.01), frequency=12) 
#取引日は1年に約225日あるので、frequency=225とここでは設定する。
plot(Topix,type = "l", xlab="year", main="")


#log_volumeの時系列プロットを作る
plot(Smarket$Yields, type = "l") #type = "l"とすることで線グラフを指定する。
#x軸をyearにする。
Yields = ts(Smarket$Yields,start=c(1974.01), frequency=12) 
#取引日は1年に約225日あるので、frequency=225とここでは設定する。
plot(Yields,type = "l", xlab="year", main="")

#log_volumeの時系列プロットを作る
plot(Smarket$CPI_Housing, type = "l") #type = "l"とすることで線グラフを指定する。
#x軸をyearにする。
CPI_Housing = ts(Smarket$CPI_Housing,start=c(1974.01), frequency=12) 
#取引日は1年に約225日あるので、frequency=225とここでは設定する。
plot(CPI_Housing,type = "l", xlab="year", main="")

#3変数のグラフを並べる。
plot_data =  ts(Smarket[c("Real","Topix","Yields","CPI_Housing")], start=c(1974.01), frequency=12) 
plot(plot_data, xlab="year",main="")

par(mfrow=c(2,2))
#Todatの自己相関をプロットする。
acf(Smarket$Real,lag.max=35,main="Realのコレログラム")
#log_volumeの自己相関をプロットする。
acf(Smarket$Topix,lag.max=35,main="Topixのコレログラム")
#log_volumeの自己相関をプロットする。
acf(Smarket$Yields,lag.max=35,main="Yieldsのコレログラム")
#log_volumeの自己相関をプロットする。
acf(Smarket$CPI_Housing,lag.max=35,main="CPI_Housingのコレログラム")

########################################################################################
###ラグ変数を含むデータの作成###

lagm <- function(x, k) {  #データをk時点ずらす関数
  n <- nrow(x)
  pad <- matrix(NA, k, ncol(x))
  rbind(pad, x[1:(n - k), ])
}

#5ラグまで含むデータを作る
arframe <- data.frame(Real =xdata[,"Real"],
                      L1 = lagm(xdata, 1), L2 = lagm(xdata, 2),
                      L3 = lagm(xdata, 3), L4 = lagm(xdata, 4),
                      L5 = lagm(xdata, 5)
)

#NAを含むデータの行を削除する
arframe <- arframe[-(1:5),]
istrain <- istrain[-(1:5)]
dim(arframe)

##############################　（０）線形回帰　########################################

###log_volumeを"DJ_return", "log_volume","log_volatility"のラグ(1-5)変数に回帰する。###
arfit <- lm(Real~ ., data = arframe[istrain, ])
summary(arfit)
arpred <- predict(arfit, arframe[istest,]) #テストデータでの予測値を計算する
#arpred <- predict(arfit, arframe[!istrain,]) #テストデータでの予測値を計算する
#テストデータMSEを計算する#
mean((arpred - arframe[istest, "Real"])^2)

#######################################################################################


###################（３）RNN、（１）順伝播型NN、（４）LSTMでの予測#####################

##kerasを使えるようにデータの形を変換する##
n <- nrow(arframe) #サンプルサイズ
xrnn <- data.matrix(arframe[, -1]) #Xのデータを作る(第1列にあるアウトカム以外を取り出す)
xrnn <- array(xrnn, c(n, 3, 5))　#(n,3,5) (=(サンプルサイズ、変数の数、ラグ因子の数))の配列にデータを変換する。
xrnn <- xrnn[,, 5:1] #ラグ因子の時系列の順番を反転する。
xrnn <- aperm(xrnn, c(1, 3, 2)) #配列の2次元目と3次元目の順番を変える.
dim(xrnn)


######################（１）通常の順伝播型ニューラルネットによる学習・予測#####################

#共変量データの作成
x <- model.matrix(Real ~ . - 1, data = arframe)
colnames(x)

##順伝播型NNの定義、コンパイル、学習
arnnd <- keras_model_sequential() %>%
  layer_dense(units = 32, activation = 'relu',
              input_shape = ncol(x)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1)

arnnd %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop())

history <- arnnd %>% fit(
  x[istrain, ], arframe[istrain, "Real"], epochs = 50,
  batch_size = 32, validation_data =
    list(x[istest, ], arframe[istest, "Real"])
)

#順伝播型NNのテストデータMSE#
npred <- predict(arnnd, x[istest, ])
mean((arframe[istest, "Real"] - npred)^2)

############################（２）畳み込みニューラルネットワーク#####################################################

# 共変量データの作成
x <- array_reshape(x, c(dim(x)[1], dim(x)[2], 1))  # データの形状を変更

## 畳み込みニューラルネットワーク（CNN）の定義、コンパイル、学習
cnn <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = 'relu', input_shape = c(dim(x)[2], 1)) %>%
  layer_dropout(rate = 0.5) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dense(units = 1)

cnn %>% compile(loss = "mse",
                optimizer = optimizer_rmsprop())

history_cnn <- cnn %>% fit(
  x[istrain, , ], arframe[istrain, "Real"], epochs = 50,
  batch_size = 32, validation_data =
    list(x[istest, , ], arframe[istest, "Real"])
)

# 畳み込みニューラルネットワークのテストデータMSE
npred_cnn <- predict(cnn, x[istest, , ])
mean((arframe[istest, "Real"] - npred_cnn)^2)


################### (３）RNN再帰型ニューラルネットワークによる学習#############

##RNNを定義する##
model <- keras_model_sequential() %>%
  layer_simple_rnn(units = 12,
                   input_shape = list(5, 3), #入力の形式を指定する。今回は、xrnnの第2次元が5、第3次元が3なので、list(5,3)と指定。
                   dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units = 1)
#12個のユニットからなる1層の中間層をもつRNNを定義する。
#中間層には、layer_simple_rnnを使う。
#引数dropoutは、インプット層からの線形入力のドロップアウト率を指定する。
#引数recurrent_dropoutは、recurrent stateからの線形入力のドロップアウト率を指定する。

#モデルをコンパイルする。 
model %>% compile(optimizer = optimizer_rmsprop(),
                  loss = "mse")
#RNNを学習する。
history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "Real"],
  batch_size = 64, epochs = 50,
  validation_data =
    list(xrnn[istest,, ], arframe[istest, "Real"])
)

#RNNによる予測
rnnpred <- predict(model, xrnn[istest,, ])
#RNNによるテストデータMSE
mean((rnnpred - arframe[istest, "Real"])^2)


############################（４）LSTMによる学習・予測######################################

##LSTMの定義##
model <- keras_model_sequential() %>%
  layer_lstm(units = 12,input_shape=c(5, 3),dropout = 0.1, recurrent_dropout = 0.1) %>%
  layer_dense(units = 1)
#12個のユニットからなる1層の中間層をもつLSTMを定義する。
#中間層には、layer_lstmを使う。

#モデルのコンパイル#
model %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop())
#モデルの学習#
history <- model %>% fit(
  xrnn[istrain,, ], arframe[istrain, "Real"],
  batch_size = 64, epochs = 50,
  validation_data =
    list(xrnn[istest,, ], arframe[istest, "Real"])
)

#テストデータMSEの計算#
lstmpred <- predict(model, xrnn[istest,, ])
mean((lstmpred - arframe[istest, "Real"])^2)

##########################テストデータにおける予測値と実際のRealのプロットの作成###############################

Real = ts(arframe[istest, "Real"], start=c(2004.01), frequency=12) #実際の値 #テストデータは1980年1月2日から始まる。
LSTM_prediction = ts(lstmpred, start=c(2004.01), frequency=12) #LSTMの予測値
RNN_prediction = ts(rnnpred, start=c(2004.01), frequency=12) #RNNの予測値
cNN_prediction = ts(npred_cnn, start=c(2004.01), frequency=12) #CNNの予測値
AR_prediction = ts(arpred, start=c(2004.01), frequency=12) #線形回帰の予測値
NN_prediction = ts(npred, start=c(2004.01), frequency=12) #順伝播型NNの予測値

cols = c("black","blue","red","green","orange","magenta") #線グラフの色
labels = c("Actual value", "AR", "RNN", "NN", "LSTM","CNN") #線グラフのラベル
ltys = c("solid","dashed","dotted","longdash","twodash","dotdash") #線グラフのスタイル

par(mfrow=c(1,1))
plot(Real,type = "l", col=cols[1],lty=ltys[1],ylab="Real",xlab="year",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1)) #log_volumeは黒線で描く　
#ylim = c(-3.0,3.0)によってy軸の範囲を-3から3に指定している。
#col=によって線の色を指定しる。
#lty=によって線のスタイルを指定している。
par(new=T) #図を重ねる
plot(AR_prediction,type = "l",col=cols[2],lty=ltys[2],ylab="",xlab="",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1))  #線形回帰は青線で描く
par(new=T) #図を重ねる
plot(RNN_prediction,type = "l",col=cols[3],lty=ltys[3],ylab="",xlab="",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1)) #RNNは赤線で描く
par(new=T) #図を重ねる
plot(NN_prediction,type = "l",col=cols[4],lty=ltys[4],ylab="",xlab="",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1)) #NNは緑線で描く
par(new=T) #図を重ねる
plot(LSTM_prediction,type = "l",col=cols[5],lty=ltys[5],ylab="",xlab="",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1)) #LSTMはオレンジ線で描く
par(new=T) #図を重ねる
plot(cNN_prediction,type = "l",col=cols[3],lty=ltys[6],ylab="",xlab="",xlim=c(2014.01,2018.06),ylim=c(-0.2,-0.1)) #cNNは紫線で描く

par(new=T) #図を重ねる
legend("topleft", legend = labels, col = cols, lty = ltys) #図の左上に凡例を付ける。


##テストデータにおける累積2乗誤差のプロットの作成##

#累積2乗誤差を計算する関数
cum_sqerr <- function(pred,act_values){
  cum_sqerr <- c()
  pre_cum_sqerr <- 0
  for (t in 1:length(pred)) {
    sqerr <- (pred[t] - act_values[t])^2
    cum_sqerr[t] <- sqerr + pre_cum_sqerr
    pre_cum_sqerr <- cum_sqerr[t]
  }
  return(cum_sqerr)
}

#累積2乗誤差の計算
AR_cum_sqerr <- cum_sqerr(AR_prediction,arframe[istest, "Real"]) %>% ts(start=c(2004.01), frequency=12) 
AR_cum_sqerr <-  ts(cum_sqerr(AR_prediction,arframe[istest, "Real"]),start=c(2004.01), frequency=12) 
RNN_cum_sqerr <- cum_sqerr(RNN_prediction,arframe[istest, "Real"]) %>% ts(start=c(2004.01), frequency=12)
NN_cum_sqerr <- cum_sqerr(NN_prediction,arframe[istest, "Real"]) %>% ts(start=c(2004.01), frequency=12)
LSTM_cum_sqerr <- cum_sqerr(LSTM_prediction,arframe[istest, "Real"]) %>% ts(start=c(2004.01), frequency=12)
cNN_cum_sqerr <- cum_sqerr(cNN_prediction,arframe[istest, "Real"]) %>% ts(start=c(2004.01), frequency=12)


#累積2乗誤差のプロット
plot(AR_cum_sqerr,type = "l",ylim = c(0,0.2),col=cols[2],lty=ltys[2],ylab="Cumulative Squared Error",xlab="year",xlim=c(2014.01,2018.06))  #線形回帰は青線で描く
par(new=T) #図を重ねる
plot(RNN_cum_sqerr,type = "l",ylim = c(0,0.2),col=cols[3],lty=ltys[3],ylab="",xlab="",xlim=c(2014.01,2018.06)) #RNNは赤線で描く
par(new=T) #図を重ねる
plot(NN_cum_sqerr,type = "l",ylim = c(0,0.2),col=cols[4],lty=ltys[4],ylab="",xlab="",xlim=c(2014.01,2018.06)) #NNは緑線で描く
par(new=T) #図を重ねる
plot(LSTM_cum_sqerr,type = "l",ylim = c(0,0.2),col=cols[5],lty=ltys[5],ylab="",xlab="",xlim=c(2014.01,2018.06)) #LSTMはオレンジ線で描く
par(new=T) #図を重ねる
plot(cNN_cum_sqerr,type = "l",ylim = c(0,0.2),col=cols[3],lty=ltys[6],ylab="",xlab="",xlim=c(2014.01,2018.06)) #cNNは紫線で描く

par(new=T) #図を重ねる
legend("topleft", legend = labels[2:6], col = cols[2:6], lty = ltys[2:5]) #図の左上に凡例を付ける。



