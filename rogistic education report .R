###############################################################################

#########  教育分析（藤原）最終レポート　専門職のロジスティック回帰　

df <- read.csv("pj010rogi.csv",header=T)
head(df, n = 3)
#dat <- df[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70)]#3列が目的変数。他が説明変数。7natluniv,10indcomp,16 offices
dat <- df[,c(1:70)]
#dat <- scale(dat) #scale関数で標準化する

summary(dat)

install.packages("psych")
library(psych)
describe(dat)
###################################  母　基本全部　##########################

ans <- glm(as.factor(m1_prof) ~ 
#ans <- glm(as.factor(m1_mana) ~ 
#ans <- glm(as.factor(c1_prof) ~ 
#ans <- glm(as.factor(c1_mana) ~ 
  
           + scale(m1_35_1) + scale(m1_m_prof) + scale(m1_f_prof) 
        #  + scale(m1_35_1) + scale(m1_m_mana) + scale(m1_f_mana) 

           + scale(m1_18a)  + scale(m1_18b)  + scale(m1_18c)  + scale(m1_18d)  + scale(m1_18e)  + scale(m1_18f)  + scale(m1_18g)  + scale(m1_18h)  + scale(m1_18i)  + scale(m1_18j)  + scale(m1_18k)  + scale(m1_18l) 
        #  + scale(c1_6a)   + scale(c1_6b) 	 + scale(c1_6c)   + scale(c1_6d) 	 + scale(c1_6e) 	 + scale(c1_6f) 	 + scale(c1_6g)  + scale(c1_6h)  + scale(c1_6i)  + scale(c1_6j) 	+ scale(c1_6k)	+ scale(c1_6l) + scale(c1_6m) + scale(c1_6n) + scale(c1_6o) 

           + scale(c1_15a) + scale(c1_15b)+ scale(c1_15c)+ scale(c1_15d)+ scale(c1_15e)+ scale(c1_15f)+ scale(c1_15g)+ scale(c1_15h)+ scale(c1_15i)+ scale(c1_15j)
        #  + scale(male)	+	scale(female)
           + scale(maleselfprof) +	scale(malemamprof)  +	scale(malemprof)  +	scale(malefprof)	+ scale(femselfprof) + scale(femmamprof) + scale(femmprof) + scale(femfprof)	
        #  + scale(maleselfmana) +  scale(malemammana) 	+ scale(malemmana)	+ scale(malefmana)  + scale(femselfmana) + scale(femmammana) + scale(femmmana) + scale(femfmana)
           ,family = binomial(link = logit),
           data = dat)

s.ans <- summary(ans)
s.ans

##################################  （１）母親　専門職 希望　やり直し

## ロジスティック回帰（規準化後）
ans <- glm(as.factor(m1_prof) ~ 
           + scale(m1_18a) + scale(m1_18g)  + scale(m1_18l) 
           + scale(c1_15d)+ scale(c1_15g) + scale(c1_15i)   ## i j どちらでもよい  
           + scale(female) 	
           + scale(malemprof) + scale(malefprof)  + scale(femmprof)       
        #  + scale(maleselfmana) + scale(malemmana)  + scale(malefmana)   + scale(femselfmana)  + scale(femmammana) + scale(femmmane) + scale(femfmana)
           ,family = binomial(link = logit),
           data = dat)

## ロジスティック回帰（規準化前）
ans <- glm(as.factor(m1_prof) ~ 
           + m1_18a + m1_18g  + m1_18l 
           + c1_15d + c1_15g  + c1_15i  
           + female 	
           + malemprof + malefprof  + femmprof       
           #  + scale(maleselfmana) + scale(malemmana)  + scale(malefmana)   + scale(femselfmana)  + scale(femmammana) + scale(femmmane) + scale(femfmana)
           ,family = binomial(link = logit),
           data = dat)

###################################  （１）-２比較　母　管理職　希望　やり直し　##########################
## ロジスティック回帰（規準化後）
ans <- glm(as.factor(m1_mana) ~ 
           + scale(m1_18c)  + scale(m1_18f) + scale(m1_18j)     
           + scale(c1_15e) + scale(c1_15f)+ scale(c1_15j)   #j ⇔ i（技術家庭） はダメ
           +	scale(female)
           # + scale(maleselfprof) +	scale(malemamprof)  +	scale(malemprof)  +	scale(malefprof)	+ scale(femselfprof) + scale(femmamprof) + scale(femmprof) + scale(femfprof)	
           + scale(malefmana) 
           
           ,family = binomial(link = logit),
           data = dat)

## ロジスティック回帰（規準化前）
ans <- glm(as.factor(m1_mana) ~ 
           + m1_18c  + m1_18f + m1_18j     
           + c1_15e + c1_15f + c1_15j
           + female
           # + scale(maleselfprof) +	scale(malemamprof)  +	scale(malemprof)  +	scale(malefprof)	+ scale(femselfprof) + scale(femmamprof) + scale(femmprof) + scale(femfprof)	
           +  malefmana 
           
           ,family = binomial(link = logit),
           data = dat)


###################################################################

##########  （２）本人　専門職 #######
## ロジスティック回帰（規準化後）

ans <- glm(as.factor(c1_prof) ~ 
             
           + scale(c1_6b) + scale(c1_6g) +scale(c1_6o) 
           + scale(c1_15b) + scale(c1_15e)+ scale(c1_15f) + scale(c1_15g) +scale(c1_15i) + scale(c1_15j)  ## i（技術家庭）とj（美術）は交換可能
           + scale(female)
           +	scale(malemprof)  +	scale(malefprof) + scale(femmprof) 
           
           ,family = binomial(link = logit),
           data = dat)

## ロジスティック回帰（規準化前）

ans <- glm(as.factor(c1_prof) ~ 
             
           + c1_6b  + c1_6g  +　c1_6o 
           + c1_15b + c1_15e + c1_15f + c1_15g + c1_15i
           + female
           + malemprof +	malefprof + femmprof 
           
           ,family = binomial(link = logit),
           data = dat)

##########  （２）-2 本人　管理職 #######
## ロジスティック回帰（規準化後）

ans <- glm(as.factor(c1_mana) ~ 
           + scale(c1_6a)   
           + scale(c1_15e)+ scale(c1_15h)
           + scale(female)
           + scale(malemmana)	+ scale(femfmana)
           
           ,family = binomial(link = logit),
           data = dat)

## ロジスティック回帰（規準化前）

ans <- glm(as.factor(c1_mana) ~ 
           + c1_6a   
           + c1_15e + c1_15h
           + female
           + malemmana + femfmana
           
           ,family = binomial(link = logit),
           data = dat)

#########################################

##########  （２）本人　専門職 やり直し　#######
## ロジスティック回帰（規準化後）

ans <- glm(as.factor(c1_prof) ~ 
             
    　    + scale(c1_6b) + scale(c1_6g) +scale(c1_6o) 
          + scale(c1_15b) + scale(c1_15e)+ scale(c1_15f) + scale(c1_15g) + scale(c1_15i)
           + scale(female)
           +	scale(malemprof)  +	scale(malefprof) + scale(femmprof) 
           
           ,family = binomial(link = logit),
           data = dat)

##############################################################

s.ans <- summary(ans)
s.ans


coe <- s.ans$coefficient  #$coefficientで回帰係数、標準誤差、z値、p値を抽出
coe  #左から、回帰係数・標準誤差(SE)・z値・p値

#オッズ比とオッズ比の95％信頼区間
RR <- exp(coe[,1])  # （係数の）対数オッズ比を指数関数でオッズ倍率に戻す
RRlow <- exp(coe[,1]-1.96*coe[,2])
RRup <- exp(coe[,1]+1.96*coe[,2])

#AICと要素数を抽出
N <- nrow(df)
aic <- AIC(ans)
r <- s.ans$r.squared
adjr<- s.ans$adj.r.squared
f<- s.ans$fstatistic

coe
r
adjr
f

RR
RRlow
RRup
logLik(ans)
deviance(ans)  
aic
N
DescTools::PseudoR2(ans, "all")





