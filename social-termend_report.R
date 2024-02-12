## 2023/01/18 
###　社会調査　最終課題　期末レポート
## 37-207172 小林秀二（こばやし　ひでじ）　工学系研究科先端学際工学専攻

install.packages("rlang")
install.packages("vctrs")
install.packages("stargazer")

remove.packages("vctrs")
remove.packages("stargazer")

# ライブラリ
library(readr)
library(tidyverse)
library(magrittr)
library(car)
library(estimatr)
library(skimr)
library(stargazer)
library(janitor)
library(dplyr)
library(rlang)

# データ
PY110 <- read_csv("data/PY110.csv")
PM110 <- read_csv("data/PM110.csv")
JLPS <- bind_rows(PY110,PM110,.id = "YM")  # データを積む
nrow(JLPS)


# 変数
source("Social_survey_variables_20211205.R")

# 変数を作成する（変数の選択，欠損値の処理，ケースの選択）

JLPS <- JLPS %>% 
  mutate(citysize =  car::recode(SIZE,"1:2=1;3:4=0;else=NA"),
         work_ex = car::recode(ZQ03_3,"1=1;2=0;else=NA"),
         goodlife_now = car::recode(ZQ12,"5=0;4=1;3=2;2=3;1=4;else=NA"),  # reverse
         goodlife_future = car::recode(ZQ62,"5=0;4=1;3=2;2=3;1=4;else=NA"),  # reverse
         child = car::recode(ZQ14_3,"1=1;2=0;else=NA"),
         d_ZQ14_33B = car::recode(ZQ14_33B,"1=1;else=0"),  
         d_ZQ14_33C = car::recode(ZQ14_33C,"1=1;else=0"),  
         d_ZQ14_33D = car::recode(ZQ14_33D,"1=1;else=0"),  
         d_ZQ14_33E = car::recode(ZQ14_33E,"1=1;else=0"),  
         d_ZQ14_33F = car::recode(ZQ14_33F,"1=1;else=0"),  
         d_ZQ14_33G = car::recode(ZQ14_33G,"1=1;else=0"),  
         d_ZQ14_33H = car::recode(ZQ14_33H,"1=1;else=0"),  
         d_ZQ14_33I = car::recode(ZQ14_33I,"1=1;else=0"),  
         edu3 = car::recode(ZQ23A,"1:2=1;3:4=2;5:6=3;else=NA"),#本人　高卒／専門・短大卒／大卒
         edu3 = factor(edu3, 
                       levels = 1:3,
                       labels = c("JH/HS", "PTC/JC", "Univ")),  
         sedu3 = car::recode(ZQ23B,"1:2=1;3:4=2;5:6=3;else=NA"),#配偶者　高卒／専門・短大卒／大卒
         sedu3 = factor(sedu3, 
                        levels = 1:3,
                        labels = c("JH/HS", "PTC/JC", "Univ")),   
         childcare  = d_ZQ14_33B+d_ZQ14_33C+d_ZQ14_33D+d_ZQ14_33E+d_ZQ14_33F+d_ZQ14_33G+d_ZQ14_33H+d_ZQ14_33I,
         h_asset = car::recode(ZQ49,"1=1;2=25;3=75;4=200;5=400;6=750;7=2000;8=4000;9=5000;else=NA"),
         r_ZQ33D = car::recode(ZQ33D,"c(4,9)=0;3=1;2=2;1=3;else=NA"),  # reverse
         r_ZQ33F = car::recode(ZQ33F,"c(4,9)=0;3=1;2=2;1=3;else=NA"),  # reverse
         work_hesitate = r_ZQ33D + r_ZQ33F,
         
         hope = car::recode(ZQ36,"1=5;2=4;3=3;4=2;5=1;else=NA"),  # reverse
         r_ZQ54A = car::recode(ZQ54A,"c(6,8)=0;5=1;4=2;3=3;2=4;1=5;else=NA"),  # reverse
         r_ZQ54B = car::recode(ZQ54B,"c(6,8)=0;5=1;4=2;3=3;2=4;1=5;else=NA"),  # reverse
         r_ZQ54C = car::recode(ZQ54C,"c(6,8)=0;5=1;4=2;3=3;2=4;1=5;else=NA"),  # reverse
         hubby_share = r_ZQ54A + r_ZQ54B + r_ZQ54C,
         m_work = car::recode(ZQ22B1,"1:2=0;3:9=1;else=NA"),
         health = car::recode(ZQ25,"5=4;4=3;3=2;2=1;1=0;else=NA"),  # min = 0, max = 4
         own_house = car::recode(ZQ44,"1:2=1;3:8=0;else=NA"),
        
         d_ZQ61_A = car::recode(ZQ61_A,"1=1;else=0"),  
         d_ZQ61_B = car::recode(ZQ61_B,"1=1;else=0"),  
         d_ZQ61_C = car::recode(ZQ61_C,"1=1;else=0"),  
         d_ZQ61_D = car::recode(ZQ61_D,"1=1;else=0"),  
         future_willing = d_ZQ61_A + d_ZQ61_B + d_ZQ61_C + d_ZQ61_D,
         
         housex = car::recode(ZQ45,"2=1;else=0"),
         h_asset0 = car::recode(ZQ49,"1=0;2=25;3=75;4=200;5=400;6=750;7=2000;8=4000;9=5000;else=NA"),
         housewife = car::recode(ZQ55_1,"1:2=0;3:8=1;else=NA"),
         partwife = car::recode(ZQ47A,"1:4=1;5:13=0;else=NA"),
         housewife = car::recode(ZQ47A,"1=1;2:4=2;5:13=3;else=NA"),#専業／パート／フルワーク
         housewife = factor(housewife, 
                       levels = 1:3,
                       labels = c("full-wife", "part-work", "full-work")),  
         income0 = car::recode(ZQ47A,"1=0;2=12.5;3=50;4=100;5=200;6=300;7=400;8=500;9=700;10=1000;11=1500;12=2000;13=2250;else=NA"),
         s_income0 = car::recode(ZQ47B,"1=0;2=12.5;3=50;4=100;5=200;6=300;7=400;8=500;9=700;10=1000;11=1500;12=2000;13=2250;else=NA"),
         h_income0 = car::recode(ZQ47C,"1=0;2=12.5;3=50;4=100;5=200;6=300;7=400;8=500;9=700;10=1000;11=1500;12=2000;13=2250;else=NA")
                 )
ifelse(JLPS$childcare >=1,1,0)
ifelse(JLPS$hubby_share >=1,1,0)
ifelse(JLPS$future_willing >=1,1,0)

nrow(JLPS)
# 欠損値を含める
length(JLPS$marry)
# 欠損値を含めない
sum(!is.na(JLPS$marry))
# 既婚
sum(JLPS$marry == 0 , na.rm = T)

## seduy sedu3 を入れるとその時点で既婚者のみとなることに注意
d <- JLPS %>% 
  dplyr::select(PanelID,partwife,housewife,income0,s_income0,seduy,sedu3,age,gender_role,mental_health,hope,work_hesitate,future_willing,hubby_share,eduy,edu3,gender,marry,citysize,own_house,housex, goodlife_now, goodlife_future,child,childcare,m_work) %>%
  drop_na()
nrow(d)

d_Female <- d %>% dplyr::filter(gender == "Female") # 女のみ（既婚女性）
nrow(d_Female)
sum(d_Female$marry == 1 , na.rm = T)

d_male <- d %>% dplyr::filter(gender == "Male") # 男のみ
nrow(d_male)
sum(d_male$marry == 1 , na.rm = T)

dd_Female <- JLPS %>% dplyr::filter(gender == "Female") # 女のみ（未婚含む女性）
nrow(dd_Female)
sum(dd_Female$marry == 1 , na.rm = T)

d_Female_marry <- d_Female %>% dplyr::filter(marry == 1) # 更に既婚者のみ（不要）
nrow(d_Female_marry)

sum(d_Female_marry$income0 >= 0 , na.rm = T)
sum(JLPS$income0 >= 0 , na.rm = T)

#記述統計量を確認する．
skim(d_Female_marry)

#関連を図示する．
d_Female_marry %>% ggplot(aes(x = s_income0, y = income0)) +
  labs(x = "hubby-income", y = "wife-income") + 
  geom_point() + 
  geom_smooth(method = lm)

#回帰分析
# 回帰分析
fit1 <- lm(partwife ~ s_income0 +child + work_hesitate , data = d_Female_marry)
fit2 <- lm(partwife ~ s_income0 +child + work_hesitate + childcare + eduy + gender_role +hubby_share + future_willing , data = d_Female_marry)
fit3 <- lm(partwife ~ s_income0 +child + work_hesitate + childcare + eduy + gender_role +hubby_share + future_willing + goodlife_now + goodlife_future , data = d_Female_marry)
fit4 <- lm(partwife ~ s_income0 + childcare + eduy + gender_role +hubby_share + future_willing , data = d_Female_marry)
stargazer(fit1, fit2,fit3, type = "text", 
          column.labels = c("Model 1", "Model 2", "Model 3"))
stargazer(fit1, fit2, type = "text", 
          column.labels = c("Model 1", "Model 2"))


## 二値のときはlmでやってからstargazerにc(starprep)のオプションを付ける
stargazer(fit1, fit2, fit3, se = c(starprep(fit1,fit2,fit3)), type = "text", 
          column.labels = c("Model 1", "Model 2", "Model 3"))
stargazer(fit1, fit2, se = c(starprep(fit1,fit2)), type = "text", 
          column.labels = c("Model 1", "Model 2"))

stargazer(fit1, fit4,fit3, se = c(starprep(fit1,fit4,fit3)), type = "text", 
          column.labels = c("Model 1", "Model 4", "Model 3"))


## 二値の場合の線形確率モデル lm ⇒lm_robust を使う
fit_robust1 <- lm_robust(partwife ~ s_income0 +child + work_hesitate, data = d_Female_marry)
fit_robust2 <- lm_robust(partwife ~ s_income0 +child + work_hesitate + gender_role + eduy + childcare +hubby_share + future_willing , data = d_Female_marry)
fit_robust3 <- lm_robust(partwife ~ s_income0 +child + work_hesitate + gender_role + eduy + childcare +hubby_share + future_willing  + goodlife_now + goodlife_future , data = d_Female_marry)

summary(fit_robust1) 
summary(fit_robust2) 
summary(fit_robust3) 

############################

############################
# パッケージ
library(corrr)
library(Hmisc)
library(janitor)
library(vcd)


# パッケージ
#library(tidyverse)
#library(skimr)
#library(car)
#library(magrittr)
library(corrr)
library(Hmisc)
library(janitor)
library(vcd)



xtabs(~ edu3 + sedu3, data = d_Female_marry)

xtabs(~ edu3 + housewife, data = d_Female_marry)


library(gtsummary)
cro_tab <- d_Female_marry %>% 
  tbl_cross(row = edu3, col = housewife)
cro_tab

cro_tab <- d_Female_marry %>% 
  tbl_cross(row = edu3, col = housewife,
            label = list(edu3 ~ "学歴３分類", housewife ~ "既婚女性３分類"),
            missing = "no",
            margin_text = "合計",
            percent = "row")
cro_tab


# テーブルとしてオブジェクトに格納
tab <- table(d_Female_marry$edu3, d_Female_marry$housewife)
tab


# 行パーセントのクロス表
# パイプ演算子を用いる
# Step1: テーブルを作る
# Step2: 行に周辺度数を加える
# Step3: 行パーセントを求める
# Step4: 列に周辺度数を加える
# Step5: 値をまるめ，100をかける
tab %>% addmargins(1) %>% prop.table(1) %>% addmargins(2) %>% round(3)*100

## 総パーセント表で条件付き確率を確認
addmargins(prop.table(tab)) %>%round(3)


## モザイクプロット
library(vcd)
mosaic(~ edu3 + housewife ,shade = TRUE, data = d_Female_marry)

## クラメールのV
assocstats(tab)#vcdパッケージ

##########################  ２×２　参考　##############################

## 総パーセント表で条件付き確率を確認
addmargins(prop.table(tab)) %>%round(3)
## 大卒を条件としたときの失業する条件付き確率
addmargins(prop.table(tab))[2,2]/ addmargins(prop.table(tab))[2,3]
## 非大卒を条件としたときの失業する条件付き確率
addmargins(prop.table(tab))[1,2]/ addmargins(prop.table(tab))[1,3]

## モザイクプロット
library(vcd)
mosaic(~ edu2 + housewife,shade = TRUE, data = JLPS)

## ユールのQ係数
(tab[1,1]*tab[2,2] - tab[1,2]*tab[2,1])/ (tab[1,1]*tab[2,2] + tab[1,2]*tab[2,1])

library(psych)
Yule(tab)

## ϕ係数
(tab[1,1]*tab[2,2] - tab[1,2]*tab[2,1])/ (sqrt((tab[1,1]+tab[1,2])) *sqrt((tab[2,1]+tab[2,2])) *sqrt((tab[1,1]+tab[2,1])) *sqrt((tab[1,2]+tab[2,2])))#桁あふれするため平方根を先に計算
#ダミー変数0,1にして相関係数を計算すると同じ値となる．
cor(as.numeric(PY110$edu2) -1,as.numeric(PY110$unemp) -1, use = "complete")

## オッズ比
非大卒が失業するオッズ
tab[1,2]/tab[1,1]

大卒が失業するオッズ
tab[2,2]/tab[2,1]

##################################################

#第１１回　相関係数

パッケージ
#library(tidyverse)
#library(magrittr)
library(skimr)

x= d_Female_marry$s_income0
y= d_Female_marry$income0

d1 <- tibble(x,y)
d1

#記述統計量の確認
skim(d1)

install.packages(ggpmisc)
library(tidyverse)
library(ggpmisc)

#散布図と回帰直線
d1 %>% ggplot(aes(x = x, y = y)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                 aes(label = paste(stat(eq.label))),
                 parse = TRUE)
  
  
  


#相関係数，検定統計量，自由度，臨界値，p値を求める
# サンプルサイズ（2つの変数に欠損がないケースの数）
n <- nrow(d1)
# 相関係数
r <- cor(d1$x, d1$y)
# 自由度df
df <- n - 2
# T値を求める
T_val <- (r * sqrt(n - 2)) / sqrt(1 - r^2)
T_val

# 臨界値を求める
cv <- qt(c(0.025,0.975), df = df)
# p値を求める
p <- pt(abs(T_val), df, lower.tail = FALSE) * 2
# 結果をまとめる
list("相関係数" = r, "サンプルサイズ" = n, "自由度" = df, "T値" = T_val, "臨界値" = cv, "p値" = p)

# 相関係数の検定
cor.test(d1$x,d1$y)


## 学歴ごとの相関係数を計算
d_Female_marry %>% 
  group_by(edu3) %>% 
  summarise(r = cor(x, y, use = "na.or.complete"))


# 大陸別にまとめる
df <- group_by(d_Female_marry,edu3)
df
## 散布図に直線（大陸別）
ggplot(data = df, aes(x =x , y =y )) +
  geom_jitter(alpha = 0.3)+
  geom_smooth(method = "lm", se = FALSE) + 
  facet_wrap(~edu3, ncol=2) +
  labs(x = "hubby-income",
       y = "wife-income") + 
  theme_minimal() +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                      aes(label = paste(stat(eq.label))),
                      parse = TRUE)

###############################

#  箱ヒゲ図　妻学歴ごとの収入 
plot(d_Female_marry$edu3, d_Female_marry$income0, xlab="education", ylab="income")

# ヒストグラムを描く
hist(d_Female$income0,breaks=seq(0,2000,by=100),ylim=c(0,1800),main= "既婚女性",xlab="Female_marry-income",ylab="Frequency")　## 今回対象の既婚女性
hist(dd_Female$income0,ylim=c(0,1800),main = "全女性",xlab="Female-income", ylab="Frequency") ## 全女性

## ヒストグラムの重ね書き ggplot　数が一致している必要あり
dat <- tibble(s1 = d_Female$income0, s2 = dd_Female$income0)
dat %<>% gather(key, value, s1,s2) 

ggplot(dat, aes(x=value,fill=key)) +
  geom_histogram(position = "identity", alpha = 0.5)

ggplot(dat, aes(x=income0,fill=marry)) +#fillでsexと指定して，sexで色分けする．
  geom_histogram(breaks = seq(min(dat$income0)-5,max(dat$income0)+5,1),position = "identity", alpha = 0.8 )#position = "identity"としてsexごとに独立のヒストグラムを描く．alphaは透過率

###########　記述統計　###################### 

## サマリー
summary(d_Female_marry)

## カッコよい要約表
skim(d_Female_marry)

## 論文に載せる記述統計
#library(stargazer)
#install(summarytools)
library(summarytools)
descr(d_Female_marry, transpose = TRUE)

# 変数全部
d_Female_marry %>% 
  dfSummary() %>% 
  summarytools::view()

d_Female_marry %>% 
  dplyr::select(age,gender_role,work_hesitate,future_willing,hubby_share,child,childcare) %>%
  dfSummary() %>% 
  summarytools::view()

dplyr::select(partwife,housewife,income0,eduy,edu3,s_income0,seduy,sedu3,age,gender_role,work_hesitate,future_willing,hubby_share,child,childcare) %>%
dplyr::select(partwife,housewife,income0,eduy,edu3,s_income0,seduy,sedu3) %>%
dplyr::select(age,gender_role,work_hesitate,future_willing,hubby_share,child,childcare) %>%
  
  
  
## describe
install.packages("psych")  
library(psych)
describe(d_Female_marry)
## グループごとに出す
describeBy(d_Female_marry$income0,d_Female_marry$edu3)

















