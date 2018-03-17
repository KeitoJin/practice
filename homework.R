#銀行の顧客データと過去のキャンペーンデータを元に定期預金に関して新しくキャンペーンを打った際の跳ね返りを予測、売上を最大化するリストの作成
#データ詳細はこちらhttps://archive.ics.uci.edu/ml/datasets/bank+marketing#
#電話1件で500円の費用、申込み1件で2,000円の利益として売上を最大化

#顧客データ（CSV）の読み込み

bank_data <- read.csv("homework_data/bank_marketing_train.csv")

#データの中身の確認
head(bank_data)

#非説明変数の値をyes/noから1/0に変換
bank_data$y <-ifelse(bank_data$y=="yes", 1, 0)

#変換できているか再度データの中身の確認
head(bank_data)

#元データから学習用データとテスト用データに7:3でランダムに分割
train_idx<-sample(c(1:dim(bank_data)[1]), size = dim(bank_data)[1]*0.7) 
train<-bank_data[train_idx, ] #上記インデックスで指定して学習用データを作成
test<-bank_data[-train_idx, ] #インデックスに”-”をつけると”それ以外”という意味になる。残った３割をテストデータに

#データのサマリを確認
summary(train)

#被説明変数が「キャンペーンを申し込む/申し込まない」の2値なため、すべての説明変数を使ってロジステック回帰
bank_data.lr<-glm(y~.,data=train, family="binomial")
summary(bank_data.lr)
AIC(bank_data.lr)
#ざっと見たところ有意な説明変数で効いているのが、退職者/学生、学部卒の人
#学生は将来を見越して預金？大学生向けキャンペーンが多い？、退職者は将来のための預金
#年齢的に20代くらいと60代以降くらいが高そう、ちゃんとやるならageを年代別にカテゴリデータに変えたほうが良さそう（一旦やらない、あとでやる）

#step関数を使ってAICを最小化
bank_data.lr2<-step(bank_data.lr)

#モデルのサマリーの確認
summary(bank_data.lr2)

#指数をとって各変数のオッズ比を確認
exp(bank_data.lr2$coefficients)
#やっぱり、退職者と学生効いてる。あと月によってかなり変わる→アメリカのデータ月によってイベントとかなにかありそう（ドメイン知識の確認の必要あり）
#さっくり作ってここでモデル作成は終了。ここからテストデータで確認

#上記モデルをもとにテストデータの顧客ごとの申し込み確率をscoreとして計算
score<-predict(bank_data.lr2, test, type = "response")


#上記で作成したモデルをもとに、電話一件で500円のコスト、一件申し込みで2000円の収益で売上を算定する関数の作成
#1.上記モデルでテストデータの人の申し込む確率を算定、そのうちxのところで事前に規定した確率以上になったら電話をかけることにする
#2.テストデータの実測値と照らし合わせconfusion matrixを作成
#3.confusion matrixをもとにかけた件数のうち実際に獲得できた売上-かけた件数分のコストで利益を出す、という関数

profit<-function(x){
  ypred_flag<-ifelse(score > num_list[x], 1, 0)
  conf_mat<-table(test$y, ypred_flag )
  conf_mat
  
  attack_num<-conf_mat[3] + conf_mat[4]
  attack_num
  
  your_cost <- attack_num * 500
  your_cost
  
  conf_mat[4] 
  expected_revenue<-conf_mat[4] * 2000
  expected_revenue
  
  return(expected_revenue - your_cost)
}

#上記関数をもとに、利益の最大値をfor文で算定
#電話をかける確率のリストを売上がプラマイ０になる25%をめどに0.001刻みで作成
num_list<-seq(0.1, 0.25, by = 0.001)

max_profit<-profit(1)
max_i<-1
for (i in 1:length(num_list)){
  if(profit(i)>max_profit){
    max_profit<-profit(i)
    max_i<-i
  }
}
#最大の売上
max_profit
#売上最大になるしきい値
num_list[max_i]