#金融資料分析套件
library(quantmod)

#清空佔存
rm(list = ls())

#帶入標的
STK = get(getSymbols("2330.TW"))
STK = na.omit(STK)

#獲利初始值設定
PL = setNames(rep(0,length(Cl(STK))),time(STK))

#保有股數&成本初始值設定
PZ=long2=0
#資本額(千)
long=10000

#交叉條件
#短天期大於長天期均線 True:1 False:0
Cross = SMA(Cl(STK),5) > SMA(Cl(STK),10) | SMA(Cl(STK),5) > SMA(Cl(STK),20) | SMA(Cl(STK),10) > SMA(Cl(STK),20)
#遺漏值填補
Cross[is.na(Cross)]=TRUE

#MA設定
ma=5
m=ma+1

#策略實作
#黃金交叉收盤加碼買進；直到死亡交叉收盤全數賣出
while ( m < nrow(STK)){
  #黃金交叉
  if (Cross[m-1]==0 && Cross[m]==1){
    #黃金交叉到死亡交叉的期間
    while (Cross[m]==1 && m <nrow(STK)){
      if (sd(Cl(STK)[(m-5):(m-1)])<3.8 && long>=0 && long>=Cl(STK)[m]){
        #買進手續費=成本*0.1425%
        long=long-ceiling(as.numeric(Cl(STK)[m])*(1+0.001425))
        long2=long2+ceiling(as.numeric(Cl(STK)[m])*(1+0.001425))
        PZ=PZ+1
      }
      m=m+1
    }
    #死亡交叉
    if (Cross[m-1]==1 && Cross[m]==0){
      if (sd(Cl(STK)[(m-5):(m-1)])<4.7){
        #賣出手續費=成本*0.1425%
        #賣出證交稅=成本*0.3%
        fee=ceiling(as.numeric(Cl(STK)[m])*PZ*0.004425)
        long=as.numeric(Cl(STK)[m])*PZ+long-fee
        PL[m]=as.numeric(Cl(STK)[m])*PZ-long2-fee
        PZ=0
        long2=0
      }
    }
  }
  m=m+1
}

#綜合條件
(WR = length(PL[PL>0])/length(PL[PL!=0])) #勝率
(Odds = mean(PL[PL>0])/abs(mean(PL[PL<0]))) #賺賠比
(EV = WR*Odds+(1-WR)*(-1)) #期望值
(PF = sum(PL[PL>0])/abs(sum(PL[PL<0]))) ###獲利因子

#綜合圖表
DD=cumsum(PL)-cummax(cumsum(PL))
yRang = range(DD,cumsum(PL))
plot(DD,type="h",col="green",ylim=yRang, main="台積電(2330)  – Yahoo Finance")
par(new=T)
plot(cumsum(PL), col="blue", lwd=2, type="l", ylim=yRang , ylab="")
points(which(DD == 0), cummax(cumsum(PL))[which(DD == 0)], pch=4, col="red")

#其他
(MDD=min(DD)) #MDD:歷史區間的最大回檔
(max(sort(diff(which(DD==0))))) #DDD

#總交易次數
length(PL[PL!=0])

which.max(PL) #新高時間
end=as.Date("2022-12-24")
begin=as.Date("2020-01-08")
(difftime(end,begin,units="days")) #距離上次創新高時間
(difftime(end,begin,units="days")) #距離上次創新高時間