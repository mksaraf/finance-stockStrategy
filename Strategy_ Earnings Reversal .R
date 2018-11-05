## Strategy : Reversal During Earnings-Announcements ###
## reversal in price of an asset occurs due to investors' overreaction to asset-related news and 
#the subsequent price correction
##  average returns in the 3-day window between t-4 and t-2, where t is the day of earnings announcement. The investor goes long on the bottom quintile (past losers) and short on the top quintile (past winners) and 
# holds the stocks during the 3-day window between t-1, t, and t+1.


library(jsonlite)
library(ggplot2)
library(data.table)
## INPUT: earn_stk dataset (see Driver)

filterTickers <- function(frame,cap) {
  frame[cap_mm>cap]
}


## NOT in Use 
prepareData<-function(asset,startDate=Sys.Date()-365){  ## change: expected move

  endDate = Sys.Date()
  startDate = as.Date(startDate  )
  
  rollingAvgShort = 20 ;   rollingAvgLong = 175
  proximity = 0.1 # tolerance: distance threshold betwee long and short averages
  slopeSigF = 0.001 # Slope cannot be less than this number to be considered
  holdDays  = c(1:3) ;   evalDays = c(0:-4)
  
  offset = 1 # start of prediction window
  SD =1 # of std deviations to use for significance

  
  allPrices = getPrices(asset,startDate ,endDate ); prices =  Cl(allPrices)
  returns = ROC(prices,type = 'discrete')
  
  yrRange =  rollmaxr(prices,rollingAvgLong,fill=NA) - rollapplyr(prices,rollingAvgLong,min,fill=NA) # seriesHi(data[(.N-255):.N,c(close)])-seriesLo(data[(.N-255):.N,c(close)])
  pY = (prices-rollapplyr(prices,rollingAvgLong,min,fill=NA))/yrRange
  atr = ATR(allPrices,n=rollingAvgLong)
  
  vlty= atr$atr/prices ; avg = EMA(prices,n=rollingAvgShort);  
  pB = 0.5 + (prices-avg)/(4*vlty)  # This is percent Bollinger
  pB = EMA(pB,rollingAvgShort/4)

  
  volRatio = EMA(Vo(allPrices),3)/mean(Vo(allPrices),na.rm = T) # This is ratio (daily): day/long term-1
  vltyRatio = (ATR(allPrices,n=15)$atr/prices)/vlty
  volSlope = ROC(EMA(Vo(allPrices),3))
  # delt = Delt(pB)
  rollmeanShort = EMA(prices,rollingAvgShort) ;  rollmeanLong = EMA(prices,rollingAvgLong)
  
  slopeShort= ROC(rollmeanShort,n=5,type = 'discrete') ### Slope Smoothed over 5 days # data[data[,slopeShort]*shift(data[,slopeShort])<0,.(dates,close,slopeShort)]
  slopeLong= ROC(rollmeanLong,n=1,type = 'discrete')
  ROC.slopeLong= ROC(slopeLong,n=1,type = 'discrete')
  
  MFI = MFI(Cl(allPrices),Vo(allPrices),n=7)
  
  gain15 = rollapply(prices,width=15,align = "right",FUN = function(x){tail(x,1)/as.numeric(head(x,1))})
  BSRatio = rollapplyr(OpCl(allPrices),width=44,FUN=function(x){sum( x[x>0] )/sum( abs(x[x<0]) )})  # Buy Sell weighted ratio for 10 days
  
  pricesSPY = Cl(rbind(getSymbols("SPY",from = startDate, to = Sys.Date(),env=NULL)[,1:5],xts(order.by = Sys.Date(),getQuote('SPY')[,c("Open","High","Low","Last","Volume")])))
  minX = min(nrow(prices),nrow(pricesSPY))
  pSPY = pricesSPY[(1+nrow(pricesSPY)-minX):nrow(pricesSPY),]
  
  pA = prices[(1+nrow(prices)-minX):nrow(prices),]

  dt=data.table(dates=index(prices),allPrices=allPrices,returns,vltyRatio=vltyRatio,slopeShort,slopeLong,slopeCross=slopeShort-slopeLong,
                ROC.slopeLong,regime=rollmeanShort-rollmeanLong,pB,pY=pY,MFI,volRatio,gain15,BSRatio,volSlope,symbol=asset)
  setkey(dt,dates)
  close = as.numeric(Cl(allPrices)); 
  maxi<-function(x){max(x,na.rm = T)}
  mini<-function(x){min(x,na.rm = T)}
  cumRet<-function(x){prod(1+x,na.rm = T)}

  colnames(dt)<-c("dates",'open' ,'high' ,'low' ,'close' ,'volume',"returns","vltyRatio","slopeShort","slopeLong","slopeCross","ROC.slopeLong","regime","pB","pY",
                  "MFI","volRatio","gain15","BSRatio",'volSlope',"symbol")
  
  
  dt$preReturns =  rollapply(dt$returns,width= list(evalDays),FUN = mean,fill=NA,align = 'right')
  dt$postReturns =  rollapply(dt$returns,width= list(holdDays),FUN = cumRet,fill=NA,align = 'left')
  dt$highPrice = rollapply(close,width=list(holdDays),align = "left",FUN = maxi,partial=F,fill=NA)
  dt$dayToHigh = rollapply(close,width=list(holdDays),align = "left",FUN = which.max,partial=F,fill=NA)
  dt$lowPrice = rollapply(close,width=list(holdDays),align = "left",FUN = mini,partial=F,fill=NA)
  dt$dayToLow = rollapply(close,width=list(holdDays),align = "left",FUN = which.min,partial=F,fill=NA)
  
  dt$high_to_close = dt$highPrice/close
  setkey(dt,dates)
  
  ## The Labels 
 
  # if(is.null(change)){ change = SD*vlty*sqrt(length(holdDays)) } else {   change = abs(change) }
  # dt$moveUP<- as.numeric(ifelse(dt$close*(1+change) < dt$highPrice,1,0))
  # dt$moveDN<- as.numeric(ifelse(dt$close*(1-change) > dt$lowPrice,-1,0))
  # dt$move =  (dt$moveUP*!dt$moveDN ) + (dt$moveDN*!dt$moveUP)  ## XOR GATE
  
  return(dt[,.(dates,symbol=toupper(symbol),close,returns,preReturns,postReturns,highPrice,high_to_close,lowPrice)])
}


 earn = filterTickers(na.omit(earn),cap = 1000)  ## should check small caps too
 print(paste('Number of tickers: ',nrow(earn)))
 indicators = lapply(earn$symbol, function(st) {print(st)
                   tryCatch (merge(earn,prepareData(st),by='symbol')[dates == earnDate],   error = function(e) data.table(symbol=st)) })
                 
 
 

plotScatter(rbindlist(indicators,fill = T)[cap_mm>150000],'preReturns','postReturns')

### DRIVER 

computeIndicators<-function(dt){
  rollingAvgShort = 20 ;   rollingAvgLong = 175
  proximity = 0.1 # tolerance: distance threshold betwee long and short averages
  slopeSigF = 0.001 # Slope cannot be less than this number to be considered
  holdDays  = c(-1:1) ;   evalDays = c(-4:-2)  ## day 0 = action day
  
  offset = 1 # start of prediction window
  SD =1 # of std deviations to use for significance
  dt = cbind(dt[,.(index,Open,High,Close),by=Symbol],dt[,(1+ClCl(.SD)),by=Symbol][,V1])
  colnames(dt)<- c('Symbol' ,'index','Open', 'High','Close','ClCl')
  cumRet<-function(x){prod(1+ROC(x,type = 'discrete'),na.rm = T)}
  
  dt =  dt[,OpCl := .(Close/Open),by=Symbol]  
  dt = dt[,greenBar := .(OpCl>=1),by=Symbol]
  dt = dt[,openHigher := .(ClCl/OpCl>1),by=Symbol]
  # dt = dt[,greenBarK := seq_len(.N),by= c('Symbol',rleid('greenBar'))]
  
  dt =  dt[,HiCl := .(Close/High),by=Symbol]
  dt =  dt[,closedHigh := .(HiCl>0.998),by=Symbol]
  dt =  dt[,preReturns:=rollapply(Close,width= list(evalDays),FUN = cumRet,fill=NA,align = 'right'),by=Symbol]  ##  
  dt =  dt[,postReturns:=rollapply(Close,width= list(holdDays),FUN = cumRet,fill=NA,align = 'left'),by=Symbol]
  dt$move = ifelse(dt$postReturns>1,1,-1)
  setkey(dt,Symbol)
  
  return(dt)
  return(dt[,.(dates,symbol=toupper(symbol),close,returns,preReturns,postReturns,highPrice,high_to_close,lowPrice)])
  
}   ## custom to this strategy only long processing time 

earn_stk = merge(earn,computeIndicators(stkdata[stkdata[,nrow(.SD)>100,by=Symbol],on='Symbol'][V1==T]),
                 by.x =c('symbol','actionDate'),by.y = c('Symbol','index'))  ## pre and post to check 


## Inly top 20% by market cap 
quantile(earn_stk$cap_mm, probs = c(0.2,0.8),na.rm = T)  ## ~10MB
earn_stk[cap_mm>10000]

## take further quntiles on prereturns
quantile(earn_stk[cap_mm>20000]$preReturns, probs = c(0.1,0.9),na.rm = T) 

plotScatter(earn_stk[cap_mm>10000 & preReturns<0.93],'preReturns','postReturns') ## Corr = 
plotScatter(earn_stk[cap_mm>10000 & preReturns>1.09],'preReturns','postReturns') ## Corr = 0 
plotScatter(earn_stk[cap_mm>5000 ][! preReturns%between%c(.95,1.05)],'preReturns','postReturns') ## Corr = .4

plotScatter(earn_stk[cap_mm>500 & preReturns<0.9],'preReturns','postReturns')  ## corr = 0.3
plotScatter(earn_stk[cap_mm>500 & preReturns> 1.1],'preReturns','postReturns') ## Corr = -.24
plotScatter(earn_stk[cap_mm>500 & preReturns> 1.1],'preReturns','move') ## Corr = 0.4
plotScatter(earn_stk[cap_mm>5000 & preReturns < 0.95],'preReturns','move') ## Corr =

hist(earn_stk[cap_mm>300 ]$preReturns,breaks = 100)
hist(earn_stk[cap_mm>300 ]$postReturns,breaks = 100)

## Modelling ..
ctrl = trainControl(method = "repeatedcv",  repeats = 1,  number = 5,allowParallel = F)
mod = train(move ~ .,   data = na.omit(earn_stk[,.(move=as.factor(move),cap_mm, ClCl, OpCl, greenBar, openHigher,  HiCl, closedHigh, preReturns)]), method = "rf",metric='Kappa' ,importance=T,  tuneLength = 10,trControl = ctrl)
print(getTrainPerf(mod))  ## nothing 


