## Strategy : Candle Stick Bars ### V - patterns 
## Check for  large Green Bar Filled near Valley/ below ma3 line




computeIndicators2<-function(dt){
  rollingAvgShort = 20 ;   rollingAvgLong = 175
  proximity = 0.1 # tolerance: distance threshold betwee long and short averages
  slopeSigF = 0.001 # Slope cannot be less than this number to be considered
  holdDays  = c(1:10) ;   evalDays = c(-4:-1)
  
  dt = cbind(dt[,.(index,Open,High,Close),by=Symbol],dt[,(1+ClCl(.SD)),by=Symbol][,V1])
  colnames(dt)<- c('Symbol' ,'index','Open', 'High','Close','ClCl')
  cumRet<-function(x){prod(1+ROC(x,type = 'discrete'),na.rm = T)}

  
  dt = dt[,ma3 := SMA(Close,3),by=Symbol]
  dt = dt[,ma3_sl := ROC(ma3),by=Symbol]
  dt = dt[,ma20 := SMA(Close,20),by=Symbol]
 
  dt =  dt[,OpCl := .(Close/Open),by=Symbol]  
  dt = dt[,greenBar := .(OpCl>=1),by=Symbol]
  dt = dt[,openHigher := .(ClCl/OpCl>1),by=Symbol]
  # dt = dt[,greenBarK := seq_len(.N),by= c('Symbol',rleid('greenBar'))]
  dt = dt[, relativeChange := OpCl/SMA(OpCl,60) ,by=Symbol]
  
  
  dt =  dt[,HiCl := .(Close/High),by=Symbol]
  dt =  dt[,closedHigh := .(HiCl>0.998),by=Symbol]
  dt =  dt[,preReturns:=rollapply(Close,width= list(evalDays),FUN = mean,fill=NA,align = 'right'),by=Symbol]  ##  mean returns
  dt =  dt[,postReturns:=rollapply(Close,width= list(holdDays),FUN = max,fill=NA,align = 'left'),by=Symbol]
  dt$move = ifelse(dt$postReturns/dt$Close >1,1,-1)
  setkey(dt,Symbol)
 
  return(dt)
  return(dt[,.(dates,symbol=toupper(symbol),close,returns,preReturns,postReturns,highPrice,high_to_close,lowPrice)])
  
}

ind = computeIndicators2(na.omit(stkdata[Symbol %in% uss$symbol]))   ## only uss symbols / how about Volume / or near yearly lows 

ind[ greenBar==T & (ma3>ma20) & ma3_sl>0 & (Close<ma3) &relativeChange>1.04 & Close>10]
# ind = computeIndicators2(stkdata[stkdata[,nrow(.SD)>100,by=Symbol],on='Symbol'][V1==T])  ## removing some symbols
 
table(ind[ greenBar==T & (ma3>ma20) & ma3_sl>0 & (Close<ma3) &relativeChange>1.04 & Close>10,move])  
## Best Ratio 15/1 : Large Green Bar on slope up on + regime @Close below ma3 line 

table(ind[ greenBar==T   & (Close<ma3) &relativeChange>1.04 & Close>10,move]) 
## Ratio 188/42 - try shaping V

##
ind[Symbol %in% unique(earn[cap_mm>10000]$symbol) ,.(index,Symbol,ClCl,OpCl,greenBar,barK=seq_len(.N),postReturns,move),by=.(Symbol,rleid(greenBar))]


plotScatter(ind[ closedHigh==T & greenBar==T],x='openHigher','move',other_aes = 'Close')



ind1 = ind[Symbol %in% unique(earn[cap_mm>10000]$symbol) ,
           .(index,Symbol,ClCl,OpCl,closedHigh,openHigher,greenBar,barK=seq_len(.N),postReturns,move),by=.(Symbol,rleid(greenBar))]

table(ind1[ openHigher==T & barK==2 ]$greenBar , ind1[openHigher==T & barK==2 ]$move) ## nothing 
table(ind1[  greenBar==T & barK==2 ]$closedHigh , ind1[ greenBar==T & barK==2 ]$move)  ##  best of now 

ctrl = trainControl(method = "repeatedcv",  repeats = 1,  number = 5,allowParallel = F)
 mod = train(move ~ .,   data = na.omit(ind1[,.(move=as.factor(move),greenBar,barK,closedHigh)]), method = "xgbTree",metric='Kappa' ,importance=T,  tuneLength = 10,trControl = ctrl)
print(getTrainPerf(mod))  ## NO Luck but barK is everything 
varImp(mod,scale = F)
 
earn[, cat :=cut(earn$cap_mm,c(0,500,2000,6000,15000,Inf),labels = c('tiny','small','mid','large','mega'))]
stkdata = rbindlist(lapply(unique(earn$symbol), function(st) { 
  tryCatch (prepareData(st,startDate = '2017-01-01'),  error = function(e) data.table(symbol=st)) }),fill = T)


earn = filterTickers(na.omit(earn),cap = 1000)  ## should check small caps too
print(paste('Number of tickers: ',nrow(earn)))
indicators = lapply(earn$symbol, function(st) {print(st)
  tryCatch (merge(earn,prepareData(st),by='symbol')[dates == earnDate],
            error = function(e) data.table(symbol=st)) })





plotScatter(rbindlist(indicators,fill = T)[cap_mm<500],'preReturns','postReturns')


