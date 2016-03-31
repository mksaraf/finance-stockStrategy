# Finance Strategy:  Indicators ~  Upward Move +   Responses ~
# ML Algo: Decision Tree / Pruning
# Stock: 

library("quantmod")
library("rpart")
library("rpart.plot")
library("e1071")

#Let us easily create good looking diagrams of the trees.

asset = c('spy')
endDate = Sys.Date()-1 
startDate = endDate-365 #year's worth
lagDays = 14
fwdDays = 5



decisionTree<-function(asset,startDate,endDate,lagDays,fwdDays){

  # prices.Ad = do.call(cbind,lapply(symbols, function(x) Op(getSymbols(x,from = startDate, to = endDate,env=NULL) )))
  prices =  OHLCV(getSymbols(asset,from = startDate, to = endDate,env=NULL))
  
#   ema.volChange = round(diff(log(EMA(prices[,5],3)),lag=lagDays),1) 
#   ema.priceChange = round(diff(log(EMA(prices[,4],3)),lag=lagDays),1)

#Indicators ~ Absolute Change
   ema.volChange = round(diff(EMA(prices[,5],3),lag=lagDays)/EMA(prices[,5],3),1) 
   ema.priceChange = round(diff(EMA(prices[,4],3),lag=lagDays)/EMA(prices[,4],3),1)
   mfi = MFI(prices[,c(2,3,4)],prices[,5])
  
  vlty = sd(EMA(prices[,4],3),na.rm=TRUE)*sqrt(lagDays)
  
  f <- function(x) (tail(x,1)/as.numeric(head(x,1)))   # print(head(x,1))
  
 dset<-merge( rollapply(prices[,4],width=fwdDays,align = "left",FUN = f),ema.priceChange,ema.volChange,mfi )
 colnames(dset)<-c("FwdReturns","priceChange","volChange","MFI")
 dset = dset[complete.cases(dset),]
  
  #  xts (and zoo) objects are a matrix with an index attribute, and you can't mix types in a matrix 
 dset<-as.data.frame(dset)
  dset$FwdReturns <-  as.factor(trunc(dset$FwdReturns ))

  
  trainingset<-dset[ 1:((2/3)*nrow(dset)),]
  testset<-dset[ ((2/3)*nrow(dset)):nrow(dset),] 
 

  #Algo: DECISION TREE
  dtree<-rpart(FwdReturns~.,data=trainingset)   #controlling the growth of the tree by setting the minimum amount of information gained (cp) needed to justify a split.
  
  cols <- ifelse(dtree$frame$yval == 1, "darkred", "green4")   # green if survived
  prp(dtree,type=2,extra=8,col = cols)
  
  min.xerror.cp = printcp(dtree)[which.min(printcp(dtree)[,"xerror"]),"CP"]   #shows the minimal cp for each trees of each size.
  plotcp(dtree,upper="splits")   #plots the average geometric mean for trees of each size.
  dtree.pruned<-prune(dtree,cp=min.xerror.cp)   #I am selecting the complexity parameter (cp) that has the lowest cross-validated error (xerror)
  prp(dtree.pruned,type=2,extra=8,col = cols)
  printcp(dtree.pruned)
  
  # Time to see how well it does over our TRAINIG set.
  tab=table(predict(dtree.pruned,trainingset,type="class"),trainingset[,"FwdReturns"],dnn=list('predicted','actual'))
  print(tab)
  paste("Decision Tree: TRAIN: % Correct:",round(100*sum(diag(tab))/sum(tab)))
 
   # Time to see how well it does over our test set.
  tab=table(predict(dtree.pruned,testset,type="class"),testset[,"FwdReturns"],dnn=list('predicted','actual'))
  print(tab)
  (paste("Decision Tree: % Correct:",round(100*sum(diag(tab))/sum(tab))))
  
  #Algo: Naive Bayes
  
  NBModel<-naiveBayes(FwdReturns~.,data=trainingset) 
  NBModel
  
  #predict(NBModel,testset)
  
  tab2=table(predict(NBModel,testset),testset[,"FwdReturns"],dnn=list('predicted','actual')) 
  print(tab2)
  (paste("% Correct:",round(100*sum(diag(tab2))/sum(tab2))))
}



decisionTree(asset= "DG",startDate = startDate ,endDate = endDate,lagDays = lagDays,fwdDays = fwdDays)






# --------------------------------------------------------

# RSI5<-RSI(Op(BAC), n= 5)
# 
# EMA10<-EMA(Op(BAC),n=10)
# #Calculate a 5-period exponential moving average (EMA)
# EMAcross<- Op(BAC)-EMA10
# #Let’s explore the difference between the open price and our 10-period EMA
# 
# MACD<-MACD(Op(BAC),fast = 12, slow = 26, signal = 9)
# #Calculate a MACD with standard parameters
# MACDsignal<-MACD[,2]
# #Grab just the signal line to use as our indicator.
# 
# 
# SMI<-SMI(Op(BAC),n=13,slow=25,fast=2,signal=9) 
# #Stochastic Oscillator with standard parameters
# SMI<-SMI[,1]
# #Grab just the oscillator to use as our indicator
# 
# PriceChange<- Cl(BAC) - Op(BAC)
# dset<-data.frame(change=ifelse(PriceChange>0,"UP","DOWN"),EMAcross,RSI5,SMI,MACDsignal)
# 
# colnames(dset)<-c("change","EMAcross","RSI5","Stochastic","MACDsignal")
# dset<- dset[complete.cases(dset),]

 

