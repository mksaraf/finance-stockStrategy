library("quantmod")
library("neuralnet")

asset = c('^gspc')
endDate = Sys.Date()-1 
startDate = endDate-365 #year's worth
lagDays = 14
fwdDays = 5

  # prices.Ad = do.call(cbind,lapply(symbols, function(x) Op(getSymbols(x,from = startDate, to = endDate,env=NULL) )))
  prices =  OHLCV(getSymbols(asset,from = startDate, to = endDate,env=NULL))
  RSI3 = RSI(Cl(prices),3)
  EMA5<-EMA(Cl(prices),n=5)
  EMAcross<-Cl(prices)-EMA5   #Look at the difference between the open price and a 5-period EMA
  
  MACD<-MACD(Cl(prices),fast = 12, slow = 26, signal = 9)
  MACDsignal<-MACD[,2]  #Grab the signal line of the MACD
  BB<-BBands(Cl(prices),n=20,sd=2)
  BBp<-BB[,4]     #We will use the Bollinger Band %B, which measures the price relative to the upper and lower Bollinger Bands
  
  PriceChange<-Cl(prices)-Op(prices)   #For this example we will be looking to predict the numeric change in price
  
  dset<-data.frame(RSI3,EMAcross,MACDsignal,BBp,PriceChange)
  colnames(dset)<-c("RSI3","EMAcross","MACDsignal","BollingerB","PriceChange")
  dset = dset[complete.cases(dset),]
  
  Normalized <-function(x) {(x-min(x))/(max(x)-min(x))} # Then normalize our data between 0-1 for SIGmoid function 
  dNormalized<-as.data.frame(lapply(dset,Normalized))

  trainingset<-dNormalized[ 1:((2/3)*nrow(dNormalized)),]
  testset<-dNormalized[ ((2/3)*nrow(dNormalized)):nrow(dNormalized),] 
  
  nn1<-neuralnet(PriceChange~RSI3+EMAcross+MACDsignal+BollingerB,data=trainingset, hidden=c(3,3), learningrate=.001,algorithm="backprop")
  plot(nn1) #We are using our indicators to predict the priceChnage over the training set, and a learning rate of .001 with a backpropagation algorithm
  
  
  