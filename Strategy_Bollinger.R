################# Finance Strategy:  Bollinger Pops and Sinks  ##################################
###################### ML Algo: - All Applied #################

require(data.table)
require(quantmod)
require(plyr)
library("rpart")
library("rpart.plot")
library("e1071")
library("ggplot2")
library(randomForest)
library(neuralnet)

setwd(paste0("C:/Users/",Sys.getenv("username"),"/Dropbox/Projects/RCode/finance-stockStrategy/"))
source('../utilities/util.R')


# ###################### Prepare RAW Data ###########
prepareData<-function(asset="SPY",endDate = Sys.Date()){

  startDate = endDate -365 -30 # we need to take more dates as tradings days are less than calendar days
 
  bollingerValues = getBollingerValue(asset,startDate =startDate,endDate=Sys.Date())
  bollingerValues$perChange = bollingerValues[,c(grep(".Close",names(bollingerValues)))]/lag(bollingerValues[,c(grep(".Close",names(bollingerValues)))])-1
  bollingerValues$vlty <- sd(bollingerValues$perChange,na.rm = TRUE) 
   colnames(bollingerValues) = c("dn","mavg","up","pctB","high","low","close","perChange","vlty")
  return(bollingerValues)
}
#    prices =  Cl(getSymbols(asset,from = startDate, to = endDate,env=NULL))
#   pricesTesting = prices[(length(prices)-30):length(prices)]
#   pricesTraining = prices[1:(length(prices)-30)]
#   
#   rollmeanShort = EMA(pricesTraining,rollingAvgShort)
#   rollmeanLong = EMA(pricesTraining,rollingAvgLong)
#   potCases = (abs((rollmeanShort-rollmeanLong)/rollmeanLong) < proximity) *(rollmeanShort-rollmeanLong)
#   dt=data.table(dates=index(potCases),diff=potCases,slopeShort= ROC(rollmeanShort),slopeLong=ROC(rollmeanLong),symbol=asset,pR=prices/shift(prices, fwdDays, type='lead'))
#   setkey(dt,dates)
#   colnames(dt)<-c("dates","diff.EMA","slopeShort","slopeLong","symbol",paste0("priceRatio.",fwdDays))
#   
#   dt = dt[(complete.cases(dt) &  diff.EMA != 0)]
#   
#   pc = paste0("priceRatio.",fwdDays)
#   
#   dt[,priceChange:=ifelse((pc)>1L,"UP","DN")]
#   
#   dt[,priceChange:=ifelse(dt[[6]]>1.0,"UP","DN")]  # Col 6 is hard  
#   dt$cross =  ifelse(dt$diff.EMA>0 & dt$slopeShort<0 &  dt$slopeLong<0,"D",ifelse(dt$diff.EMA<0 & dt$slopeShort>0 &  dt$slopeLong>0,"G",""))
#   
#   # dt[,priceChange:=ifelse(eval(as.expression(paste0("priceRatio.",fwdDays)))>1.0,"UP","DN")]
#   #  .(p5=p1/p2,p1=prices[(dt[,dates])],p2=prices[(dt[,dates]+5)])
#   return(dt)


# ###################  Driver : Prepare Indicators ########################################

asset = c('ORCL')
endDate = Sys.Date()-1 
startDate = endDate-365 #year's worth
lagDays = 14
fwdDays = 6
fwdDays2 = 10
win = 30 # window of observations 
prices = prepareData(asset,endDate ) # gets bollinger values and HLC 

fmaxD<-function(x){which.max(x)}
fmaxV<-function(x){max(x)}
maxi=data.frame(rollapply(prices$close,width=win,align = "left",FUN = fmaxV),rollapply(prices$close,width=win,align = "left",FUN = fmaxD))
colnames(maxi)<-c("maxPrice","daysLater")
maxi = merge(prices,xts(maxi,order.by = as.Date(row.names(maxi))))
maxi = maxi[complete.cases(maxi),]
maxi$move<-NA
maxi$move<- ifelse(maxi$close*(1+maxi$vlty*sqrt(maxi$daysLater)) < maxi$maxPrice,"UP","0")

indicators=data.table(dates=index(maxi),changeRatio=(abs(as.numeric(maxi$perChange))/as.numeric(maxi$vlty)),pB=as.numeric(maxi$pctB) ,changePB=Delt(as.numeric(maxi$pctB) ),symbol=asset,maxi$move)
setkey(indicators,dates)
setnames(indicators,4,"changePB")

# indicators[,prices$BABA.Close/shift(prices$BABA.Close, 10, type='lead')] 

indicators = indicators[complete.cases(indicators),]
set.seed(12)

# indicators[,priceChange1:=ifelse(indicators[[5]]>1.05,"UP",ifelse(indicators[[5]]<0.95,"DN","NA"))] 
# indicators[,priceChange2:=ifelse(indicators[[6]]>1.08,"UP",ifelse(indicators[[6]]<0.92,"DN","NA"))] 

#################

# # date and price of the Maximum price (the first occurance) in the window 'win'
# maxPts = prices$close[win:length(prices$close)][which(prices$close[win:length(prices$close)]==rollmaxr(prices$close,k=win))]


#  #######################  ML Algo: Decision Tree  ######################

dtree1<-rpart(priceChange1~perChange+pctB,data=indicators)    
cols <- ifelse(dtree1$frame$yval == 1, "darkred", "green4")    
prp(dtree1,type=2,extra=8,col = cols)

dtree2<-rpart(priceChange2~perChange+pctB,data=indicators)   # [priceChange2 %in% c("UP","DN"),]    
cols <- ifelse(dtree2$frame$yval == 1, "darkred", "green4")    
prp(dtree2,type=2,extra=8,col = cols)

#  #######################  ML Algo: Random Forest   ###################################

# we need to find the optimal number of indicators to use for each individual tree
FeatureNumber<-tuneRF(indicators[,c("changeRatio","pB","changePB"),with=F],indicators[,move],ntreeTry=100, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE)


RF = randomForest(move ~ changeRatio+pB+changePB,  data=indicators, mtry=1,importance=TRUE, ntree=2000,proximity=TRUE,keep.forest=TRUE)

# prp(RF1,type=2,extra=8,col = cols)
 importance(RF)
varImpPlot(RF)
MDSplot(RF,indicators$pctB)
rftree=getTree(RF, 3, labelVar=TRUE)
cols <- ifelse(rftree$frame$yval == 1, "darkred", "green4")    
prp(rftree,type=2,extra=8,col = cols)


#  #######################  ML Algo: SVM  ###################################

#Build our support vector machine using a radial basis function as our kernel,
# the cost, or C, at 1, and the gamma function at ½, or 1 over the number of inputs we are using

SVM1<-svm(priceChange1~perChange+pctB,data=indicators, kernel="radial",cost=0.5,gamma=1/2,type="C")
SVMPred<-data.frame(indicators,"SVMPred"=predict(SVM1,indicators,type="class"))
ggplot(SVMPred,aes(x=perChange,y=pctB))+stat_density2d(geom="contour",aes(color=SVMPred))+labs(title="SVM  Predictions @ 5%",x="perChange",y="pctB",color="SVMPred")


SVM2<-svm(priceChange2~perChange+pctB,data=indicators, kernel="radial",cost=0.5,gamma=1/2,type="C")
SVMPred<-data.frame(indicators,"SVMPred"=predict(SVM2,indicators,type="class"))
ggplot(SVMPred,aes(x=perChange,y=pctB))+stat_density2d(geom="contour",aes(color=SVMPred))+labs(title="SVM  Predictions @ 8%",x="perChange",y="pctB",color="SVMPred")

# tune.svm ()
#  #######################  ML Algo: ANN  ###################################
Normalized <-function(x) {(x-min(x))/(max(x)-min(x))} # Then normalize our data between 0-1 for SIGmoid function 
indicatorsNorm<-cbind(sapply(indicators[,.(perChange,pctB,PriceFactor6,PriceFactor10)],FUN=Normalized),indicators[,.(priceChange1,priceChange2)])

# trainingset<-dNormalized[ 1:((2/3)*nrow(dNormalized)),]
# testset<-dNormalized[ ((2/3)*nrow(dNormalized)):nrow(dNormalized),] 
m=model.matrix(data=indicatorsNorm,~priceChange1+priceChange2+perChange+pctB) # Since neuralnet only deals with quantitative variables
nn1UP<-neuralnet(priceChange1UP~perChange+pctB,data=m,hidden=c(3,3), learningrate=.001,algorithm="backprop",linear.output=FALSE,err.fct = "ce")
plot(nn1UP) #We are using our indicators to predict the priceChnage over the training set, and a learning rate of .001 with a backpropagation algorithm

gwplot(nn1UP,selected.covariate=2,rep="best")
# confidence.interval(nn1)
# nn1$result.matrix

