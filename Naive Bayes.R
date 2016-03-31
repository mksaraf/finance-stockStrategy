# Finance Strategy: EMA5-EMA10
# ML Algo: Naive Bayes, Association learning, bagging
# Stock: SPY  chartSeries(to.weekly(Cl(getSymbols("SPY",auto.assign = FALSE,from=startDate,to=endDate)),OHLC=FALSE))

source('C:/Users/Vhawa/Dropbox/Projects/RCode/utilities/util.R')

library("quantmod")
library("e1071") #Gives us access to the Naïve Bayes classifier 
library("adabag") # Used for bagging and boosting trees

# #Download Michael Kapler's “Systematic Investor Toolbox”, 
# library(curl)
# con = gzcon(curl('https://github.com/systematicinvestor/SIT/raw/master/sit.gz','rb'))
# source(con)
# close(con)
# # a powerful set of tools used to backtest and evaluate quantitative trading strategies 


asset = c('erx')
endDate = Sys.Date()-1 
startDate = endDate-365 #year's worth
lagDays = 14
fwdDays = 5

prices =  OHLCV(getSymbols(asset,from = startDate, to = endDate,env=NULL))
RSI = RSI(Cl(prices))
EMA5<-EMA(Cl(prices),n=5)
Volume<-Vo(prices)     

#Find the ratio change between Next day close price and today close price
ratio <- function(x) (tail(x,1)/as.numeric(head(x,1)))   # print(head(x,1))
PriceChange<- rollapply(Cl(prices),width=2,align = "left",FUN = ratio) # align = "left" is fwd looking 

# PriceChange<- Delt(Cl(prices),k=1)  #Find the ratio change between the today close price and prevday close price 
dset<-round(data.frame(Cl(prices),EMA5,RSI,Volume,PriceChange),2)
dset<-data.frame(ifelse(PriceChange>1,"UP","DOWN"),dset)
colnames(dset)<-c("PriceMove","Price","EMA5","RSI","Volume","PriceChange")
dset = dset[complete.cases(dset),]

TrainingSet<-dset[1:((2/3)*nrow(dset)),] #We will use ⅔ of the data to train the model
TestSet<-dset[((2/3)*nrow(dset)):nrow(dset),]  #And ⅓ to test it on unseen data 

NBModel<-naiveBayes(PriceMove~ EMA5+RSI+Volume,data=TrainingSet) 
 
tab=table(predict(NBModel,TestSet),TestSet[,1],dnn=list('predicted','actual')) 
print(tab) ; (paste("NB Simple Features: % Correct:",round(100*sum(diag(tab))/sum(tab))))

# - Model 2 --->improvised features
RSIChange <- ROC(RSI(Cl(prices)),3,type='discrete')
EMA5Cross<- Cl(prices) -EMA(Cl(prices),n=5)
VolumeChange<-ROC(Vo(prices),3,type='discrete')

dset2<-round(data.frame(Cl(prices),EMA5Cross,RSIChange,VolumeChange,RSI),2)
dset2<-data.frame(ifelse(PriceChange>1,"UP","DOWN"),PriceChange,dset2)  
colnames(dset2)<-c("PriceMove","PriceChange","Price","EMA5Cross","RSIChange","VolumeChange","RSI")
dset2 = dset2[complete.cases(dset2),]

TrainingSet<-dset2[1:((2/3)*nrow(dset2)),] #We will use ⅔ of the data to train the model
TestSet<-dset2[((2/3)*nrow(dset2)):nrow(dset2),]  #And ⅓ to test it on unseen data 

NBModel<-naiveBayes(PriceMove~EMA5Cross+RSIChange+VolumeChange,data=TrainingSet) 

tab=table(predict(NBModel,TestSet),TestSet[,1],dnn=list('predicted','actual')) 
print(tab) ; (paste("NB-Improved Feaures: % Correct:",round(100*sum(diag(tab))/sum(tab))))

# Extracting Rules / Association learning  ---> 'Truth table' 

resultsdf = data.frame("X"=ifelse(predict(NBModel,TestSet)==TestSet[,1],"Correct","Incorrect"),"Pred"=predict(NBModel,TestSet),TestSet)

qplot(EMA5Cross,PriceChange, data=(resultsdf[resultsdf$X=="Correct" ,]),geom=c("line"))
qplot(RSIChange,PriceChange, data=(resultsdf[resultsdf$X=="Correct" ,]),geom="line")
qplot(VolumeChange,PriceChange, data=(resultsdf[resultsdf$X=="Correct" ,]),geom="line")

signals= data.frame(asset,signal=ifelse(dset2$EMA5Cross>0.5,1,ifelse(dset2$EMA5Cross< -0.5,-1,0)),dset2)

backTest(signals$Price,signals[,c("asset","signal")])

# -----------------------------------------

orders<-signals[,c("asset","signal")]
df<- signals[,c("asset","signal")]
stkvalues<-signals$Price
colnames(df)<-c("asset","holdings")


df[,"holdings"]<-0 
# df[,"holdings"]<- ifelse(df[,c("holdings")]<0,orders$signal<0,0,df[,"holdings"]) # No shorts
for( sig in 2:length(orders$signal)){
  
  df[,"holdings"][sig]=df[,"holdings"][sig-1]+orders$signal[sig]
  if(df[,"holdings"][sig]<0)  df[,"holdings"][sig]<-0
  
}

df <-data.frame(df,stkvalues,holdingValue=stkvalues*df[,"holdings"],realizedValue=cumsum(-1*orders$signal*stkvalues),signal=orders$signal)
ggplot(aes(x=as.Date(row.names(df))),data=df) + geom_line(aes(color="holdingValue",y=holdingValue)) + geom_line(aes(color="realizedValue",y=realizedValue))





# ---Ensemble - bagging 
bagmodel<-bagging(PriceMove~EMA5Cross+RSIChange+VolumeChange,data=TrainingSet)

tab=table(predict.bagging(bagmodel,newdata=TestSet)$class,TestSet[,1],dnn=list('predicted','actual')) 
print(tab) ; (paste("Bagging: % Correct:",round(100*sum(diag(tab))/sum(tab))))
bagmodel$importance



