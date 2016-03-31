
source('https://github.com/vishalhawa/utilities/raw/master/util.R')
getRollingVol<-function(asset,idxrange= 100 ,endDate = Sys.Date()){
rolling = 10 # Rolling Volatility to be averaged over

#endDate = Sys.Date()

startDate = endDate - (1.5*idxrange) -1.5*rolling # we need to take more dates as tradings days are less than calendar days


asset = append("^dji",as.character(asset))
st = mapply(SIMPLIFY=FALSE ,asset  ,FUN=function(x)tryCatch(getStockHist(stk=x,sdate=startDate,edate=endDate,pricetype="Adj.Close") ,error=function(e)  return(NA)))

# SELCT all symbols that have same number of observations as '^dji' = Getting clean data 
st <- st[which(lapply(st[],  function(row) { nrow(st[["^dji"]]) == nrow(row)  }) %in% TRUE )]

stkval= do.call("cbind",st) 
stkval= stkval[order(stkval[1],decreasing=T),]
stkprices = t(as.matrix(sapply(c(1:nrow(stkval)),function(x){ as.numeric(stkval[x,grep("*.price",colnames(stkval))]) })))
colnames(stkprices)<-gsub(".price","",grep("*.price",colnames(stkval),value = TRUE))

changePer = 100*diff(-1*stkprices)/stkprices[-idxrange,] # Stk prices are in order: latest to oldest
apply(changePer,2, sd) # just a check

rollvol = t(sapply(c(1:idxrange),function(x){apply(changePer[x:(x+rolling-1),], 2, sd)}))

return(rollvol)
}


#----------------------------Driver ----------------------------

require(ggplot2)
require(reshape2)
endObservationDate = Sys.Date()
idxrange= 200 # window of observations
asset = c("FB","INTC")
volatility = getRollingVol(asset,idxrange,endObservationDate)

qplot(y=volatility[nrow(volatility):1,"^dji"],x=seq(1:idxrange),data = as.data.frame(volatility),geom="line",colour="^dji")+geom_line(aes(y=volatility[nrow(volatility):1,asset],colour=asset))




dfm <- melt(data=volatility,value.name = "Volatility",variable.name="asset") 
qplot(data=dfm,aes( x=Var1,y=Volatility,  geom="line", colour = Var2,group=Var2) )     # NOT Workign 

ggplot(dfm, aes(x=Var1, y=Volatility, color=Var2, group=Var2))+geom_line()  #Workign 



qplot(y=volatility[,"^dji"],x=seq(1:idxrange),data = as.data.frame(volatility),geom="line")
qplot(y=volatility[,"INTC"],x=seq(1:idxrange),data = as.data.frame(volatility),geom="line")


