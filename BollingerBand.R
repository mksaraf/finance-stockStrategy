# This scripts calls and calculates Bollinger Values ------------

source('https://github.com/vishalhawa/utilities/raw/master/util.R')


{ #-----  Driver -----------
#   startDate=as.Date("01/01/2008",format="%m/%d/%Y")


lookback = 20
endDate = as.Date("12/31/2009",format="%m/%d/%Y")
beginDate = as.Date("1/1/2008",format="%m/%d/%Y")
startDate = beginDate- (lookback+10)

asset.vec = list(stock = c("AAPL","MSFT"))
  
#   asset.vec = read.csv("shinyApp/sp5002012.csv")

st = mapply(SIMPLIFY=FALSE ,as.character(asset.vec$stock)  ,FUN=function(x)tryCatch(getStockHist(stk=x,sdate=startDate,edate=endDate,pricetype="Adj.Close") ,error=function(e)return(NA)))

countofobs = as.numeric(names(which.max(table(sapply(sapply(st, "[", 2),length)))))
# which(sapply(sapply(st, "[", 2),length)!=countofobs)  # Indices that are unequal to most others should be removed

# -----------------Remove indices -----------------------
removeidx = which(sapply(sapply(st, "[", 2),length)!=countofobs)

st=  st[-removeidx]

 stkval= do.call("cbind",st)
# stkval2=do.call(cbind,sapply(st, "[", 2))
  
asset.vec.rv = names(st)
idxrange=which.min(stkval[,1]>=(beginDate))

stkval= stkval[order(stkval[1],decreasing=T),]

#   stkval[1:20,paste0(asset.vec,".price")]



rollsd = sapply(c(1:idxrange),function(x){apply(stkval[x:(x+lookback-1),paste0(asset.vec.rv,".price")], 2, sd)})


rollmean = as.matrix(sapply(c(1:idxrange),function(x){apply(stkval[x:(x+lookback-1),paste0(asset.vec.rv,".price")], 2, mean)}))

stkprices = as.matrix(sapply(c(1:idxrange),function(x){stkval[x,paste0(asset.vec.rv,".price")]}))

bvalues = (as.numeric(stkprices)-rollmean)/rollsd

#-------------------------------------------------

rowsofinterest = which(rowSums(bvaluesf$SPY.price>1 & bvaluesf[]  < -2)>0)

bval.interest = bvaluesf[rowsofinterest,]

#--------------------------------------------
roll.mean = colMeans(stkval[1:lookback,paste0(asset.vec,".price")])

roll.sd = apply(stkval[1:lookback,paste0(asset.vec,".price")], 2, sd)




prices = stkval[1,grep("*.price",colnames(stkval))]

(prices-roll.mean)/roll.sd

Bollinger_AAPL = (stkval[1,paste0("AAPL",".price")] - roll.mean["AAPL.price"]) / (roll.sd["AAPL.price"]) 

Bollinger_MSFT = (stkval[1,paste0("MSFT",".price")] - roll.mean["MSFT.price"]) / (roll.sd["MSFT.price"]) 

getBollingerValue(asset = c("SPY","A") ,startDate = startDate,endDate=Sys.Date(),lookback = 20)



}
# st[names(st)]
# # matrix(unlist(st),nrow=15)
# sapply(c(seq(1:501)),function(x) nrow(st[[x]]))
# length(st[[c(1,2)]])
# sapply(st, nrow)
# as.matrix(st)
# names(st[]) %in% c("ZMH")
#  do.call(cbind,sapply(st, "[", 2))
# which(sapply(sapply(st, "[", 2),length)==533)
# as.numeric(names(which.max(table())))  
# 

getStockHist("SPY",sdate = startDate,edate = endDate,pricetype = "Adj.Close")
