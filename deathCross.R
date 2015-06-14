

source('https://github.com/vishalhawa/utilities/raw/master/util.R')

rollingAvgShort = 50
rollingAvgLong = 200

idxrange = 20 # of observations from current date 
asset = c("uso","pfe")

endDate = Sys.Date()
startDate = endDate - 2*rollingAvgLong # we need to take more dates than needed to avoid errors

st = mapply(SIMPLIFY=FALSE ,as.character(asset)  ,FUN=function(x)tryCatch(getStockHist(stk=x,sdate=startDate,edate=endDate,pricetype="Adj.Close") ,error=function(e)return(NA)))

stkval= do.call("cbind",st) #issue: if number of values differ in 'st' then this function throws error
asset.vec.rv = names(st)
stkval= stkval[order(stkval[1],decreasing=T),]


#rollsd = sapply(c(1:idxrange),function(x){apply(stkval[x:(x+lookback-1),paste0(asset.vec.rv,".price")], 2, sd)})


rollmeanShort = t(sapply(c(1:idxrange),function(x){apply(stkval[x:(x+rollingAvgShort-1),paste0(asset.vec.rv,".price")], 2, mean)}))

rollmeanLong = t(sapply(c(1:idxrange),function(x){apply(stkval[x:(x+rollingAvgLong-1),paste0(asset.vec.rv,".price")], 2, mean)}))

stkprices = as.matrix(sapply(c(1:idxrange),function(x){stkval[x,paste0(asset.vec.rv,".price")]}))

percentileMatrix = (rollmeanShort-rollmeanLong)/rollmeanLong

slopeShort = diff(rollmeanShort[nrow(rollmeanShort):1,])

slopeLong = diff(rollmeanLong[nrow(rollmeanLong):1,])



