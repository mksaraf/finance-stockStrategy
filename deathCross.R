

source('https://github.com/vishalhawa/utilities/raw/master/util.R')

rollingAvgShort = 50
rollingAvgLong = 200
idxrange = 20 # of observations from current date 
proximity = 0.02 # distance threshold between long and short averages

# Min 2 assets are required
asset = c("^dji","twtr")

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

percentileMatrix.proximity = abs(percentileMatrix) < proximity

slopeShort = diff(-1*rollmeanShort)

slopeLong = diff(-1*rollmeanLong)


# ifelse(slopeLong[1,] <0 & slopeShort[1,] <0,  print("Negative Slopes"),  ifelse(slopeLong[1,] >0 & slopeShort[1,] >0,print("Positive Slopes")  ,    print("Mixed Slopes")))

# the slopeMatrix provides wether long and short slopes are both positive , negative or Mixed
slopeMatrix = ifelse(slopeLong <0 & slopeShort <0, -1,  ifelse(slopeLong>0 & slopeShort>0,1  ,  0))


apply(slopeMatrix*percentileMatrix.proximity[1:idxrange-1,], c(1,2), function(x)(if(x>0) print("Golden Cross") else if(x<0) print("Death Cross")))


