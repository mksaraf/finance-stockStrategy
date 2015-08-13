

source('https://github.com/vishalhawa/utilities/raw/master/util.R')

getCross<-function(asset,endDate = Sys.Date()){

rollingAvgShort = 50
rollingAvgLong = 200
idxrange = 20 # of observations from current date 
proximity = 0.01 # tolerance: distance threshold between long and short averages

# Min 2 assets are required
# asset = c("WLT","T","BABA","BBBY") 
asset = append("^dji",as.character(asset))


startDate = endDate - 2*rollingAvgLong # we need to take more dates than needed to avoid errors

st = mapply(SIMPLIFY=FALSE ,asset  ,FUN=function(x)tryCatch(getStockHist(stk=x,sdate=startDate,edate=endDate,pricetype="Adj.Close") ,error=function(e)  return(NA)))

# SELCT all symbols that have same number of observations as '^dji' = Getting clean data 
st <- st[which(lapply(st[],  function(row) { nrow(st[["^dji"]]) == nrow(row)  }) %in% TRUE )]

stkval= do.call("cbind",st) 

stkval= stkval[order(stkval[1],decreasing=T),]

rollmeanShort = t(sapply(c(1:idxrange),function(x){apply(stkval[x:(x+rollingAvgShort-1),grep("*.price",colnames(stkval))], 2, mean)}))

rollmeanLong = t(sapply(c(1:idxrange),function(x){apply(stkval[x:(x+rollingAvgLong-1),grep("*.price",colnames(stkval))], 2, mean)}))

stkprices = as.matrix(sapply(c(1:idxrange),function(x){stkval[x,grep("*.price",colnames(stkval))]}))

percentileMatrix = (rollmeanShort-rollmeanLong)/rollmeanLong

percentileMatrix.proximity = abs(percentileMatrix) < proximity

slopeShort = diff(-1*rollmeanShort) # why multi -1

slopeLong = diff(-1*rollmeanLong)


# ifelse(slopeLong[1,] <0 & slopeShort[1,] <0,  print("Negative Slopes"),  ifelse(slopeLong[1,] >0 & slopeShort[1,] >0,print("Positive Slopes")  ,    print("Mixed Slopes")))

# the slopeMatrix provides wether long and short slopes are both positive , negative or Mixed : Potential Crosses
# slopeMatrix = ifelse(slopeLong <0 & slopeShort <0, -1,  ifelse(slopeLong>0 & slopeShort>0,1  ,  0))

# Confirmed Crosses
slopeMatrix = ifelse(slopeLong <0 & slopeShort <0 & (rollmeanShort[-idxrange,]<rollmeanLong[-idxrange,]), -1,  ifelse(slopeLong>0 & slopeShort>0 & (rollmeanShort[-idxrange,]>rollmeanLong[-idxrange,]) ,  1 ,  0))

# This produces table of Death and Golden Cross
 crossdf = as.data.frame(apply(slopeMatrix*percentileMatrix.proximity[1:idxrange-1,], c(1,2), function(x)(if(x>0) x="Golden Cross" else if(x<0) x="Death Cross" else x="NA")) )

 row.names(crossdf) <- stkval[1:idxrange-1,"^dji.date"]
 
return (crossdf)
}
# -------------------Driver ---------------------------------


require(plyr)
library(RODBC)
ch <- odbcConnect("portfolio")
res <- sqlFetch(ch, "USStocks") #Fetch Query results to DF
tickers <- res[,"symbol"]
#tickers <- c("SAP","AA","AAPL")
getCross(tickers[800:810])
#(ldply(tickers, getCross))

close(ch)



#--------------------------------------------EOF-----------------
