
require(plyr)
source('https://github.com/vishalhawa/utilities/raw/master/util.R')

getCross<-function(asset,endDate = Sys.Date()){

rollingAvgShort = 50
rollingAvgLong = 200
idxrange = 60 # window of observations from end date: 60 = Quarter 
proximity = 0.01 # tolerance: distance threshold between long and short averages
slopeSigF = 0.001 # Slope cannot be less than this number to be considered
asset = append("^dji",as.character(asset))


startDate = endDate - (1.5*idxrange) -1.5*rollingAvgLong # we need to take more dates as tradings days are less than calendar days

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

slopeShort = round(1*diff(-1*rollmeanShort) /rollmeanShort[-idxrange,],digits = 3) # 

slopeLong = round(1*diff(-1*rollmeanLong) /rollmeanLong[-idxrange,],digits = 3)


# ifelse(slopeLong[1,] <0 & slopeShort[1,] <0,  print("Negative Slopes"),  ifelse(slopeLong[1,] >0 & slopeShort[1,] >0,print("Positive Slopes")  ,    print("Mixed Slopes")))

# the slopeMatrix provides wether long and short slopes are both positive , negative or Mixed : Potential Crosses
# slopeMatrix = ifelse(slopeLong <0 & slopeShort <0, -1,  ifelse(slopeLong>0 & slopeShort>0,1  ,  0))

# Confirmed Crosses
slopeMatrix = ifelse(slopeLong <0 & slopeShort <0 & (rollmeanShort[-idxrange,]<rollmeanLong[-idxrange,]), -1,  ifelse(slopeLong>0 & slopeShort>0 & (rollmeanShort[-idxrange,]>rollmeanLong[-idxrange,]) ,  1 ,  0))

# This produces internal table of Death and Golden Cross : Debug use only
 crossdf = as.data.frame(apply(slopeMatrix*percentileMatrix.proximity[1:idxrange-1,], c(1,2), function(x)(if(x>0) x="Golden Cross" else if(x<0) x="Death Cross" else x="NA")) )

 row.names(crossdf) <- stkval[1:idxrange-1,"^dji.date"]

 # --- Creating Df for events : cross -----
 
 cross <- data.frame(day=as.Date(character()), stk=character(), type=character(),stringsAsFactors=FALSE) 
 
 results = as.data.frame(slopeMatrix*percentileMatrix.proximity[1:idxrange-1,])
 row.names(results) <- stkval[1:idxrange-1,"^dji.date"]
 
  rnames = rownames(results)[ which(results==1,arr.ind = TRUE)[,1]]
 cnames = colnames(results)[ which(results==1,arr.ind = TRUE)[,2]]
 
 if (length(rnames) >0) cross = rbind(cross, data.frame(day=as.Date(rnames,format ="%Y-%m-%d" ),stk=gsub(".price","" ,cnames), type="G"))
 
 rnames = rownames(results)[ which(results== -1,arr.ind = TRUE)[,1]]
 cnames = colnames(results)[ which(results== -1,arr.ind = TRUE)[,2]]

 if (length(rnames) >0) cross = rbind(cross, data.frame(day=as.Date(rnames,format ="%Y-%m-%d" ),stk=gsub(".price","" ,cnames), type="D"))

 # temp =    (stkval[stkval$`^dji.date`%in%as.Date(rnames),  colnames(stkval)%in% cnames])

 cross$day.P = apply(cross, 1,function(x)  (stkval[which(x["day"]==stkval[1]), paste0(x["stk"],".price")] ) ) 
 # mapply(`[[`, dimnames(results),t(which(results==1,arr.ind = TRUE)))
 
return (cross)
}


# -------------------Driver ---------------------------------



library(RODBC)
ch <- odbcConnect("portfolio")
res <- sqlFetch(ch, "US Stocks Beta-High") #Fetch Query results to DF
res <- rbind(sqlFetch(ch, "US Stocks Beta-Low"), sqlFetch(ch, "US Stocks Beta-High"))
tickers <- res[,"symbol"]
endObservationDate = Sys.Date()-100
events = getCross(tickers[300:350],endObservationDate)
events
if(nrow(events)>1) {

events = cbind(events,day20=events$dates+20,day40=events$dates+40, day60=events$dates+60)

events$day20.P = apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day20"]),1) })
events$day20.Percent = round(((events$day20.P/events$price) -1)*100,digits=2)
events$day40.P = apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day40"]),1) })
events$day40.Percent = round(((events$day40.P/events$price) -1)*100,digits = 2)
events$day60.P = apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day60"]),1) })
events$day60.Percent = round(((events$day60.P/events$price) -1)*100,digits = 2)
events = events[,order(names(events))]
events
# getRollingAvg(asset = c("FFIV"),duration=15,endDate = as.Date("2015-06-25"),rollingperiod=1) 
}

close(ch)



#--------------------------------------------EOF-----------------
