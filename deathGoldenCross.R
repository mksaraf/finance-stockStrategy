
require(plyr)
source('https://github.com/vishalhawa/utilities/raw/master/util.R')

getCross<-function(asset,endDate = Sys.Date()){

rollingAvgShort = 70
rollingAvgLong = 225
idxrange = 45 # window of observations from end date: 60 = Quarter 
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

crossOverRegion = abs(percentileMatrix) < proximity
# ----Potential Cases: that have crossed in the crossover Region
potcases=crossOverRegion*(rollmeanShort-rollmeanLong)
priceCol = names(which(abs(apply(potcases,2,function(x){sum(ifelse(x<0 , x,0))  })*apply(potcases,2,function(x){sum(ifelse(x>0 , x,0))  }))>0))


slopeShort = round(1*diff(-1*rollmeanShort) /rollmeanShort[-idxrange,],digits = 3) # 

slopeLong = round(1*diff(-1*rollmeanLong) /rollmeanLong[-idxrange,],digits = 3)


# ifelse(slopeLong[1,] <0 & slopeShort[1,] <0,  print("Negative Slopes"),  ifelse(slopeLong[1,] >0 & slopeShort[1,] >0,print("Positive Slopes")  ,    print("Mixed Slopes")))

# the slopeMatrix provides wether long and short slopes are both positive , negative or Mixed : Potential Crosses
# slopeMatrix = ifelse(slopeLong <0 & slopeShort <0, -1,  ifelse(slopeLong>0 & slopeShort>0,1  ,  0))
# Confirmed Cases
slopeMatrix = ifelse(slopeLong <0 & slopeShort <0 & (rollmeanShort[-idxrange,]<rollmeanLong[-idxrange,]), -1,  ifelse(slopeLong>0 & slopeShort>0 & (rollmeanShort[-idxrange,]>rollmeanLong[-idxrange,]) ,  1 ,  0))

# This produces internal table of Death and Golden Cross : Debug use only
 crossdf = as.data.frame(apply(slopeMatrix*crossOverRegion[1:idxrange-1,], c(1,2), function(x)(if(x>0) x="Golden Cross" else if(x<0) x="Death Cross" else x="NA")) )

 row.names(crossdf) <- stkval[1:idxrange-1,"^dji.date"]

 # --- Creating Df for events : cross -----
 
 cross <- data.frame(day=as.Date(character()), stk=character(), type=character(),stringsAsFactors=FALSE) 
 
 results = as.data.frame(slopeMatrix*crossOverRegion[1:idxrange-1,])[,priceCol]
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
close(ch)

endObservationDate = Sys.Date()-100
#tickers <- res[,"symbol"]
# Just to make stuff slower
events = getCross(res[,"symbol"][801:828],endObservationDate)

for(k in seq(1,800,by=200)){
  print(paste("Tickers",k,":",(k+199)))
  events = rbind(events,getCross(res[,"symbol"][k:(k+199)],endObservationDate))
  
} 

events
# Unique Cases--G---
length(unique((events[events$type=="G",])$stk))
# Unique Cases--D---
length(unique((events[events$type=="D",])$stk))


if(nrow(events)>1) {
#--Look only for first day of the events----
events =  merge(aggregate(data=events,day ~stk,min),events,by=c("day","stk"))
  
events = cbind(events,day15=events$day+15,day40=events$day+40, day60=events$day+60,day90=events$day+90)

events$day.P = round(events$day.P,digits = 2)

events$day15.P = round(apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day15"]),1) }),digits = 2)
events$day15.Percent = round(((events$day15.P/events$day.P) -1)*100,digits=2)

events$day40.P = round(apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day40"]),1) }),digits = 2)
events$day40.Percent = round(((events$day40.P/events$day.P) -1)*100,digits = 2)

events$day60.P = round(apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day60"]),1) }),digits = 2)
events$day60.Percent = round(((events$day60.P/events$day.P) -1)*100,digits = 2)

events$day90.P = round(apply(events,1,function(x){getRollingAvg(x["stk"],1,as.Date(x["day90"]),1) }),digits = 2)
events$day90.Percent = round(((events$day90.P/events$day.P) -1)*100,digits = 2)

events = events[,order(names(events))]
events
# getRollingAvg(asset = c("FFIV"),duration=15,endDate = as.Date("2015-06-25"),rollingperiod=1) 
}

#-----------Improvising: Looking for peaks in ----------90 days Horizon ---------------

 peaks = lapply(apply(events,1,function(x){ getRollingAvg(x["stk"],duration=90,endDate=as.Date(x["day90"]),15)  }) ,function(x) data.frame(daysMax=(90-which.max(x)),max=round(max(x),digits = 2),daysMin=(90-which.min(x)),min=round(min(x),digits = 2)))

 events = cbind(peak=do.call(rbind,peaks),events)
 

#--------Results Display -----------
gcross = events[events$type=="G",]
dcross = events[events$type=="D",]

gcross.stats = data.frame(med15=median(gcross$day15.Percent),odds15=round(sum(gcross$day15.Percent>0)/nrow(gcross),digits = 2),med40=median(gcross$day40.Percent),odds40=round(sum(gcross$day40.Percent>0)/nrow(gcross),digits = 2), med60=median(gcross$day60.Percent),odds60=round(sum(gcross$day60.Percent>0)/nrow(gcross),digits = 2),med90=median(gcross$day90.Percent),odds90=round(sum(gcross$day90.Percent>0)/nrow(gcross),digits = 2))
dcross.stats = data.frame(med15=median(dcross$day15.Percent),odds15=round(sum(dcross$day15.Percent<0)/nrow(dcross),digits = 2),med40=median(dcross$day40.Percent),odds40=round(sum(dcross$day40.Percent<0)/nrow(dcross),digits = 2), med60=median(dcross$day60.Percent),odds60=round(sum(dcross$day60.Percent<0)/nrow(dcross),digits = 2),med90=median(dcross$day90.Percent),odds90=round(sum(dcross$day90.Percent<0)/nrow(dcross),digits = 2))

rbind(gcross.stats,dcross.stats)
# AT least Favourable once
1-sum(apply(gcross[,grep("*.Percent",colnames(events))],1,function(x){x["day15.Percent"]<0 & x["day40.Percent"]<0 & x["day60.Percent"] <0 & x["day90.Percent"] <0}))/nrow(gcross)
1-sum(apply(dcross[,grep("*.Percent",colnames(events))],1,function(x){x["day15.Percent"]>0 & x["day40.Percent"]>0 & x["day60.Percent"] >0 & x["day90.Percent"] >0}))/nrow(dcross)

#-------GCROSS ------------------
plot(gcross[,"peak.daysMax"],gcross[,"peak.max"]/gcross[,"day.P"])

median(gcross[,"peak.max"]/gcross[,"day.P"])
median(gcross[,"peak.daysMax"])
paste("% Success:",100*(sum(gcross[,"peak.max"]/gcross[,"day.P"] >1)/nrow(gcross)),"%")
cor.test(gcross[,"peak.daysMax"],gcross[,"peak.max"]/gcross[,"day.P"])


#-------DCROSS ------------------
plot(dcross[,"peak.daysMin"],dcross[,"peak.min"]/dcross[,"day.P"])

median(dcross[,"peak.min"]/dcross[,"day.P"])
median(dcross[,"peak.daysMin"])
paste("% Success:",100*(sum(dcross[,"peak.min"]/dcross[,"day.P"] <1)/nrow(dcross)),"%")
cor.test(dcross[,"peak.daysMin"],dcross[,"peak.min"]/dcross[,"day.P"])


#--------------------------------------------EOF-----------------
