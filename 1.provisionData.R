
## Data Provison Scripts ###

library(jsonlite)
library(data.table)
library(ggplot2)
library(PerformanceAnalytics)
library(quantmod)
library(caret)

plotScatter <- function(data_in, x, y,other_aes = 'cap_mm'){ 
  data <- na.omit(data_in ); #print(data)
  p <- ggplot(data= data, aes(x = data[[x]], y=data[[y]],size=other_aes)) + geom_point(color='purple', alpha=0.3)
  p = p + geom_smooth(method = lm) 
  p = p + xlab(paste0(x, '\n', 'CORR: ', round(cor(data[[x]], data[[y]]), 2)))+ ylab(y) + theme_light() 
  return(p) 
}


#earn <- as.data.table(fromJSON("https://api.earningscalendar.net/?date=20180101"))

getPrices<-function(asset="SPY",startDate=Sys.Date()-365,endDate = Sys.Date()){
  allPrices = getSymbols(asset,from = startDate, to = endDate,env=NULL)
  allPrices = rbind((allPrices[,1:5]),xts(order.by = Sys.Date(),getQuote(asset)[,c("Open","High","Low","Last","Volume")]))  ## get todays Price 
  return(allPrices)
}

 

getEarnings <- function(from,to) {
  days = gsub('-','',seq(as.Date(from), as.Date(to),by='day'))
  earn = lapply(days , function(d) {
    Sys.sleep(1)
    url = paste0("https://api.earningscalendar.net/?date=", d)
    print(url)
    earn =  as.data.table(fromJSON(url))
    if (nrow(earn) > 0) {
      earn[, .(symbol = ticker,
               earnDate = as.Date(d, format = '%Y%m%d'),
               when,
               cap_mm = as.numeric(gsub(',', '', cap_mm)))]
    } else {
      data.table( earnDate = as.Date(d, format = '%Y%m%d'))
    }
  })

  earn = rbindlist(earn,fill = T)
  earn$actionDate = as.Date(ifelse(earn$when=='amc',earn$earnDate+1,earn$earnDate),origin = '1970-01-01')  ## adjust for when=amc
  return(earn)
}



prepareData<-function(asset,startDate=Sys.Date()-365){  ## change: expected move
  
  endDate = Sys.Date() ;   startDate = as.Date(startDate  )
  allPrices = getPrices(asset,startDate ,endDate )
  dt = cbind(as.data.table(allPrices),asset=asset)
  colnames(dt)<- c('index','Open','High','Low','Close','Volume','Symbol')
  return(dt)
}




### DRIVER 
fromDate = '2018-10-22'
toDate =   Sys.Date()
 load(file = 'earningsCalendar.rdata')  ## if earn is not loaded
earn = rbind(earn,getEarnings(fromDate,toDate),fill=T)
earn[, cat :=cut(earn$cap_mm,c(0,500,2000,6000,15000,Inf),labels = c('tiny','small','mid','large','mega'))]
save(earn,file = 'earningsCalendar.rdata')

## Filter database uss symbols
library(readr)
uss <- as.data.table(read_csv("USStocks.csv"))
uniqueN(earn[symbol %in% uss$symbol]$symbol)

# # load(file = 'stkdata.rdata',verbose=T)  ## if not loaded   
# stkdata = rbindlist(lapply(unique(earn$symbol), function(st) { 
#   tryCatch (prepareData(st,startDate = '2017-01-01'),  error = function(e) data.table(symbol=st)) }),fill = T)

incr_stkdata = rbindlist(lapply(unique(earn[symbol %in% uss$symbol]$symbol), function(st) { 
  tryCatch (prepareData(st,startDate = fromDate),  error = function(e) data.table(Symbol=st)) }),fill = T)

stkdata = rbind(stkdata,incr_stkdata)
save(stkdata,file = 'stkdata.rdata')

## earn and stkdata : dataset for analysis 




