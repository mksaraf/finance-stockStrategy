
################# Finance :  NYSE Quoter STOCK Updates  ##################################

############## INput: Takes stock symbols from the database ##
############## Process: Gets Key Stats & STK data from Yahoo URL / Query  ### 
############## Process: Store into database/USSTOCKS ###
############## Process: gets Dividends and NAVs Store into database/USFund ## 

library(RODBC)
library(fOptions)
library(data.table)
library(DBI)
library(quantmod)

processNumber<-function(x){
  
  x=gsub(",","",x)
  x=gsub("%","/1",x) # Div by 1 is just to make it values compatible with Java program
  x=gsub("N/A","0",x)
  x=gsub("K","*1000",x)
  x=gsub("M","*1000000",x)
  x=gsub("B","*1000000000",x)
  x=gsub("T","*1000000000000",x)
  x=eval(parse(text=x))
  return(x)
}

setwd(paste0("C:/Users/",Sys.getenv("username"),"/Dropbox/Projects/RCode/finance-stockStrategy/"))

ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")

# con = DBI::dbConnect(odbc::odbc(), driver = "{Microsoft Access Driver (*.mdb, *.accdb)}",  Dbq   = "../../FirmFinancials/USStocks.accdb")
# stk.tab <- data.table(dbReadTable(con,'USStocks'))

# DBI::dbConnect(odbc::odbc(), driver = "{Microsoft Access Driver (*.mdb, *.accdb)}",  dsn = "USStocks")
# dbc::dbConnect( drv = "{Microsoft Access Driver (*.mdb, *.accdb)}",  database = "../../FirmFinancials/USStocks.accdb")

#### US Stocks  ####
#- Key Stats to be done ################ ### Update USStocks table #######  if Volume==0 then its an OLD symbol in this case
cat('Updating  USStocks ...') ###
stk.tab <- data.table(sqlFetch(ch, "USStocks")) #Fetch Query results 
setkey(stk.tab,symbol)

whatToGet = c('Last Trade (Price Only)','Price/Sales','Price/Book','P/E Ratio','PEG Ratio','Price/EPS Estimate Current Year','Short Ratio','Volume','Average Daily Volume','Dividend Yield','Dividend/Share')
whatToGet = c(whatToGet,'Book Value','Change','Earnings/Share','EPS Estimate Current Year','EPS Estimate Next Year','Market Capitalization','EBITDA','50-day Moving Average','200-day Moving Average')

ticker = (stk.tab[,.(symbol)]$symbol)
 # ticker = c(as.character(ticker),'AAPL')
# quotes = data.table(getQuote(paste(ticker,collapse = ';'),what = yahooQF(whatToGet)),keep.rownames = T) #[,.(symbol=rn,ClosePrice=as.numeric(Last),Volume,LastUpdated=Sys.Date(),LastTrade=as.Date(`Trade Time`))]
## Unitl getQuotes gets fixed blocked unitl quotes = 
# 
# setnames(quotes,4:22,c('PS','PBV','PECurr','PEG5','PEPSCurr','ShortRatio','Volume','VolumeAvg','DividendYield','DividendRate','BookValue','Change','EPS','EPSCurr','EPSFwd','MCap','EBITDA','MA50','MA200'))
# setnames(quotes,c('rn','Trade Time','Last'),c('symbol','LastTrade','ClosePrice'))
# setkey(quotes,'symbol')
# 
# quotes$MCap=unlist(lapply(quotes$MCap,processNumber))
# quotes$EBITDA=unlist(lapply(quotes$EBITDA,processNumber))
# quotes= cbind(quotes[,1:2],apply(quotes[,-c(1,2)],c(1,2),as.numeric))  # Check NA intro by coearcion 
# quotes$LastUpdated=Sys.Date()
# 
# quotes$Name = stk.tab$Name
# quotes$Sector = stk.tab$Sector
# quotes$Industry = stk.tab$Industry   today =  tryCatch(getQuote(asset),error=function(e) data.frame(asset=asset))

lookBack = 100 ## for RSI etrc 
quoteL  = (lapply(as.vector(ticker), function(asset) tryCatch(Ad(getSymbols(asset,from = Sys.Date()-lookBack, to = Sys.Date(),auto.assign = F)),error=function(e) data.frame(asset = NA)) ))
quoteL = lapply(quoteL,function(x) na.omit(x))  ## remove all NA vqalues 
quoteL = quoteL[unlist(lapply(quoteL,function(x) nrow(x) > 20 ))]  ## Removing all NA or obselete tickers 
gain =   unlist(lapply(quoteL,function(t) prod(1+ROC(t,n=1,type = 'discrete'),na.rm = T) )) ## in absolute fraction 
gain10 = unlist(lapply(quoteL,function(t) prod(1+ROC(last(t,10),n=1,type = 'discrete'),na.rm = T) ) ) ## in past 10 days   
gain20 = unlist(lapply(quoteL,function(t) prod(1+ROC(last(t,20),n=1,type = 'discrete'),na.rm = T) ) ) ## in past 20 days  

volatility20 = unlist(lapply(quoteL,function(t) sd(ROC(last(t,20),n=1,type = 'discrete'),na.rm = T )) ) ## in past 20 days  
volatility =  unlist( lapply(quoteL,function(t) sd(ROC(t,n=1,type = 'discrete'),na.rm = T )) )

quotes = rbindlist(lapply(quoteL,function(st) data.table(symbol=sub('.Adjusted','',names(st)),ClosePrice=as.numeric(st),RSI5=as.numeric(RSI(st,3)), roll10=as.numeric(EMA(st)))[.N]))

quotes[, `:=`(gain=gain,gain10=gain10,gain20=gain20,volatility20 = volatility20,volatility=volatility,LastUpdated=Sys.Date()-1)] 
setkey(quotes,'symbol')

## process Dividends 
cat('Updating Dividends.. USStocks ...') ###
div =   rbindlist(lapply(quotes$symbol,function(x){ setnames(data.table(getDividends(x,from = Sys.Date()-366)),1,c('div'))[, c(.SD[.N],.SD[1],sum(.SD[,div]) )]   }) ,fill = T)[,1:3] #setnames(n,1:2,c("D","E")) 
div = setnames(cbind(div,quotes[,.(symbol,ClosePrice)]),1:5,c('div1','divN','divSum','symbol','ClosePrice'))
div$DividendGrowth = div[,.(div1/divN-1)]
div$DividendYield = div[,.(divSum/ClosePrice)]
setkey(div,'symbol')
quotes = quotes[div][,-c('i.ClosePrice','Volume','div1','divN','divSum')] 


cat('Updating DB.. USStocks ...') ###
sqlUpdate(ch,dat=quotes, tablename = 'USStocks',index = c('symbol'))  #Caution: Older Columns will not be updated 

# Stack USStocks file to Web App for pick up 
print('Writing CSV.. USStocks ...')
# write.csv(quotes,paste0('C:/Users/',Sys.getenv("username"),'/Dropbox/Projects/RCode/finance-WebApps/bollingerPops/USStocks.csv'))
# write.csv( data.table(sqlFetch(ch, "US High Yield")),paste0('C:/Users/',Sys.getenv("username"),'/Dropbox/Projects/RCode/finance-WebApps/bollingerPops/USHighYield.csv'))

write.csv( data.table(sqlFetch(ch, "USStocks")),'USStocks.csv')  ## Generalize  this view .. for all data to shiny 
# write.csv( data.table(sqlFetch(ch, "US High Yield")),'USHighYield.csv')

##### US FUNDS - with Div Yield and growth  #########
print('Processing USFunds ...')
funds.tab <- data.table(sqlFetch(ch, "USFund")) #Fetch Query results 
setkey(funds.tab,symbol)

ticker = as.character(funds.tab[,.(symbol)]$symbol)
lookBack = 100  
# quotes = data.table(getQuote(paste(ticker,collapse = ';')),keep.rownames = T)[,.(symbol=rn,NAV=as.numeric(Last),Volume,LastUpdated=Sys.Date(),LastTrade=as.Date(`Trade Time`))]
fundL  = lapply(as.vector(ticker), function(asset) tryCatch(Ad(getSymbols(asset,from = Sys.Date()-lookBack, to = Sys.Date(),auto.assign = F)),error=function(e) data.frame(asset = NA)) )
fundL = lapply(fundL,function(x) na.omit(x))  ## remove all NA vqalues 
fundL = fundL[unlist(lapply(fundL,function(x) nrow(x) > 0 ))]  ## Removing all NA or obselete tickers 
quotes = rbindlist(lapply(fundL,function(st) data.table(symbol=sub('.Adjusted','',names(st)), NAV=as.numeric(st),LastUpdated = Sys.Date()-1)[.N]))
gain20 = unlist(lapply(fundL,function(t) prod(1+ROC(last(t,20),n=1,type = 'discrete'),na.rm = T) ) ) ## in past 20 days  
quotes[,`:=`(gain20=gain20)]
setkey(quotes,'symbol')

cat('Funds...Getting Dividends ')

div =   rbindlist(lapply(quotes$symbol,function(x){ setnames(data.table(getDividends(x,from = Sys.Date()-366)),1,c('div'))[, c(.SD[.N],.SD[1],sum(.SD[,div]) )]   }) ,fill = T)[,1:3] #setnames(n,1:2,c("D","E")) 
div = setnames(cbind(div,quotes[,.(symbol,NAV)]),1:5,c('div1','divN','divSum','symbol','NAV'))
div$dG = div[,.(div1/divN-1)]
div$dY = div[,.(divSum/NAV)]
setkey(div,'symbol')
quotes = quotes[div][,-c('i.NAV','Volume','div1','divN','divSum')] 

sqlUpdate(ch,dat=quotes, tablename = 'USFund',index = c('symbol'))

close(ch)

print('Processing Done ...')
