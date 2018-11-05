
################# Finance :  Stock Quoter  STOCK Updates for Indian market  ##################################

############## INput: Takes stock symbols from the database ################
############## Process: Gets Key Stats & STK data from Yahoo URL / Query  ################
############## Process: Store into database/USSTOCKS/INStocks ################


library(quantmod)
library(RODBC)
library(RQuantLib)
library(data.table)

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

####  ################ ### Update INSTOCKS table #######   

stk.tab <- data.table(sqlFetch(ch, "INStocks",stringsAsFactors = F)) #Fetch Query results 
setkey(stk.tab,symbol)

symbolList = rbindlist(lapply(stk.tab$symbol,function(x) tryCatch(data.table(getSymbols(x,from=Sys.Date()-2,env = NULL),x )[,6:7],error=function(e) data.table(NA,x)) ), fill = F)
stocks.in = symbolList[symbolList[,max(.I),keyby=x][,V1],]
stocks.in$LastUpdated = Sys.Date()
setnames(stocks.in,1:2,c('ClosePrice','symbol'))

sqlUpdate(ch,dat=stocks.in, tablename = 'INStocks',index = c('symbol'))

####  ################ ### Update INPortfolioTable #######   

pf = data.table(sqlQuery(channel = ch, "Select INStocks.symbol,Number,BuyPrice,BuyDate,INStocks.ClosePrice from INPortfolioTable,INStocks where INStocks.symbol=INPortfolioTable.symbol",stringsAsFactors=FALSE))

pf$Profit = (pf$ClosePrice-pf$BuyPrice)*pf$Number
pf$AbsoluteGrowth = (pf$ClosePrice/pf$BuyPrice -1)
pf$AnnualizedGrowth = pf$AbsoluteGrowth/(as.numeric(Sys.Date()-as.Date(pf$BuyDate))/365)
pf$CostBasis = (pf$BuyPrice)*pf$Number
pf$LastUpdated = Sys.Date()

sqlUpdate(ch,dat=pf[,-c('ClosePrice')],tablename="INPortfolioTable",index=c("symbol"))


close(ch)