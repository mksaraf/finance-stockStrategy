
require(Quandl)
Quandl.api_key('r6fMvypDu9Kp8tRwvDLU')
 Quandl(c('GOOG/NASDAQ_ABDC', 'GOOG/NASDAQ_MSFT'), type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 Quandl(c('YAHOO/AAPL', 'YAHOO/MSFT'), type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 
 ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")
 stk.tab <- data.table(sqlFetch(ch, "USStocks")) #Fetch Query results 
 setkey(stk.tab,symbol)

 code = paste0('YAHOO/',(stk.tab[,.(symbol)]$symbol)[1:350],'.4')
 
 hist = Quandl(code, type="xts",start_date=Sys.Date()-30,end_date=Sys.Date(),column_index=11)
 colnames(hist) = ticker[1:350]
 Quandl('ZEP/ABBV', type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 Quandl('EOD/GOOG', type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 Quandl(c('YAHOO/DRW','YAHOO/AAPL'), type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 
 Quandl(c('NSE/OIL.5'), type="xts",start_date=Sys.Date()-30,end_date=Sys.Date()) 
 
 Quandl(c('WIKI/DRW','WIKI/MSFT'), type="xts",start_date=Sys.Date()-30,end_date=Sys.Date())
 