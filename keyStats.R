#######################################################################
##To download all key stats using XML and x_path – PREFERRED WAY
#######################################################################

library("quantmod")
library(RODBC)
require(XML)
require(plyr)

getAnalystOpinion <- function(symbol) {

  yahoo.URL <- "http://finance.yahoo.com/q/ao?s="
  html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")
  
  #search for <td> nodes anywhere that have class ‘yfnc_tablehead1′
  nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1']")
  if(length(nodes) > 0 ) {
    measures <- sapply(nodes, xmlValue)
    values <- sapply(nodes, function(x)  xmlValue(getSibling(x)))
  }
     df= data.frame(measures,values)
     return( df[!(is.na(df$measures) | df$measures==""), ])   
}

# getAnalystOpinion("VRX")


getKeyStats <- function(symbol) {
 # print(symbol)
  yahoo.URL <- "http://finance.yahoo.com/q/ks?s="
  html_text <- htmlParse(paste(yahoo.URL, symbol, sep = ""), encoding="UTF-8")

  #search for <td> nodes anywhere that have class ‘yfnc_tablehead1′
  nodes <- getNodeSet(html_text, "/*//td[@class='yfnc_tablehead1']")
  
  if(length(nodes) > 0 ) {
    measures <- sapply(nodes, xmlValue)
    
    #Clean up the column name
    measures <- gsub( "*[0-9]*:", "", gsub(" \\(.*?\\)[0-9]*:","", measures))   
    
    #Remove dups
    dups <- which(duplicated(measures))
    #print(dups) 
    for(i in 1:length(dups)) 
      measures[dups[i]] = paste(measures[dups[i]], i, sep=" ")
    
    #use siblings function to get value
    values <- sapply(nodes, function(x)  xmlValue(getSibling(x)))
    
    df <- data.frame(t(values),"symbol"=symbol)
    colnames(df) <- c(measures,"symbol")
    return(df)
  }  #if 
}

getShareStats<-function(symbol){
  endDate = Sys.Date()
  startDate = endDate-365 #year's worth
  dividendRate = 0
  dividendYield = 0
  dividendChange = 0
  
 prices = OHLC(getSymbols(symbol,from = startDate, to = endDate,env=NULL,warnings = FALSE))
 vol20 = volatility(prices,n=20)/sqrt(260) # this is daily volatility
 roll20 = rollapply(Cl(prices),width=20,align = "right",FUN = mean ) # align = "left" is fwd looking 
 roll90 = rollapply(Cl(prices),width=min(90,nrow(prices)),align = "right",FUN = mean )
 ClosePrice = getQuote(symbol)$Last
 dividends = getDividends(symbol,from = startDate, to = endDate,env=NULL,warnings = FALSE)
 if(length(dividends)>0){
     dividendRate = sum(dividends)
     dividendYield = sum(dividends)/ClosePrice
     dividendChange = dividends[[nrow(dividends)]]-dividends[[1]] # [[]] gives me  numeric class
 }
 df=data.frame(vol20,roll20,roll90,ClosePrice,dividendRate,dividendYield,dividendChange)
 colnames(df)<-c("Volatility20","RollingAvg20","RollingAVG","ClosePrice","DividendRate","DividendYield","DividendChange")
 return (df[nrow(df),])
}


processNumber<-function(x){

  x=gsub(",","",x)
  x=gsub("%","/100",x)
  x=gsub("N/A","0",x)
  x=gsub("M","*1000000",x)
  x=gsub("B","*1000000000",x)
  x=eval(parse(text=x))
  return(x)
}





ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")

symbols <- sqlQuery(ch, "Select symbol from USPortfolio where Remarks<>'MF'",stringsAsFactors=FALSE) #Fetch Query results 

symbols <- unique(gsub(pattern="[[:digit:]]{6}\\D[[:digit:]]{8}$",replacement="", symbols$symbol )) # Collect set of ticker symbols from Stock and options


stats <- (ldply(symbols, getKeyStats))

keyStats= stats 

deleteCols = c("Enterprise Value/Revenue","Enterprise Value/EBITDA","Fiscal Year Ends","Most Recent Quarter","Revenue Per Share","Net Income Avl to Common","Total Cash Per Share","Book Value Per Share","52-Week Change","S&P500 52-Week Change")
deleteCols = c(deleteCols,c("Shares Outstanding","Float","% Held by Insiders","% Held by Institutions","Shares Short","Shares Short 2","Trailing Annual Dividend Yield","Trailing Annual Dividend Yield 3"))
deleteCols = c(deleteCols,c("5 Year Average Dividend Yield","Payout Ratio","Dividend Date","Ex-Dividend Date","Last Split Factor","Last Split Date") )
deleteCols = c(deleteCols,c("Forward Annual Dividend Rate","Forward Annual Dividend Yield") )

keyStats<-keyStats[, !names(keyStats) %in% deleteCols]


keyStats<-rename(keyStats, c("Diluted EPS"="EPS", "Total Debt/Equity"="DebtRatio","Operating Margin"="OPM","Profit Margin"="NPM","52-Week Low"="LowValue","52-Week High"="HighValue")) 
keyStats<-rename(keyStats, c("Trailing P/E"="PECurr","Return on Equity"="ROE"))
keyStats<-rename(keyStats, c("Forward P/E"="PEForward","PEG Ratio"="PEG5","Price/Sales"="PS","Price/Book"="PBV","Market Cap"="MCap","Enterprise Value"="BookValue","Avg Vol"="VolumeAvg","Avg Vol 1"="VolumeAvg20","50-Day Moving Average"="MA50"))
keyStats<-rename(keyStats, c("200-Day Moving Average"="MA200","Levered Free Cash Flow"="FCFF","Operating Cash Flow"="OCFF","Return on Assets"="ROA"))

shorts = apply(data.frame(stats$`Shares Short`,stats$`Shares Short 2`),c(1,2),processNumber)
keyStats$ShortGrowth <-    shorts[,1]/shorts[,2] -1
keyStats=as.data.frame(apply(subset(keyStats,select= -c(symbol)), c(1,2), FUN=processNumber))

keyStats$symbol <- stats$symbol

temp = data.frame(t(sapply(symbols, getShareStats ))) 
temp$symbol = rownames(temp)
keyStats = merge(keyStats,temp,all.y = TRUE)
keyStats$LastUpdated <- Sys.Date()

sqlUpdate(ch, dat=keyStats, tablename = "USStocks",index=c("symbol"))


close(ch)



#write.csv(stats, "FinancialStats_updated.csv",row.names=TRUE)  

# -----------------EOF ----------------------------