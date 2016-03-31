# Update Portfolio
library("quantmod")
library("RODBC")
library("RQuantLib")

setwd(paste0("C:/Users/",Sys.getenv("username"),"/Dropbox/Projects/RCode/finance-stockStrategy/"))
source('../utilities/util.R') # This is for Greeks calculation

fetchOptions<-function(stock,exp=c("2016","2017")){
  options<-do.call(rbind,lapply(getOptionChain(Symbols=stock,Exp=exp), function(x) do.call(rbind, x))) 
  options$OS<-gsub("^.*\\.","",rownames(options)) # option symbols
  options$stock <- gsub(pattern="[[:digit:]]*\\D[[:digit:]]*$","",options$OS)
  return (options)
}


print("Updating Share Stats in DB...")
source('keyStats.R') # This is for Share Stats

rf = getQuote("^TYX")$Last/100

ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")
res <- sqlFetch(ch, "USPortfolio") #Fetch Query results 

symbols = gsub(pattern="[[:digit:]]*\\D[[:digit:]]*$","", (res[res$Remarks %in% c("BC","BP","SC","SP"),c("symbol")]))

stkdata<-(t(sapply(symbols,function(x)sqlQuery(ch,paste0("select ClosePrice,Volatility20,DividendYield from USStocks where symbol='",x,"'")))))
stkdata<-data.frame(apply(stkdata,c(1,2),as.numeric))
stkdata$stock<-rownames(stkdata)

print("Fetching Options...")

options<-merge(do.call(rbind,lapply(symbols, fetchOptions)),stkdata,by="stock")
# options<-options[complete.cases(options),] # filter out cases where data is incomplete

options<-cbind(options,apply(t(mapply(options$OS,FUN = calculateGreeks,stkPrice=options$ClosePrice,dailyVolatility=options$Volatility20,dy=options$DividendYield/100)),c(1,2),as.numeric))

# options=cbind(options,(t(mapply(options$OS,FUN = getOptionPrice,stkPrice=options$ClosePrice,yearlyVol=options$Volatility20))))
# apply(t(mapply(options$OS,FUN = getOptionPrice,stkPrice=options$ClosePrice,yearlyVol=options$Volatility20)),c(1,2),as.numeric)

print("Inserting Options in DB...")

sqlDrop(ch, "USOptionsTable")
sqlSave(ch, options, tablename="USOptionsTable",rownames = FALSE)

print("Updating Portfolio.. DB...")
portfoliodf = sqlQuery(channel = ch, "Select ID,USStocks.symbol,Number,BuyPrice,BuyDate,USStocks.ClosePrice from USPortfolio,USStocks where USStocks.symbol=USPortfolio.symbol",stringsAsFactors=FALSE)
portfoliodf$Profit = (portfoliodf$ClosePrice-portfoliodf$BuyPrice)*portfoliodf$Number
portfoliodf$AbsoluteGrowth = (portfoliodf$ClosePrice/portfoliodf$BuyPrice -1)
portfoliodf$AnnualizedGrowth = portfoliodf$AbsoluteGrowth/(as.numeric(Sys.Date()-as.Date(portfoliodf$BuyDate))/365)
portfoliodf$CostBasis = (portfoliodf$BuyPrice)*portfoliodf$Number
portfoliodf$LastUpdated = Sys.Date()

# The options profit , growth etc is not getting updates just yet..

sqlUpdate(ch,dat=portfoliodf,tablename="USPortfolio",index=c("ID"))

close(ch)

print("Closing.. DB...End of R: Update Portfolio")




# # extract Dates
# as.Date(gsub("*\\D[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)","",c("CVX160311C00077000","V160422C00045000"))),format = "%y%m%d")
# # Strikes
# as.numeric(gsub(pattern="(^[[:alpha:]]+)\\d{6}[[:alpha:]]","",c("CVX160311C00074500","V160422C00045000")))/1000
# # Type
# (gsub("[[:digit:]]*$","",gsub(pattern="(^[[:alpha:]]+)\\d{6}","",c("CVX160311C00077000","V160422P00045000"))))

# 
  # lapply("USO", fetchOptions)
# getOptionPrice("V160617P00070000",71.63,0.015,dy=0.0079,rf=0.0267)

# 


# Treasury Yield 30 Years (^TYX)