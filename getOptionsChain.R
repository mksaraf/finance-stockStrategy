
library(jsonlite)

fetchOptionsChain<-function(date,stk) {

document <- fromJSON(txt=paste0('https://query2.finance.yahoo.com/v7/finance/options/',stk,'?date=',date),flatten = T)
calls = as.data.frame(as.data.frame(document$optionChain$result$options)[,"calls"])
puts = as.data.frame(as.data.frame(document$optionChain$result$options)[,"puts"])

if(nrow(calls)<1 | nrow(puts)<1) { return(NA)}
calls$currency = "calls"
puts$currency = "puts"

options = rbind(calls,puts)

options$expiration  = as.Date(options$expiration/86400,origin = "1970-01-01")
options$lastTradeDate  = as.Date(options$lastTradeDate/86400,origin = "1970-01-01")

options$type=options$currency 
options$currency = NULL
return(options)
}

getOptions<-function(symbol){
cat('Optioning: ',symbol,'\n')
document=fromJSON(txt=paste0('https://query2.finance.yahoo.com/v7/finance/options/',symbol))
dates = document$optionChain$result$expirationDates

options = tryCatch(do.call(rbind,lapply(dates[[1]], fetchOptionsChain,stk=symbol)), error = function(e) e)
options$stock <-symbol
names(options)[names(options) == 'contractSymbol'] <- 'OS'
names(options)[names(options) == 'openInterest'] <- 'OI'

options=na.omit(options)
return(options)
}
