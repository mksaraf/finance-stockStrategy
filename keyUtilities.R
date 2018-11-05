library(XML)

stocks <- c("AXP","BA",'aapl','bb','cat','cost')

getKeyStats <- function(stk){
  stk = toupper(stk)
  url <- paste0("http://finviz.com/quote.ashx?t=", stk)
  webpage <- readLines(url)
  html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
  tableNodes <- getNodeSet(html, "//table")
  
  # ASSIGN TO STOCK NAMED DFS
  dt = data.table(readHTMLTable(tableNodes[[9]], header= c("data1", "data2", "data3", "data4", "data5", "data6",
                                                      "data7", "data8", "data9", "data10", "data11", "data12")))
  dt = rbind(dt[,.(data1,data2)],dt[,.(data3,data4)],dt[,.(data5,data6)],dt[,.(data7,data8)],dt[,.(data9,data10)],dt[,.(data11,data12)],use.names=F )
  dt = cbind(stk,dt)
  colnames(dt)<- c('stk','key','value')
  dt = dcast(dt,stk~key,value.var = 'value',fun=I,fill = NA)

  return(dt)

  
}
slist = lapply(stocks,function(s) getKeyStats(s))
rbindlist(slist)
 

## Update Optionable Stocks
library(data.table)
library(odbc)
uss = DBI::dbReadTable(dbConnect(odbc::odbc(), "USStocks"),'USStocks')
optionable = rbindlist(sapply(uss$symbol,function(s) {data.table(nrow(getOptions(s))) }),idcol = T) ## to refresh optionalble 
load(file = 'symbols.liq.rdata') ## Alternate 

lapply(optionable[V1>30]$.id, function(x) {up = paste0("Update USStocks Set Options=1 where symbol ='",x,"'"); DBI::dbExecute(con, up  )})
## END 

