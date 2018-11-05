## Each quarter, investor looks for companies which announce stock repurchase program (with announced buyback for at least 5% of outstanding stocks)
# during days -30 to -15 before earnings announcement date for each company. 
# Investor goes long stocks with announced buybacks during days -10 to +15 around earnings announcement. 
# Portfolio is equally weighted and rebalanced daily.

library(stringr)

getStockPurchase<-function(){
    url <- paste0("https://www.marketbeat.com/stock-buybacks/")
    webpage <- readLines(url)
    html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
    tableNodes <- getNodeSet(html, "//table")
    dt = as.data.table(readHTMLTable(tableNodes[[1]]))
    dt$Date = as.Date(dt$Date,format = '%m/%d/%Y')
    dt$symbol = gsub('\\(|\\)','',str_extract(dt$Company, '\\([[:alpha:]]+\\)$'))
    dt$`Percent of Shares`  = as.numeric(sub('%','',dt$`Percent of Shares`))/100
dt 
}

stockPurchaseTable = funion(stockPurchaseTable,getStockPurchase())
stockPurchaseTable  ## stock purchase information 

 ## Merge with earnings date future and measure performance at earnings and also 15 days later 
## filter > 5% 