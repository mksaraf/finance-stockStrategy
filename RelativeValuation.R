#######################################################################
##To Perform Relative Valuation
#######################################################################

library("RODBC")
library("data.table")



ch<-odbcDriverConnect("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=../../FirmFinancials/USStocks.accdb")
stks <- sqlFetch(ch, "USStocks") #Fetch Query results 

dt = data.table(stks)

sectors = dt[,list(counts=length(symbol)),by=list(Sector, Industry)]

ind.pharma =  dt[Industry=="Major Pharmaceuticals",list(symbol,Name,PBV,ROE)]

close(ch)