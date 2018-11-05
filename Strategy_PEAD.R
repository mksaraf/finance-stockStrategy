# Strategy PEAD ###
# get Next weeks stock earnings
# Filter for optionable > 5 
library(dplyr)

earnNextWeek = getEarnings(Sys.Date()+1,Sys.Date()+7)

opList = lapply(unique(earnNextWeek$symbol), function(s) {

      oplist = tryCatch({nr=do.call(rbind,getOptionChain(Symbols=s));data.table(s,nrow(nr))}, error=function(x){data.table(s,0 ) })
      (oplist)
})

opFilter = rbindlist(opList,fill = T)[V2>5] ## stocks with options > 5 
earnNextWeek[symbol %in% opFilter$s][cap_mm>10000]

## How did they perform in past ?
earn_stk[earnNextWeek[symbol %in% opFilter$s][cap_mm>1000]$symbol]%>%head(50)
