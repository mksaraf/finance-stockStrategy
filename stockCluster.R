# Formation of Stock CLusters
require(plyr)
library(RODBC)
ch <- odbcConnect("portfolio")
sqlks = "select USStocks.symbol,	MarketCap,	EnterpriseValue,	TrailingPE,	ForwardPE,	PEGRatio,	PriceSales,	PriceBook,	EnterpriseValueRevenue,	EnterpriseValueEBITDA,	ProfitMargin,	OperatingMargin,	ReturnonAssets,	ReturnonEquity,	USStocksKS.Revenue,	RevenuePerShare,	QtrlyRevenueGrowth,	GrossProfit,	EBITDA,	NetIncomeAvltoCommon,	DilutedEPS,	QtrlyEarningsGrowth,	TotalCash,	TotalCashPerShare,	TotalDebt,	TotalDebtEquity,	CurrentRatio,	BookValuePerShare,	OperatingCashFlow,	LeveredFreeCashFlow,	USStocks.Beta,		AvgVol,	AvgVol1,	ForwardAnnualDividendYield,	TrailingAnnualDividendYield3, "
sql = "Sector, AnalystOpinion, RollingAVG, Volatility, Kurtosis, RSI"
sql = paste(sqlks,sql,"FROM USStocks,USStocksKS where USStocks.symbol = USStocksKS.symbol",collapse = "\t")
sql = gsub("\t","",sql)

stkdata <- sqlQuery(ch, sql) #Fetch Query results to DF
close(ch)

# Prepare Data
# change to numeric types and then replace NA
stkconvert<-stkdata[,c(2:3,15,18:20,23,25,29:30)]
stknochange<-stkdata[,-c(2:3,15,18:20,23,25,29:30)]

stkconvert = as.data.frame(sapply(stkconvert, function(x) gsub("T", "*1000000000000", x)))
stkconvert = as.data.frame(sapply(stkconvert, function(x) gsub("B", "*1000000000", x)))
stkconvert = as.data.frame(sapply(stkconvert, function(x) gsub("M", "*1000000", x)))
stkconvert = as.data.frame(sapply(stkconvert, function(x) gsub("K", "*1000", x)))
stkconvert = as.data.frame(sapply(stkconvert, function(x) gsub("N/A", 0, x)))

stkconvert = apply(stkconvert,2,function(x)sapply(x,function(x)(eval(parse(text=as.character(x)))))) # Slow but WORKS

stkdata = data.frame(stkconvert,stknochange) # The row order remains the same
stkdata[is.na(stkdata)] <- 0 # replace NA with 0
stkval = stkdata
stkdata = subset(stkdata,select = -c(symbol,Sector))
stkdata <- scale(stkdata) # standardize variables

# Determine number of clusters
k= 200 # No of clusters to Test
wss <- (nrow(stkdata)-1)*sum(apply(stkdata,2,var))
for (i in 2:k) wss[i] <- sum(kmeans(stkdata, centers=i,iter.max = 50,nstart = 25)$withinss)
plot(1:k, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# Hartigan's Rule 
#(sum(k$withinss)/sum(kplus1$withinss)-1) * (nrow(x)-k-1) is greater than 10 then move to k+1 groups  
(wss[k-1]/wss[k]-1) * (nrow(stkdata)-k-1)    


# K-Means Cluster Analysis
fit <- kmeans(stkdata, k,nstart = 25) # k cluster solution
fit$betweens / fit$totss
hist(fit$size)

# get cluster means 
aggregate(stkdata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
stkdata <- data.frame(stkval[,c("symbol","Sector")],stkdata, fit$cluster)


write.csv(stkdata, "StockCluster.csv",row.names=TRUE) 

#--------------------Hierarchical Clustering & Adding Sector information--------------------------------

d.stk = daisy(subset(stkval,select = -c(symbol)))
ag.stk = agnes(d.stk)
plot(ag.stk)
pltree(ag.stk)
ag.stk$ac
ck <- cutree(ag.stk, k)
stkval <- cbind(stkval,cluster=ck)
hist(stkval$cluster,breaks=100,labels = TRUE)
write.csv(stkval, "StockCluster-agnes.csv",row.names=TRUE) 
# ag.stk$order

(d2 <- as.dendrogram(ag.stk)) # two main branches
d2[[1]] # the first branch
d2[[2]] # the 2nd one  
d2[[c(1,1)]]
str(d2)  # ## a "textual picture" of the dendrogram :

#----------------eg only which variable imapacts the most is a cluster ---------------
factanal(subset(stkval,cluster==10,select = -c(symbol,Sector,cluster)),3)



# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit1$cluster, fit2$cluster)

