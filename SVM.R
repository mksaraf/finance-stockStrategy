# Finance Strategy: RSI -  look for RSI values over 70 to represent overbought market conditions and under 30 to represent oversold market conditions
# ML Algo: SVM with Radial Kernal
# The gamma determines how much effect a single training example can have on the decision boundary: Low is high Impact=~ 1/# of inputs
# REgularization: C determines the trade off between misclassifying examples in your training set and the simplicity of the decision boundary. A low C creates a smoother decision boundary and decreases overfitting, 
# while a high C will attempt to classify every data point in the training set correctly and may lead to overfitting ~1 (less overfit)


library("quantmod")
library("e1071") #Gives us access to the SVM
library("ggplot2")
 
asset = c('^gspc')
endDate = Sys.Date()-1 
startDate = endDate-365 #year's worth
lagDays = 14
fwdDays = 5

prices =  OHLCV(getSymbols(asset,from = startDate, to = endDate,env=NULL))
RSI = RSI(Cl(prices)) # 14 period RSI 
SMA50 = SMA(Cl(prices),50)
Trend <- Cl(prices)-SMA50


#Find the ratio change between Next day close price and today close price
ratio <- function(x) (tail(x,1)/as.numeric(head(x,1)))   # print(head(x,1))
PriceChange<- rollapply(Cl(prices),width=2,align = "left",FUN = ratio) # align = "left" is fwd looking 
dset<-data.frame("Move"=ifelse(PriceChange>1,"UP","DOWN"),Trend,RSI)
colnames(dset)<-c("Move","Trend","RSI")
dset = dset[complete.cases(dset),]

TrainingSet<-dset[1:((2/3)*nrow(dset)),] #We will use ⅔ of the data to train the model
TestSet<-dset[((2/3)*nrow(dset)):nrow(dset),]  #And ⅓ to test it on unseen data 


SVM<-svm(Move~RSI+Trend,data=TrainingSet, kernel="radial",cost=0.5,gamma=1/2)
#Build our support vector machine using a radial basis function as our kernel,
# the cost, or C, at 1, and the gamma function at ½, or 1 over the number of inputs we are using

TrainingPred<-data.frame(TrainingSet,"Pred"=predict(SVM,TrainingSet,type="class"))
#Create a data set with the predictions

TestResults = data.frame("X"=ifelse(predict(SVM,TestSet)==TestSet[,1],"Correct","Incorrect"),"Pred"=predict(SVM,TestSet),TestSet)

tab=table(predict(SVM,TestSet),TestSet[,1],dnn=list('predicted','actual')) 
print(tab) ; (paste("SVM Performance: % Correct:",round(100*sum(diag(tab))/sum(tab))))


#  Rules Extraction (Manual)------> we are not putting SVM model directly to test data
ggplot(TrainingPred,aes(x=Trend,y=RSI))+stat_density2d(geom="contour",aes(color=Pred))+labs(title="SVM RSI and Trend Predictions",x="Close - SMA50",y="RSI",color="Pred")

shorts<-which(TestSet$RSI > 45 & TestSet$Trend > 0)

tab=table(ifelse(1:nrow(TestSet) %in% shorts,"DOWN","UP"),TestSet[,1],dnn=list('predicted','actual')) 
print(tab) ; (paste("SVM Performance - RULES EXT.: % Correct:",round(100*sum(diag(tab))/sum(tab))))


