maCrossoverDays<-function(paramList, theStockString, lagDays=1) {
  # this signal assumes stock data was read in using getSymbols() pulling from Yahoo Finance
  # assumes Adj. Close prices are in column 6
  theStock<-get(theStockString)
  theStockAdjClosingPrices<-theStock[,6]  # adj closing prices are stored in column 6 of Yahoo Finance data
  shortermMA<-as.numeric(paramList[1])
  longtermMA<-as.numeric(paramList[2])
  theStock$shorttermMA<-lag(rollmean(theStockAdjClosingPrices,shortermMA,align="right"), lagDays)
  theStock$longtermMA<-lag(rollmean(theStockAdjClosingPrices,longtermMA,align="right"), lagDays)
  theStock$shortAboveLongMA<-theStock$shorttermMA > theStock$longtermMA
  
  return( theStock[theStock$shortAboveLongMA==1] )
}

maCrossoverStrategySim<-function(paramList, theStockString, lagDays=1) {
  # this signal assumes stock data was read in using getSymbols() pulling from Yahoo Finance
  # assumes Adj. Close prices are in column 6
  theStock<-get(theStockString)
  theStockAdjClosingPrices<-theStock[,6]  # adj closing prices are stored in column 6 of Yahoo Finance data
  daysWhenTrueAdjCloses<-maCrossoverDays(paramList, theStockString, lagDays=lagDays)[,6]
  print(paramList)
#  stratSim<-makeIndex(daysInTradeTS=daysWhenTrueAdjCloses, startValue=100, inputIsPercentReturns=FALSE, referenceTS=theStockAdjClosingPrices)
#  stratSim<-makeIndex(daysInTradeTS=daysWhenTrueAdjCloses, startValue=100, referenceTS=theStockAdjClosingPrices)
  returnsTS<-dailyReturn(theStock)[match(index(daysWhenTrueAdjCloses), index(theStock))] 
  stratSim<-makeIndexFromPctRetFAST(returnsTS=returnsTS, startValue=100)
  stratLabel<-makeLabelFromParamList(paramList)
  names(stratSim)<-stratLabel
  #names(stratSim)<-paste(paramList[1],paramList[2],sep="_")
  return( stratSim )
}
