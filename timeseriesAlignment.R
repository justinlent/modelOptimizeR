alignAndCreateDF<-function(allSeriesList){    # must pass in timeseries as a list. if passing in individuall wrap in list()
  alignedIndexes<-getAlignedTimeseriesIndexes(allSeriesList)
  #alignedDF<-data.frame(allSeriesList[[1]])
  alignedDF<-data.frame( allSeriesList[[1]][alignedIndexes] )
  for( i in 2:length(allSeriesList) ){
    tempDF<-data.frame(allSeriesList[[i]][alignedIndexes])
    alignedDF<-cbind(alignedDF,tempDF)  
  }
  return( as.xts(as.zoo(alignedDF)) )   # hack to make sure intraday/timezone's are coerced by R when transformations are being done
}

getAlignedTimeseriesIndexes<-function(allSeriesList) {   # must pass in timeseries as a list. if passing in individuall wrap in list()
  firstTS<-as.xts(as.zoo(allSeriesList[[1]]))   # hack to make sure intraday/timezone's are coerced by R when transformations are being done
  alignedIndexes<- index(firstTS) %in% index(firstTS)
  tempAlignedTS<-firstTS[alignedIndexes]
  
  for( i in 2:length(allSeriesList) ){
    currentTS<-as.xts(as.zoo(allSeriesList[[i]]))   # hack to make sure intraday/timezone's are coerced by R when transformations are being done
    
    matchingDays<-index(currentTS) %in% index(tempAlignedTS)
    
    alignedIndexes<-index(currentTS[matchingDays])
    tempAlignedTS<-tempAlignedTS[alignedIndexes]
  }
  return(alignedIndexes)  # returns the set of dates that are in all of the timeseries contained in the paramater 'allSeriesList'
}

