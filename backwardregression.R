library(ppcor)

data<-myData1
#dependent variable
var1<-"BODYFAT"
#threshold value
alpha<-0.01

#list of all variables in the dataset
list<-c("AGE","WEIGHT","HEIGHT","NECK","CHEST","ABDOMEN","HIP","THIGH","KNEE","ANKLE","BICEPS","FOREARM","WRIST")

#function which will return the important features in yoru model
backwardstepwise<- function(var1, list, data, alpha) {
  data <- data[,c(list,var1)]
  mySubModel <- lm(as.formula(paste(var1, "~", paste(list, collapse = "+"), sep = "")), data)
  independentColumn <- data[,var1]
  subdata1 <- data[,list]
  
  minCorColumnNames <- c()
  partialCorr <- c()
  pvalues <- c()
  while(T) {
    partialCorrelations <- c()
    for(i in c(1:length(subdata1))){
      colCorrelation <- pcor.test(independentColumn, subdata1[,i], subdata1[,-i])
      partialCorrelations <- c(partialCorrelations, colCorrelation$estimate)
      #print(partialCorrelations)
    }

    minEstimateIndex <- which.min(abs(partialCorrelations))
    tempPartialCorr <-c(partialCorr, min(abs(partialCorrelations)))
    tempMinCorColumnNames <- c(minCorColumnNames, colnames(subdata1)[which(abs(partialCorrelations) == min(abs(partialCorrelations)))])
    remainingCols <- subdata1[,-minEstimateIndex]
    mySuperModel <- lm(as.formula(paste(var1, "~", paste(names(remainingCols), collapse = "+"), sep = "")), data)
    anovaTable <- anova(mySubModel,mySuperModel)$"Pr(>F)"
    valuep <- anovaTable[2]
  
    if(valuep > alpha) {
      minCorColumnNames <- tempMinCorColumnNames
      pvalues <- c(pvalues, valuep)
      partialCorr <- tempPartialCorr
    
      subdata2 <- subdata1[,-minEstimateIndex]
      subdata1 <- subdata2
    }
    else {
      break
    }
    
    ifelse(valuep > alpha, mySubModel <- mySuperModel,mySubModel <- mySubModel)    
  }
  
  answer <- c(minCorColumnNames,pvalues,partialCorr)
  return(answer)
}

#function call
backwardstepwise("BODYFAT", list, myData1, 0.01)