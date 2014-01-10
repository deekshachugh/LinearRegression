data<-myData1
var1<-"BODYFAT"
alpha<-0.01
list<-c("BODYFAT","AGE","WEIGHT","HEIGHT","NECK","CHEST","ABDOMEN","HIP","THIGH","KNEE","ANKLE","BICEPS","FOREARM","WRIST")
forwardstepwise<- function(var1, list, data, alpha) {
  data <- data[,list]
  mySubModel <- lm(as.formula(paste(var1, "~", 1, sep = "")),data = data)
  independentColumn <- data[,var1]
  subdata1 <- data[,-match(var1, list)]
  correlation <- cor(independentColumn , subdata1)
  partialCorr <-c(max(abs(correlation)))
  maxCorColumnNames <- c(colnames(correlation)[which(abs(correlation) == max(abs(correlation)))])
  mySuperModel <- lm(as.formula(paste(var1, "~", paste(maxCorColumnNames, collapse = "+"), sep = "")), data)
  anovaTable <- anova(mySubModel,mySuperModel)$"Pr(>F)"
  valuep <- anovaTable[2]
  pvalues <- c(valuep)
  
  while(T) {
    maxCorColumns <- data[,maxCorColumnNames]
    subdata2 <- subdata1[,-match(maxCorColumnNames, names(subdata1))]
    partialCors <- apply(subdata2, 2, function(x){pcor.test(independentColumn, x, maxCorColumns)})
    maxEstimateIndex <- which.max(abs(do.call("rbind", partialCors)$estimate))
    maxEstimate <- max(abs(do.call("rbind", partialCors)$estimate))  
    maxEstimateColName <- names(partialCors[maxEstimateIndex])      
    tempMaxCorColumnNames <- c(maxCorColumnNames, maxEstimateColName)
    mySuperModel <- lm(as.formula(paste(var1, "~", paste(tempMaxCorColumnNames, collapse = "+"), sep = "")), data)
    anovaTable <- anova(mySubModel, mySuperModel)$"Pr(>F)"
    valuep <- anovaTable[2]
    if(valuep < alpha){
      maxCorColumnNames <- tempMaxCorColumnNames
      pvalues <- c(pvalues,valuep)
      partialCorr<-c(partialCorr,maxEstimate)
    }
    else {
      break
    }
    
    ifelse(valuep < alpha, mySubModel <- mySuperModel,mySubModel <- mySubModel)    
  }
  answer <-c(maxCorColumnNames,pvalues,partialCorr)
  return(answer)
}
forwardstepwise("BODYFAT", list, myData1, 0.01)