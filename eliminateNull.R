eliminateNull <- function(x) { 
  meanX = mean(x[!is.na(x)])
  for (i in 1:length(x)){
    if (is.na(x[i])) {
      #print(i)
      x[i] = meanX
    }
  }
  return(x)
}

pcData = read.csv("PilgrimCaseData.csv")

pData$X9Profit = eliminateNull(pcData$X9Profit)
pData$X9Age = eliminateNull(pcData$X9Age)
pData$X9Inc = eliminateNull(pcData$X9Inc)
pData$X9Tenure = eliminateNull(pcData$X9Tenure)
pData$X0Profit = eliminateNull(pcData$X0Profit)
