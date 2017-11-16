eliminateNull <- function(x) { 
  meanX = mean(x,na.rm = TRUE)
  for (i in 1:length(x)){
    if (is.na(x[i])) {
      #print(i)
      x[i] = meanX
    }
  }
  return(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

eliminateNullFactor <- function(x) { 
  modeX = getmode(x[!is.na(x)])
  print(modeX)
  for (i in 1:length(x)){
    if (is.na(x[i])) {
      #print(i)
      x[i] = modeX
    }
  }
  return(x)
}


pData$X9Profit = eliminateNull(pcData$X9Profit)
pData$X9Age = eliminateNullFactor(pcData$X9Age)
pData$X9Inc = eliminateNullFactor(pcData$X9Inc)
pData$X9Tenure = eliminateNull(pcData$X9Tenure)
pData$X0Profit = eliminateNull(pcData$X0Profit)
pData$X9District = eliminateNullFactor(pcData$X9District)
