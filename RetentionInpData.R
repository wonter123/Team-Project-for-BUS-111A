getRetention <- function(x,y) {
  z = c(1:length(x))
  for (i in 1: length(x)) {
    if (!is.na(y[i])) {
      z[i] = 1
    } else {
      z[i] = 0
    }
  }
  return (z)
}

pData$Retention = getRetention(pcData$X9Online,pcData$X0Online)