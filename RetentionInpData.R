getRetention <- function(x,y) {
  z = c(1:length(x))
  for (i in 1: length(x)) {
    if (!is.na(x[i]) && !is.na(y[i])) {
      if (x[i] == 1 && y[i] == 1) {
        z[i] = 1
      } else {
        z[i] = 0
      }
    } else {
      z[i] = NA
    }
  }
  return (z)
}

pData$Retention = getRetention(pcData$X9Online,pcData$X0Online)