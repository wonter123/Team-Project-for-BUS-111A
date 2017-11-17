modelOnline9 <- lm(pData$X9Profit~factor(pData$X9Online))
summary(modelOnline9)


retention <- pData$X9Profit*pData$Retention
modelRetention <- lm(retention~factor(pData$X9Online))
summary(modelRetention)

