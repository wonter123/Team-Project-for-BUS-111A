modelOnline9 <- lm(pData$X9Profit~factor(pData$X9Online))
summary(modelOnline9)


modelRetention <- lm(pData$Retention~pData$X9Online)
summary(modelRetention)

modelRetention2 <- lm(pData$Retention~pData$X9Online+pData$X9Billpay)
summary(modelRetention2)

