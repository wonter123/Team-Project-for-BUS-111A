modelOnline9 <- lm(pData$X9Profit~factor(pData$X9Online))
summary(modelOnline9)


modelRetention <- lm(pData$Retention~pData$X9Online)
summary(modelRetention)

modelRetention2 <- lm(pData$Retention~pData$X9Online+pData$X9Billpay)
summary(modelRetention2)

modelRetention3 <- lm(pData$X9Profit~pData$X9Online+pData$X9Billpay)
summary(modelRetention3)

modelRetention4 <- lm(pData$X0Profit~pData$X0Online+pData$X0Billpay)
summary(modelRetention4)

