pData = read.csv("PilgrimCaseData.csv")

modelOnline9 <- lm(pData$X9Profit~factor(pData$X9Online))
summary(modelOnline9)

plot(modelOnline9,main = "Linear Regression between profitabiliy and Online of 1999")

model9 <- lm(X9Profit~factor(X9Online)+X9Age+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9)

plot(model9, main = "Linear Regression of 1999")
