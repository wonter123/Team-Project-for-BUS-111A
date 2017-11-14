pData = read.csv("PilgrimCaseData.csv")

model9 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9)

plot(model9, main = "Linear Regression of 1999")