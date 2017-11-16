pData = read.csv("PilgrimCaseData.csv")

modelOnline9 <- lm(pData$X9Profit~factor(pData$X9Online))
summary(modelOnline9)
## R-Squared = 4.97e-05
## Adjusted R-squared = 1.809e-05 = 0.00001809
BIC(modelOnline9)
## BIC = 444664.7
AIC(modelOnline9)
## AIC = 444639.7


model2 <- lm(X9Profit~factor(X9Online)+factor(X9Age),data = pData)
summary(model2)
## R-squared = 0.02407
## Adjusted R-squared = 0.02385
BIC(model2)
## BIC = 443957.9
AIC(model2)
## AIC = 443882.6

model3 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc),data = pData)
summary(model3)
## R-squared = 0.04776
## Adjusted R-squared = 0.04731
BIC(model3)
## BIC = 443263.3
AIC(model3)
## AIC = 443121.2

model4 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure,data = pData)
summary(model4)
## R-squared = 0.06622
## Adjusted R-squared = 0.06575
BIC(model4)
## BIC = 442654.4
AIC(model4)
## AIC = 442503.9

model5 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District),data = pData)
summary(model5)
## R-squared = 0.06682
## Adjusted R-squared = 0.06629
BIC(model5)
## BIC = 442654.7
AIC(model5)
## AIC = 442487.5

## Interaction between 9Age and 9Income --> to show that perhaps nonlinear relationship is better fit of data
model6 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Age)*factor(X9Inc)),data = pData)
summary(model6)
## R-squared = 0.07126
## Adjusted R-squared = 0.06929
BIC(model6)
## BIC = 443011.7
AIC(model6)
## AIC = 442434.7

## Interaction between 9Inc and 9District --> to show that perhaps nonlinear relationship is better fit of data
model7 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Inc)*X9District),data = pData)
summary(model7)
## R-squared = 0.0683
## Adjusted R-squared = 0.0675
BIC(model7)
## BIC = 442697
AIC(model7)
## AIC = 442455.3

## log-linear model --> to show that perhaps nonlinear relationship is better fit of data
model8 <- lm(log(X9Profit)~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model8)
## R-squared = 0.05616
## Adjusted R-squared = 0.0558
BIC(model8)
## BIC = 442952
AIC(model8)
## AIC = 442835

## polynomial (quadratic) --> to show that perhaps nonlinear relationship is better fit of data
X9Age_2 <- pData$X9Age*pData$X9Age
model9_2 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Age_2)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9_2)
## R-squared = 0.06798
## Adjusted R-squared = 0.06742
BIC(model9_2)
## BIC = 442625.8
AIC(model9_2)
## AIC = 442450.2


model9 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9)
## R-squared = 0.06798
## Adjusted R-squared = 0.06742
BIC(model9)
## BIC = 442625.8
AIC(model9)
## AIC = 442450.2





