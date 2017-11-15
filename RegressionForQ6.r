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
## Adjusted R-squared = 0.02464
BIC(model2)
## BIC = 329074.9
AIC(model2)
## AIC = 329002.4

model3 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc,data = pData)
summary(model3)
## Adjusted R-squared = 0.04603
BIC(model3)
## BIC = 321298.7
AIC(model3)
## AIC = 321218.3

model4 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure,data = pData)
summary(model4)
## Adjusted R-squared = 0.05838
BIC(model4)
## BIC = 321010.4
AIC(model4)
## AIC = 320922

model5 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure+factor(X9District),data = pData)
summary(model5)
##Adjusted R-squared = 0.05893
BIC(model5)
## BIC = 321015.1
AIC(model5)
## AIC = 320910.6

## Interaction between 9Age and 9Income --> to show that perhaps nonlinear relationship is better fit of data
model6 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Age)*X9Inc),data = pData)
summary(model6)
## Adjusted R-squared = 0.06343
BIC(model6)
## BIC = 320969.1
AIC(model6)
## AIC = 320808.4

## Interaction between 9Inc and 9District --> to show that perhaps nonlinear relationship is better fit of data
model7 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay)+(X9Inc*X9District),data = pData)
summary(model7)
## Adjusted R-squared = 0.06031
BIC(model7)
## BIC = 320999.8
AIC(model7)
## AIC = 320879.2

## linear-log model --> to show that perhaps nonlinear relationship is better fit of data
model8 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+log(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model8)
## Adjusted R-squared = 0.05383
BIC(model8)
## BIC = 321147.5
AIC(model8)
## AIC = 321035

## quadratic --> to show that perhaps nonlinear relationship is better fit of data
X9Age_2 <- pData$X9Age*pData$X9Age
model9_2 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+factor(X9Age_2)+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9_2)
## Adjusted R-squared = 0.0602
BIC(model9_2)
## BIC = 320993.4
AIC(model9_2)
## AIC = 320881


model9 <- lm(X9Profit~factor(X9Online)+factor(X9Age)+X9Inc+X9Tenure+factor(X9District)+factor(X9Billpay),data = pData)
summary(model9)
## Adjusted R-squared = 0.0602
BIC(model9)
## BIC = 320993.4
AIC(model9)
## AIC = 320881





