# Set seed
set.seed(42)
# Shuffle row indices: rows
rows <- sample(nrow(pData))
# Randomly order data
pDataRandom <- pData[rows, ]
# Determine row to split on: split
split <- round(nrow(pDataRandom) * .80)
# Create train
train <- pDataRandom[1:split, ]
# Create test
test <- pDataRandom[(split + 1):nrow(pDataRandom), ]

modelDistrict <- lm(Retention~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay)+factor(X9District),data = train)
pred1 <- predict(modelDistrict, test)
error1 <- pred1 - test$X9Profit
sqrt(mean(error1^2))

modelDistrict <- lm(Retention~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay)+factor(X9District),data = pData)
summary(modelDistrict)$adj.r.squared
AIC(modelDistrict)
BIC(modelDistrict)




modelNoDistrict <- lm(Retention~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay),data = train)
pred2 <- predict(modelNoDistrict, test)
error2 <- pred2 - test$X9Profit
sqrt(mean(error2^2))

modelNoDistrict <- lm(Retention~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay),data = pData)
summary(modelNoDistrict)$adj.r.squared
AIC(modelNoDistrict)
BIC(modelNoDistrict)




modelInteraction <- lm(Retention~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Age)*factor(X9Inc)),data = train)
pred3 <- predict(modelInteraction, test)
error3 <- pred3 - test$X9Profit
sqrt(mean(error3^2))

modelInteraction <- lm(Retention~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Age)*factor(X9Inc)),data = pData)
summary(modelInteraction)$adj.r.squared
AIC(modelInteraction)
BIC(modelInteraction)

#Predict the 2000
modelBest <- lm(Retention~factor(X9Online)+factor(X9Age)+factor(X9Inc)+X9Tenure+factor(X9District)+factor(X9Billpay)+(factor(X9Age)*factor(X9Inc)),data = pData)

p <-predict(modelBest,pData)
summary(p)
mean(pData$Retention)

t.test(p,pData$Retention,paired=FALSE)


