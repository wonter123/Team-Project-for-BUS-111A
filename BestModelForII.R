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

modelDistrict <- lm(X9Profit~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay)+factor(X9District),data = train)
pred1 <- predict(modelDistrict, test)

# Compute errors: error
error1 <- pred1 - test$X9Profit
#Omit NA
#error <-error <- error[!is.na(error)]
# Calculate RMSE
sqrt(mean(error1^2))

modelNoDistrict <- lm(X9Profit~factor(X9Online)+factor(X9Inc)+X9Tenure+factor(X9Age)+factor(X9Billpay),data = train)
pred2 <- predict(modelNoDistrict, test)

# Compute errors: error
error2 <- pred2 - test$X9Profit
#Omit NA
#error <-error <- error[!is.na(error)]
# Calculate RMSE
sqrt(mean(error2^2))


