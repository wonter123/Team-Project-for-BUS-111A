
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

# Fit lm model on train: model
modeltest <- lm(train$X9Profit ~ factor(train$X9Billpay) + train$X9Inc)

# Predict on test: p
p <- predict(modeltest, test)

# Compute errors: error
error <- p - test$X9Profit

#Omit NA
error <-error <- error[!is.na(error)]

# Calculate RMSE
sqrt(mean(error^2))

#find the best model and use 1999 model complete data to predict the complete data of 2000
#summarize prediction, predict 9, actual 10, average percent dollar
#Use new data to run regression