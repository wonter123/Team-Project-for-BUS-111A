#Answer for Q7

pData = read.csv("PilgrimCaseData.csv")

library(dplyr)
train<-sample_frac(pData, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-pData[-sid,]

model <- lm(train$X9Profit ~ ., train)

# Predict on test: p
p <- predict(model, test)

error <- p - test$X9Profit

error <- error[!is.na(error)]

# Calculate RMSE
sqrt(mean(error^2)) 

modelTest <- lm(train$X9Profit ~ factor(train$X9Online))

predictTest <- predict(modelTest,test)

error <- predictTest - test$X9Profit

error <- error[!is.na(error)]

sqrt(mean(error^2)) 
