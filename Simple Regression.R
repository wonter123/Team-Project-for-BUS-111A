#set the working directory
train <- read.csv("PilgrimCaseData.csv")

# dimension of the data
dim(train)
colnames(train)
hist(train[, 7])


#randomly devide the dataset into two parts
number<- c(sample(31634, 15817))
num<- sort(number) #Sort the variables to make it easier to compare the pridiction with the real graph
train<- train[num,] #use the training set to form a linear relationship
remain<- train[-num,] #use the remaining set to test the linear formular
# No test in the current version

# observe what data lookes like
plot(train[, 7],type='p',ylab='profit', xlab='number', col='black', main='Profit Relation')
pairs(train[1:3000,1:7])

model <- lm(formula = train$X9Profit ~ train$X9Online + train$X9Age + train$X9Inc + train$X9Tenure + train$X9District +I(train$X9Online^2) + I(train$X9Age^2) + I(train$X9Inc^2) + I(train$X9Tenure^2) + I(train$X9District^2),data = train)
plot(model, main = "Model", which = c(1, 2))
summary(model)




