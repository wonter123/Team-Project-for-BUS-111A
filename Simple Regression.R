#set the working directory
train <- read.csv("PilgrimCaseData.csv")

# dimension of the data
dim(train)
colnames(train)
hist(train[, 6])


#randomly devide the dataset into two parts
number<- c(sample(31634, 15817))
num<- sort(number) #Sort the variables to make it easier to compare the pridiction with the real graph
train<- train[num,] #use the training set to form a linear relationship
remain<- train[-num,] #use the remian set to test the linear formular

# observe what data lookes like
plot(train[, 6],type='p',ylab='profit', xlab='number', col='black', main='Profit Relation')
pairs(train[1:3000,1:6])

model <- lm(formula = train[,2] ~ train[, 3] + train[, 4] + train[, 5] + train[, 6] + I(train[, 2]^2) + I(train[, 3]^2) + I(train[, 4]^2) + I(train[, 5]^2) + I(train[, 6]^2),data = train)
plot(model, main = "Model", which = c(1, 2))
summary(model)




