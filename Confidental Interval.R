pcData = read.csv("PilgrimCaseData.csv")

listProfit = pcData$X9Profit
mean(listProfit)

#Create a new matrix with age, income and profit
listAge_Profit <- matrix(c(pcData$X9Age,pcData$X9Inc,pcData$X9Profit), ncol = 3)

#Create another matrix without the missing data(eliminate those rows which have missing data)
listAge_Profit_NoNA = listAge_Profit[!is.na(listAge_Profit[,1]),]
listAge_Profit_NoNA = listAge_Profit_NoNA[!is.na(listAge_Profit_NoNA[,2]),]
listAge_Profit_NoNA = listAge_Profit_NoNA[!is.na(listAge_Profit_NoNA[,1]),]

View(listAge_Profit_NoNA)
length(listAge_Profit_NoNA)
mean_WNA = mean(listAge_Profit[,3])

mean_NoNA = mean(listAge_Profit_NoNA[,3])
sd_NONA = sd(listAge_Profit_NoNA[,3])
size_NoNA = length(listAge_Profit_NoNA) / 3
var_NoNA = var(listAge_Profit_NoNA[,3])

#t test whether missing data effect the result
t.test(listAge_Profit[,3],listAge_Profit_NoNA[,3],paired = FALSE)

#t test whether handling missing data with mode affect the result
t.test(listAge_Profit[,3],pData$X9Profit,paired = FALSE)

#Before
median(listAge_Profit_NoNA[,3])
#After
median(pData$X9Profit)