pcData = read.csv("PilgrimCaseData.csv")
####pcData[!complete.cases(pcData),]
#####listAge = pcData$X9Age
#####listAge <- listAge[!is.na(listAge)]viewmean
listProfit = pcData$X9Profit
mean(listProfit)
listAge_Profit <- matrix(c(pcData$X9Age,pcData$X9Inc,pcData$X9Profit), ncol = 3)
listAge_Profit_NoNA = listAge_Profit[!is.na(listAge_Profit[,1]),]
listAge_Profit_NoNA = listAge_Profit_NoNA[!is.na(listAge_Profit_NoNA[,2]),]
listAge_Profit_NoNA = listAge_Profit_NoNA[!is.na(listAge_Profit_NoNA[,1]),]
###temp = listAge_Profit_NoNA[,2]
View(listAge_Profit_NoNA)
length(listAge_Profit_NoNA)
mean_WNA = mean(listAge_Profit[,3])

mean_NoNA = mean(listAge_Profit_NoNA[,3])
sd_NONA = sd(listAge_Profit_NoNA[,3])
size_NoNA = length(listAge_Profit_NoNA) / 3
var_NoNA = var(listAge_Profit_NoNA[,3])
####error_NoNA <- qt(0.975,df=size_NoNA-1)*sd_NONA/sqrt(size_NoNA)

#### t = (mean_NoNA - mean_WNA) / (sd_NONA/sqrt(size_NoNA))
t.test(listAge_Profit[,3],listAge_Profit_NoNA[,3],paired = FALSE)