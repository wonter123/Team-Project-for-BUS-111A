
pData$X9Type = "0"
pData[pData$X9Online==0 & pData$X9Billpay == 0,]$X9Type = "1" 
pData[pData$X9Online==1 & pData$X9Billpay == 0,]$X9Type = "2" 
pData[pData$X9Online==1 & pData$X9Billpay == 1,]$X9Type = "3" 

pData$X0Type = "0"
pData[!is.na(pData$X0Online) & !is.na(pData$X0Billpay) & pData$X0Online==0 & pData$X0Billpay == 0,]$X0Type = "1" 
pData[!is.na(pData$X0Online) & !is.na(pData$X0Billpay) & pData$X0Online==1 & pData$X0Billpay == 0,]$X0Type = "2" 
pData[!is.na(pData$X0Online) & !is.na(pData$X0Billpay) & pData$X0Online==1 & pData$X0Billpay == 1,]$X0Type = "3" 
pData[is.na(pData$X0Online) & is.na(pData$X0Billpay),]$X0Type = "4" 

##pData[pData$X0Type == 0,]$ID

table<-table(pData$X9Type,pData$X0Type)
margin.x = table[,1]+table[,2]+table[,3]+table[,4]+table[,5]
table = cbind(table,margin.x)
table2 = table
table2[,1] = table[,1]/table[,6]
table2[,2] = table[,2]/table[,6]
table2[,3] = table[,3]/table[,6]
table2[,4] = table[,4]/table[,6]
table2[,5] = table[,5]/table[,6]
table2[,6] = table[,6]/table[,6]


round(table2,2)

