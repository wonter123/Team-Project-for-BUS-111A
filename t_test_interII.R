pData = read.csv("PilgrimCaseData.csv")

pData_withOnline9 = pData[pData$X9Online==1,]
pData_withoutOnline9 = pData[pData$X9Online==0,]
t.test(pData_withOnline9$X9Profit,pData_withoutOnline9$X9Profit,paired = FALSE)
pData_withOnline0 = pData[pData$X0Online==1,]
pData_withoutOnline0 = pData[pData$X0Online==0,]
t.test(pData_withOnline0$X0Profit,pData_withoutOnline0$X0Profit,paired = FALSE)

