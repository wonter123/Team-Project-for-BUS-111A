library(readr)
pilgrim <- read_csv("~/Desktop/pilgrim.csv")
View(pilgrim)


hist(pilgrim$`9Profit`, n=50, xlab="Profit", ylab="Frequency", main="Profitability", xlim=c(0,1500))


plot(density(pilgrim$`9Profit`), main = "Profitability", xlab = "Profit", ylab = "Frequency")

boxplot(pilgrim$`9Profit`~ pilgrim$`9Online`, xlab = "Online", ylab = "Profit")

pilgrim <- matrix(c("9Profit", "9Online", "9Age", "9Inc", "9Tenure", "9District", "21", "0", "not available", "6.33", "1200", "-6", "0", "6", "3", "29.50", "1200", "-49", "1", "5", "5", "26.41", "1100", "-4", "0", "not available", "not available", "2.25", "1300", "...", "...", "...", "...", "...", "...", "92", "1", "1", "6", "5.41", "1200", "124", "0", "3", "6", "17.50", "1300", "111.50", "0.12", "4.05", "5.46", "10.16", "n/a", "272.84", "0.33", "1.64", "2.35", "8.45", "n/a"), ncol = 6, byrow = TRUE)
colnames(pilgrim) <- c("1999 Annual Profit", "1999 Online Usage", "1999 Age Bucket (1-7)", "1999 Income Bucket (1-9)", "1999 Tenure Years", "1999 Geographic Region (1100, 1200, or 1300)")
rownames(pilgrim) <- c(" ", "1", "2", "3", "4", "...", "31,633", "31,634", "Mean", "Standard Deviation")
pilgrim <- as.table(pilgrim)
pilgrim

