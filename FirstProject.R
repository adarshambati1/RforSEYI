rSequence <- seq(from = -40, to = 240, by = 4)
excelSequence <- scan(file = "Vector.csv", sep = ",")
vectorDifference <- rSequence - excelSequence
range(vectorDifference)
fahrenheit <-rSequence
rm(rSequence, excelSequence)
celsius <-((fahrenheit-32))*5/9
celsius[which(fahrenheit == 32)]
#32 degrees fahrenheit is 0 degrees Celsius so the above value is the correct Celsius temperature
celsius[which(fahrenheit == 212)]
#212 degrees fahrenheit is 100 degrees Celsius so the above value is the correct Celsius temperature
rMatrix<-matrix(c(5,7,9,7,6,2,3,6,2,6,7,4,3,4,7,8,11,7,8,2,5,7.8,1.3,6.0,2.5,6.8,7.1,3.4),nrow=7)
colnames(rMatrix) <-c("beetles","ants","wasps","pH")
rownames(rMatrix) <-c("B","C","D","E","F","G","H")
excelDataFrame <- read.csv(file = "Matrix.csv", header =TRUE)
rownames(excelDataFrame) <-c("B","C","D","E","F","G","H")
class(rMatrix)
class(excelDataFrame)
difference <-excelDataFrame-rMatrix
rMatrix.df <- as.data.frame(rMatrix)
difference <- excelDataFrame - rMatrix.df
range(difference)
antsWasps1 <- excelDataFrame[2]+excelDataFrame[3]
antsWasps2 <- excelDataFrame$ants+excelDataFrame$wasps
meanpH<-mean(excelDataFrame$pH)
ants.G1<-excelDataFrame[6,2]
ants.G2<-excelDataFrame$ants[6]

