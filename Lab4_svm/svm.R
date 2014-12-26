# install.packages("e1071")
library(class)
library(e1071)
setwd("C:/home/aaaaStudia/Semestr_VII/MRO/Lab4")

data <- read.csv(file="breast-cancer-wisconsin.data",head=TRUE,sep=",")
data <- data[!apply(data,1,function(x)any('?' %in% c(x))),]
dataSize = nrow(data)
data <- data[sample(dataSize),]
data.orig <- data

data <- data.orig
data <- scale(data[,c(2:6, 8:10)])

ncol(data)
colnames(data) <- c("clump thickness","cell size","cell shape",
                    "marginal adhesion","Single Epithelial Cell Size",
                    "Bland Chromatin","Normal Nucleoli","Mitoses") #,"class")
colnames(data)

getSuccessRatio <- function (splitRate) {
  dataInEachPart = ceiling(nrow(data) / splitRate)
  successRates = rep(NA, splitRate) 
  for ( x in 1:splitRate-1) {  
    testSetIndexes = c((x*dataInEachPart):((x+1)*(dataInEachPart -1)))
    trainingSet = data[-c(testSetIndexes),]
    clasifier = factor(c(data.orig[-c(testSetIndexes),11]))
    testSet = data[c(testSetIndexes),]
    
    svm.model <- svm(trainingSet, y = clasifier, kernel = "polynomial")
    svm.pred <- predict(svm.model, testSet)
    
#     plot(cmdscale(dist(iris[,-5])),
#          col = as.integer(iris[,5]),
#          pch = c("o","+")[1:150 %in% model$index + 1])
    
    svm.diff = data.orig[c(testSetIndexes),11]-(as.integer(svm.pred[1:(dataInEachPart-1)])*2)
    svm.diff.ok = length(which(svm.diff == 0))
    svm.diff.ok = length(svm.diff[svm.diff == 0])
    svm.diff.bad = length(which(svm.diff != 0))
    print(svm.diff.ok)
    print(svm.diff.bad)
    print(svm.diff.ok / (svm.diff.ok+svm.diff.bad))
    successRates[x+1] <- svm.diff.ok / (svm.diff.ok+svm.diff.bad)
  }
  mean(successRates)
}

testSetProportions <- seq(4, 14, 1)
meanRatio <- rep(NA, length(testSetProportions))

for ( i in testSetProportions) {
  successRatios = getSuccessRatio(i)
  meanRatio[i+1] = successRatios  
}

meanRatio.all <- meanRatio[5:length(meanRatio)]

plot(testSetProportions, meanRatio.all)
mean(meanRatio.all)
