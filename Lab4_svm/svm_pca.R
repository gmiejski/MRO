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
  print(splitRate-1)
  #   splitRate = 4
  #   x = 0
  #   x = 1
  dataInEachPart = ceiling(nrow(data) / splitRate)
  testSetIndexes = c((x*dataInEachPart):((x+1)*(dataInEachPart -1)))
  trainingSet = data[-c(testSetIndexes),]
  clasifier = factor(c(data.orig[-c(testSetIndexes),11]))
  testSet = data[c(testSetIndexes),]
  
  svm.model <- svm(trainingSet, y = clasifier, kernel = "sigmoid")
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
  bad <- rep(NA, svm.diff.bad)
  c = 1
  for (i in 1:length(svm.diff) ) {
    if (svm.diff[i] != 0 ) {
      bad[c] = i
      c = c + 1
    }
  }
  
  bad
  
}


splitRate = 4
badlyQualified <- getSuccessRatio(4)
pca <- princomp(data, cor = TRUE)
plot(pca$scores[,1], pca$scores[,2],col=c("black", "red","black","black")[data.orig[, 11]] , pch = 19, xlab="PCA score #1", ylab = "PCA score #2")
points(pca$scores[badlyQualified,1], pca$scores[badlyQualified,2], col="yellow",pch = 19)