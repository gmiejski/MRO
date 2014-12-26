library(class)

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

getSuccessRatio <- function (splitRate, knn_k ) {
  x = 0
  dataInEachPart = ceiling(nrow(data) / splitRate)
  successRates = rep(NA, splitRate)
  testSetIndexes = c((x*dataInEachPart):((x+1)*(dataInEachPart -1)))
  trainingSet = data[-c(testSetIndexes),]
  clasifier = factor(c(data.orig[-c(testSetIndexes),11]))
  testSet = data[c(testSetIndexes),]
  
  data.knn <- knn(trainingSet, testSet, clasifier, k = knn_k, prob=TRUE)
  data.knn  
  data.knn.diff = data.orig[c(testSetIndexes),11]-(as.integer(data.knn[1:(dataInEachPart-1)])*2)
  data.knn.diff.ok = length(which(data.knn.diff == 0))
  data.knn.diff.ok = length(data.knn.diff[data.knn.diff == 0])
  data.knn.diff.bad = length(which(data.knn.diff != 0))
  bad <- rep(NA, data.knn.diff.bad)
  c = 1
  for (i in 1:length(data.knn.diff) ) {
    if (data.knn.diff[i] != 0 ) {
      bad[c] = i
      c = c + 1
    }
  }

  bad
  
}



successRatios = getSuccessRatio(4,1)

splitRate = 4
knn_k = 1
badlyQualified <- getSuccessRatio(4, 1)
pca <- princomp(data, cor = TRUE)
plot(pca$scores[,1], pca$scores[,2],col=c("black", "red","black","black")[data.orig[, 11]] , pch = 19, xlab="PCA score #1", ylab = "PCA score #2")
points(pca$scores[badlyQualified,1], pca$scores[badlyQualified,2], col="yellow",pch = 19)
