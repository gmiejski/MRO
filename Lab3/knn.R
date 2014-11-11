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

# splitRate = 4
# dataInEachPart = ceiling(nrow(data) / splitRate)


# for ( x in 0:splitRate-1) {
#   print ( x*dataInEachPart)  
getSuccessRatio <- function (splitRate, knn_k ) {
#   x = 0
  
  print(splitRate-1)
  
  dataInEachPart = ceiling(nrow(data) / splitRate)
  successRates = rep(NA, splitRate)
  for ( x in 1:splitRate-1) {  
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
    print(x)
    print(data.knn.diff.ok)
    print(data.knn.diff.bad)
    print(data.knn.diff.ok / (data.knn.diff.ok+data.knn.diff.bad))
    successRates[x+1] <- data.knn.diff.ok / (data.knn.diff.ok+data.knn.diff.bad)
  }

  sum(successRates) / length(successRates)
  
}

successRatios = getSuccessRatio(4,1)


# data.knn[1:(dataInEachPart-1)]
# train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
# test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
# cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
# knn(train, test, cl, k = 3, prob=TRUE)
# attributes(.Last.value)

# cut data examples not having all the information
# data[!apply(data,1,function(x)any('?' %in% c(x))),]

# trainingSet
