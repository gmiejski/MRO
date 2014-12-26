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

  mean(successRates)
  
}



successRatios = getSuccessRatio(4,1)


ks <- 1:15
testSetProportions <- seq(4, 14, 1)
repetitions = 20

successRates <- outer( testSetProportions, ks, Vectorize(getSuccessRatio))
# successRates <- successRatesSum / repetitions

ncols <- ncol(successRates)
nrows <- nrow(successRates)
jet.colors <- colorRampPalette(c("red", "red", "yellow"))
colorCount <- 100
color <- jet.colors(colorCount)
zfacet <- successRates[-1, -1] + successRates[-1, -ncols] + successRates[-nrows, -1] + successRates[-nrows, -ncols]
facetcol <- cut(zfacet, colorCount)

drawPlot <- function() {
  persp(testSetProportions, ks , successRates, theta = -60, phi = 20, expand = 0.3, col = color[facetcol],
        ticktype = "detailed",
        xlab = "splitRatio", ylab = "k", zlab = "effectiveness [%]")
}
drawPlot()
