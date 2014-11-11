library(class)

data <- read.csv(file="breast-cancer-wisconsin.data",head=FALSE,sep=",")
data <- data[!apply(data,1,function(x)any('?' %in% c(x))),]
names(data)
colnames(data) <- c("id","clump thickness","cell size","cell shape","marginal adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitoses","class")

splitRate = 4
dataInEachPart = as.integer(nrow(data) / splitRate)


# for ( x in 0:splitRate-1) {
#   print ( x*dataInEachPart)  
  x = 0
  testSetIndexes = c((x*dataInEachPart):((x+1)*(dataInEachPart -1)))
  trainingSet = data[-c(testSetIndexes),]
  clasifier = factor(c(trainingSet[,11]))
  trainingSet = trainingSet[,1:10]
  
  testSet = data[c(testSetIndexes),]
  
  data.knn <- knn(trainingSet, testSet[,1:10], clasifier, k = 1, prob=TRUE)
  data.knn  
  data.knn.diff = testSet[,11]-(as.integer(data.knn[1:(dataInEachPart-1)])*2)
  data.knn.diff.ok = length(which(data.knn.diff == 0))
  data.knn.diff.ok = length(data.knn.diff[data.knn.diff == 0])
  data.knn.diff.bad = length(which(data.knn.diff != 0))
  
# }

data.knn[1:(dataInEachPart-1)]
# train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
# test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
# cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
# knn(train, test, cl, k = 3, prob=TRUE)
attributes(.Last.value)

# cut data examples not having all the information
data[!apply(data,1,function(x)any('?' %in% c(x))),]

trainingSet
