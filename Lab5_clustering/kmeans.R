# kmeans

setwd("C:/home/aaaaStudia/Semestr_VII/MRO/Lab5")
data <-read.csv("mnist_all.lcsv", head = FALSE, sep = ",")

dataSize = 1000
data.selected = data[c(1:dataSize),]
data.selected.noCluster = data.selected[,-c(1)]

allClustersCount = 20

result = kmeans(data.selected.noCluster, allClustersCount)

a = matrix(nrow = allClustersCount, ncol = dataSize+3)
a[,1] = 1:allClustersCount
a[,2] = rep(3,allClustersCount) # [,3]

for ( i in 1:dataSize) {
  originalNumber = data.selected[i,1]
  cluster = result$cluster[i]
  nextIndexToWrite = a[cluster,2]
  a[cluster,nextIndexToWrite] = originalNumber
  
  a[cluster,2] = a[cluster,2] + 1
}

print(paste("cluster |"," clarity         |", "Most common number"))
      
for ( i in 1:allClustersCount) {
  withoutNA = a[i,-c(a[i,2]:dataSize+3)]
  numbersInCluster = withoutNA[-c(1,2)]
  allNumbersCount = length(numbersInCluster)
  
  xx = as.data.frame(table(numbersInCluster))
  maxOccuranceCount = xx[xx$Freq == max(xx$Freq),2]
  maxOccuranceNumber = as.character(xx[xx$Freq == max(xx$Freq),1])
  
  clarity = maxOccuranceCount / allNumbersCount
  print(paste(c(i,"->      ",clarity,"%    ", maxOccuranceNumber),collapse = ''))
}

