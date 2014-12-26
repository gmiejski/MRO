require(graphics)

mydata = read.csv("C:\\home\\aaaaStudia\\Semestr_VII\\MRO\\Lab1\\zoo.data", head=TRUE)

chosenData = mydata[3:6]

plotted <- princomp(chosenData)


biplot(plotted)
