# Daily Kos
#1.1
dailykos <- read.csv('dailykos.csv')
distance <- dist(dailykos, method="euclidean")
summary(distance)
dailykosCluster = hclust(distance, method = "ward.D") 

#1.2
plot(dailykosCluster)

#1.3

#1.4
clusterGroups = cutree(dailykosCluster, k = 7)
splH = split(dailykos, clusterGroups)
str(splH)
lapply(spHl, nrow)

#1.5
tail(sort(colMeans(splH[[1]])))

#1.6
func <- function(x) {
  o <- tail(sort(colMeans(x)))
  return(o)
}
lapply(splH, func)

#2.1
set.seed(1000)
dailykosKMC <- kmeans(dailykos, centers=7)
splK = split(dailykos, dailykosKMC$cluster)
lapply(splK, nrow)

#2.2
lapply(splK, func)

#2.3-6
table(clusterGroups, dailykosKMC$cluster)
tail(sort(colMeans(splK[[2]])))
tail(sort(colMeans(splH[[7]])))

# Market Segmentation for Airlines

#1.1
airlines = read.csv('AirlinesCluster.csv')
summary(airlines)

#1.2

#1.3
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

#2.1
distance = dist(airlinesNorm, method="euclidean")
cluster = hclust(distance, method="ward.D")
plot(cluster)

#2.2
clusterGroups = cutree(cluster, k=5)
str(clusterGroups)
table(clusterGroups)

#2.3-2.7
sort(tapply(airlines$Balance, clusterGroups, mean))
sort(tapply(airlines$QualMiles, clusterGroups, mean))
sort(tapply(airlines$BonusMiles, clusterGroups, mean))
sort(tapply(airlines$BonusTrans, clusterGroups, mean))
sort(tapply(airlines$FlightMiles, clusterGroups, mean))
sort(tapply(airlines$FlightTrans, clusterGroups, mean))
sort(tapply(airlines$DaysSinceEnroll, clusterGroups, mean))

#3.1
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
spl = split(airlinesNorm, KMC$cluster)
lapply(spl, nrow)

#3.2
KMC$centers

# Predicting stock returns
#1.1
stocks = read.csv('StocksCluster.csv')
nrow(stocks)

#1.2
prop.table(table(stocks$PositiveDec))

#1.3
cor(stocks)

#1.4
lapply(stocks, mean)

#2.1
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio=0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec~., data=stocksTrain, family='binomial')
stocksTrainPred = predict(StocksModel, type="response")
table(stocksTrain$PositiveDec, stocksTrainPred > 0.5)
(990 + 3640) / nrow(stocksTrain)

#2.2
stocksPred = predict(StocksModel, newdata=stocksTest, type="response")
table(stocksTest$PositiveDec, stocksPred > 0.5)
(417 + 1553) / nrow(stocksTest)

#2.3
prop.table(table(stocksTest$PositiveDec))

#3.1
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#3.3
# None

#3.4
set.seed(144)
km = kmeans(normTrain, centers=3)
str(km$cluster)
spl = split(normTrain, km$cluster)
lapply(spl, nrow)

#3.5
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
str(clusterTrain)
spl = split(normTest, clusterTest)
lapply(spl, nrow)

#4.1
spl = split(stocksTrain, clusterTrain)
mean(spl[[1]]$PositiveDec)
mean(spl[[2]]$PositiveDec)
mean(spl[[3]]$PositiveDec)

#4.2
StocksModel1 = glm(PositiveDec~., data=spl[[1]], family=binomial)
StocksModel2 = glm(PositiveDec~., data=spl[[2]], family=binomial)
StocksModel3 = glm(PositiveDec~., data=spl[[3]], family=binomial)
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

#4.3
splTest = split(stocksTest, clusterTest)
PredictTest1 = predict(StocksModel1, newdata=splTest[[1]], type="response")
PredictTest2 = predict(StocksModel2, newdata=splTest[[2]], type="response")
PredictTest3 = predict(StocksModel3, newdata=splTest[[3]], type="response")
table(splTest[[1]]$PositiveDec, PredictTest1 > 0.5)
(30 + 774) / nrow(splTest[[1]])
table(splTest[[2]]$PositiveDec, PredictTest2 > 0.5)
(388 + 757) / nrow(splTest[[2]])
table(splTest[[3]]$PositiveDec, PredictTest3 > 0.5)
(49 + 13) / nrow(splTest[[3]])

#4.4
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(splTest[[1]]$PositiveDec, splTest[[2]]$PositiveDec, splTest[[3]]$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467 + 1544) / length(AllOutcomes)
